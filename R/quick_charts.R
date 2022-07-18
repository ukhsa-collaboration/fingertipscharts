#' Plot compare areas chart
#'
#' Returns ggplot of compare areas chart
#' @return a ggplot of a compare areas chart
#' @param data data.frame or tibble which will be fed into ggplot functions.
#'   This object should contain the fields used for the arguments within this
#'   function
#' @param area unquoted field name for the field containing the area variable
#'   which will be plotted on y axis
#' @param value unquoted field name for the field containing the value variable
#'   which will be plotted on x axis
#' @param lowerci unquoted field name for the field containing the variable to
#'   be plotted as lower confidence interval (optional)
#' @param upperci unquoted field name for the field containing the variable to
#'   be plotted as upper confidence interval (optional)
#' @param fill unquoted field name for the field to be used to determine the
#'   colouring of the bars; usually reflecting significance. The values that
#'   values that can be used in this field with predetermined colours are:
#'   'Better', 'Higher', 'Similar', 'Lower', 'Worse', 'Not compared', 'None'
#' @param order one of "alphabetical", "asc" or "desc" - to determine how to
#'   order the bars
#' @param top_areas character vector; the areas to fix at the top of the chart.
#'   These values must exist within the area field of the data provided
#' @param title string; title of chart
#' @param xlab string; x-axis title
#' @param ylab string; y-axis title
#' @param legend.position string; the position of legend ("none", "left",
#'   "right", "bottom", "top", or two-element numeric vector)
#' @param display.values logical; whether or not to display the rounded values
#'   next to the bars on the chart
#' @param dps number; number of decimal places to be displayed when
#'   display.values = TRUE. The default is 1.
#' @family quick charts
#' @import ggplot2
#' @import dplyr
#' @importFrom rlang quo_text `:=`
#' @examples
#' library(dplyr)
#' df <- create_test_data()
#' parent <- "PAC11"
#' top_names <- c("C001", parent)
#' ordered_levels <- c("Better",
#'                     "Similar",
#'                     "Worse",
#'                     "Not compared")
#' df_ca <- df %>%
#'         filter(IndicatorName == "Indicator 3",
#'                (AreaCode %in% top_names |
#'                         ParentAreaCode == parent))
#' p <- compare_areas(df_ca, AreaCode, Value,
#'                    fill = Significance,
#'                    lowerci = LCI,
#'                    upperci = UCI,
#'                    order = "desc",
#'                    top_areas = top_names,
#'                    title = "Compare the local areas")
#' p
#'
#' @export
compare_areas <- function(data, area, value,
                          lowerci, upperci,
                          fill, order = "desc", top_areas,
                          title = "", xlab = "", ylab = "",
                          legend.position = "bottom",
                          display.values = FALSE,
                          dps = 1) {
    if (order == "desc") {
        if (!missing(top_areas)) {
            levels <- data %>%
                filter(!({{ area }} %in% top_areas)) %>%
                droplevels() %>%
                arrange(-{{ value }}) %>%
                select({{ area }}) %>%
                pull() %>%
                as.character()
            levels <- rev(c(top_areas, levels))
            data <- data %>%
                mutate({{ area }} :=
                           factor({{ area }},
                                  levels = levels))
        } else {
            levels <- data %>%
                droplevels() %>%
                arrange(-{{ value }}) %>%
                select({{ area }}) %>%
                pull() %>%
                as.character() %>%
                rev()
            data <- data %>%
                mutate({{ area }} :=
                           factor({{ area }},
                                  levels = levels))

        }
    } else if (order == "asc") {
        if (!missing(top_areas)) {
            levels <- data %>%
                filter(!({{ area }} %in% top_areas)) %>%
                droplevels() %>%
                arrange({{ value }}) %>%
                select({{ area }}) %>%
                pull() %>%
                as.character() %>%
                unique
            levels <- rev(c(top_areas, levels))
            data <- data %>%
                mutate({{ area }} :=
                           factor({{ area }},
                                  levels = levels))
        } else {
            levels <- data %>%
                droplevels() %>%
                arrange({{ value }}) %>%
                select({{ area }}) %>%
                pull() %>%
                as.character() %>%
                rev()
            data <- data %>%
                mutate({{ area }} :=
                           factor({{ area }},
                                  levels = levels))

        }

    }
    if (display.values) data <- data %>%
            mutate(label = round2({{ value }}, dps),
                   label = formatC(.data$label, format = "f", digits = dps, big.mark = ","))


    compare_areas <- ggplot(data,
                            aes(x = {{ area }},
                                y = {{ value }})) +
        coord_flip() +
        labs(title = title,
             x = ylab,
             y = xlab)

    if (!missing(fill)) {
        compare_areas <- compare_areas +
            geom_col(aes(fill = {{ fill }})) +
            scale_fill_phe(theme = "fingertips") +
            labs(fill = "Area compared to Benchmark")

    } else {
        compare_areas <- compare_areas +
            geom_col()
    }
    if (!missing(lowerci) & !missing(upperci)) {
        compare_areas <- compare_areas +
            geom_errorbar(aes(ymin = {{ lowerci }},
                              ymax = {{ upperci }}),
                          width=.2, show.legend = FALSE)
    }
    if (display.values) {
        if (!missing(upperci)) {
            label_position <- data %>%
                filter({{ upperci }} == max({{ upperci }}, na.rm = TRUE)) %>%
                pull({{ upperci }})
        } else {
            label_position <- data %>%
                filter({{ value }} == max({{ value }}, na.rm = TRUE)) %>%
                pull({{ value }})
        }
        adjust_factor <- 25
        label_position <- label_position / -adjust_factor
        scale_adjust <- 1.05 * (label_position * -adjust_factor) - (label_position * -adjust_factor)
        compare_areas <- compare_areas +
            geom_text(aes(label = .data$label),
                      hjust = 1,
                      y = label_position / 2) +
            scale_y_continuous(limits = c(label_position - scale_adjust,
                                          (label_position * -adjust_factor) + scale_adjust))
    }
    compare_areas <- compare_areas +
        theme_phe("fingertips") +
        theme(legend.position = legend.position,
              panel.grid.major.y = element_blank())
    return(compare_areas)
}

#' Plot an overview (tartan rug) of multiple indicators
#'
#' @return a ggplot of the overview/tartan rug plot
#' @inheritParams compare_areas
#' @param area unquoted field name for the field containing area names
#' @param indicator unquoted field name for the field containing indicator names
#' @param value unquoted field name for the field containing values for the
#'   indicators to be displayed
#' @param timeperiod unquoted field name for the field containing the time
#'   period
#' @param top_areas character vector; controls the areas to be displayed at the
#'   top on the left hand side of the chart. The values in the character vector
#'   must appear in the field represented by the area argument. Optional
#' @param wrap_length number; maximum number of characters in the indicator name
#'   displayed before the text is wrapped to the next line
#' @param value_label_size number; controls the size of the label of the value
#' @param legend_position the position of legends ("none", "left", "right",
#'   "bottom", "top", or two-element numeric vector)
#' @family quick charts
#' @import ggplot2
#' @import dplyr
#' @importFrom rlang quo_text `:=`
#' @importFrom stringr str_wrap
#' @examples
#' library(dplyr)
#' df <- create_test_data()
#'
#' parent <- "PAC14"
#' top_names <- c("C001", parent)
#' df_over <- df %>%
#'         filter((AreaCode %in% top_names |
#'                         ParentAreaCode == parent)) %>%
#'         mutate(Value = round(Value, 1))
#' p <- overview(df_over,
#'               area = AreaCode,
#'               indicator = IndicatorName,
#'               value = Value,
#'               timeperiod = Timeperiod,
#'               fill = Significance,
#'               top_areas = top_names,
#'               wrap_length = 40,
#'               value_label_size = 0.8)
#' p
#' @export
overview <- function(data, area, indicator, value,
                     fill, timeperiod, top_areas, wrap_length = 50,
                     value_label_size = 1, legend_position = "none") {


    if (!missing(top_areas)) {
        levels <- data %>%
            filter(!(({{ area }}) %in% top_areas)) %>%
            droplevels() %>%
            arrange(({{ area }})) %>%
            select({{ area }}) %>%
            pull() %>%
            as.character() %>%
            unique()
        levels <- c("Period", top_areas, levels)
        data <- data %>%
            mutate({{ area }} :=
                       factor(({{ area }}),
                              levels = levels))
    } else {
        levels <- data %>%
            droplevels() %>%
            arrange({{ area }}) %>%
            select({{ area }}) %>%
            pull() %>%
            as.character() %>%
            unique()
        data <- data %>%
            mutate({{ area }} :=
                       factor(({{ area }}),
                              levels = c("Period", levels)))
    }
    tp <- data %>%
        filter(({{ area }}) == levels[2]) %>%
        mutate({{ area }} := "Period",
               {{ fill }} := NA,
               {{ value }} :=
                   as.character(str_wrap(({{ timeperiod }}), 9)))


    data <- rbind(data, tp)
    levels <- data %>%
        droplevels() %>%
        arrange({{ indicator }}) %>%
        select({{ indicator }}) %>%
        pull() %>%
        as.character() %>%
        rev() %>%
        unique() %>%
        str_wrap(wrap_length)
    data <- data %>%
        mutate({{ indicator }} :=
                   factor(str_wrap(({{ indicator }}), wrap_length),
                          levels = levels)) %>%
        droplevels()

    overview <- ggplot(data, aes(x = {{ area }},
                                 y = {{ indicator }})) +
        geom_tile(aes(fill = {{ fill }}),
                  colour = "white") +
        geom_text(aes(label = {{ value }}),
                  size = value_label_size * 4) +
        scale_fill_phe("fingertips",
                       na.translate = FALSE,
                       guide = guide_legend(byrow = TRUE)) +
        scale_x_discrete(position = "top") +
        theme(legend.position = legend_position,
              axis.text.x = element_text(angle = 90,
                                         hjust = 0),
              axis.text.y = element_text(size = rel(1)),
              axis.title = element_blank(),
              line = element_blank(),
              rect = element_blank())
    return(overview)
}

#' Plot compare indicators plot
#'
#' @return a ggplot object of a scatterplot comparing two indicators
#' @inheritParams compare_areas
#' @param x unquoted field name for the field containing x variable
#' @param y unquoted field name for the field containing y variable
#' @param point_size number; size of point
#' @param highlight_area character vector; list of areas for highlighting. These
#'   ares must be in the area field of the data supplied
#' @param area unquoted field name for the field containing areas. This is Only
#'   required if highlight_area has a value (optional)
#' @param add_R2 logical; should R2 be displayed?
#' @family quick charts
#' @import ggplot2
#' @import dplyr
#' @importFrom rlang quo_text new_formula ensym
#' @importFrom stats lm
#' @examples
#' library(tidyr)
#' library(dplyr)
#' df <- create_test_data()
#'
#' df_ci <- df %>%
#'         filter(IndicatorName %in% c("Indicator 1", "Indicator 3")) %>%
#'         select(IndicatorName, AreaCode, Value) %>%
#'         pivot_wider(names_from = IndicatorName,
#'                     values_from = Value) %>%
#'         rename(Ind1 = `Indicator 1`,
#'                Ind3 = `Indicator 3`) %>%
#'         mutate(Ind2 = runif(nrow(.), min = Ind1 * 0.5, max = Ind1 * 1.5))
#' p <- compare_indicators(df_ci,
#'                         x = Ind1,
#'                         y = Ind3,
#'                         xlab = "Indicator 1 label",
#'                         ylab = "Indicator 3 label",
#'                         highlight_area = c("C001", "AC172"),
#'                         area = AreaCode,
#'                         add_R2 = TRUE)
#' p
#' @export
compare_indicators <- function(data, x, y,
                               xlab = "", ylab = "",
                               point_size = 4, highlight_area,
                               area, add_R2 = FALSE) {


    compare_indicators <- ggplot(data, aes(x = {{ x }},
                                           y = {{ y }})) +
        labs(x = xlab,
             y = ylab) +
        theme(rect = element_blank(),
              line = element_blank(),
              panel.grid.major.y = element_line(colour = "#E6E6E6"),
              legend.position = "none")
    if (!missing(highlight_area)) {
        if (missing(area)){
            stop("If highlight_area contains a value, so must area_field")
        }
        data <- data %>%
            mutate(highlight = ifelse({{ area }} %in% highlight_area, T, F))
        compare_indicators <- compare_indicators +
            geom_point(data = data,
                       aes(shape = .data$highlight,
                           fill = .data$highlight),
                       size = point_size) +
            scale_shape_manual(values = c(23, 21),
                               limits = c(T, F)) +
            scale_fill_manual(values = c("black", "#7CB5EC"),
                              limits = c(T, F))
    } else {
        compare_indicators <- compare_indicators +
            geom_point(shape = 21,
                       fill = "#7CB5EC",
                       size = point_size)
    }

    if (add_R2 == TRUE) {
        form <- rlang::new_formula(rlang::ensym(y), rlang::ensym(x))
        r2 <- summary(lm(form, data = data))
        r2frame <- data.frame(val = ifelse(r2$r.squared > 0.15,
                                           paste("R^2:",round2(r2$r.squared, 2)),
                                           "R2 below 0.15;\nNot displayed"),
                              x = -Inf,
                              y = Inf)
        if (r2$r.squared > 0.15) {
            compare_indicators <- compare_indicators +
                geom_text(data = r2frame, aes(x = .data$x,
                                              y = .data$y,
                                              label = .data$val),
                          hjust = 0,
                          vjust = 1,
                          parse = TRUE) +
                geom_abline(intercept = r2$coefficients[1, 1],
                            slope = r2$coefficients[2, 1],
                            colour = "#ED1F52")
        } else {
            compare_indicators <- compare_indicators +
                geom_text(data = r2frame, aes(x = .data$x,
                                              y = .data$y,
                                              label = .data$val),
                          hjust = 0,
                          vjust = 1)
        }
    }
    return(compare_indicators)
}

#' Plot trend chart
#'
#' @return a ggplot of trends for an indicator alongside a comparator
#' @inheritParams compare_areas
#' @inheritParams overview
#' @inheritParams compare_indicators
#' @param area unquoted field name for the field containing the area names
#' @param comparator string; name of comparator area (this value should exist in
#'   the field described by the area parameter)
#' @param area_name string; name of the area to be displayed (this value should
#'   exist in the field described by the area parameter)
#' @param subtitle string; subtitle of the chart
#' @family quick charts
#' @import ggplot2
#' @import dplyr
#' @examples
#' library(dplyr)
#' df <- create_test_data()
#'
#' df_trend <- df %>%
#'         arrange(IndicatorName) %>%
#'         mutate(Timeperiod = rep(c("2011", "2012", "2013", "2014", "2015", "2016"),
#'                                 each = 111))
#' p <- trends(df_trend,
#'             timeperiod = Timeperiod,
#'             value = Value,
#'             area = AreaCode,
#'             comparator = "C001",
#'             area_name = "AC142",
#'             fill = Significance,
#'             lowerci = LCI,
#'             upperci = UCI,
#'             title = "Trend compared to country",
#'             subtitle = "For area AC142",
#'             xlab = "Year",
#'             ylab = "Value (%)")
#' p
#' @export
trends <- function(data, timeperiod, value,
                   area, comparator, area_name, fill,
                   lowerci, upperci,
                   title = "", subtitle = "",
                   xlab = "", ylab = "", point_size = 4) {

    data <- data %>%
        filter({{ area }} %in% c(area_name, comparator))
    line_colours <- c("black", "#7CB5EC")
    names(line_colours) <- c(comparator, area_name)
    trends <- ggplot(data,
                     aes(x = {{ timeperiod }},
                         y = {{ value }},
                         group = {{ area }})) +
        geom_line(aes(linetype = {{ area }},
                      colour = {{ area }})) +
        geom_point(data = filter(data, {{ area }} == comparator),
                   fill = "black",
                   aes(shape = {{ area }}),
                   size = point_size) +
        scale_linetype_manual(name = "",
                              values = rep("solid", 2),
                              labels = comparator) +
        scale_shape_manual(name = "",
                           values = 21,
                           labels = comparator) +
        scale_colour_manual(name = "",
                            values = line_colours) +
        labs(title = title,
             subtitle = subtitle,
             x = xlab,
             y= ylab) +
        theme_phe("fingertips") +
        theme(legend.position = "bottom")
    if (!missing(fill)) {
        trends <- trends +
            geom_point(data = filter(data, {{ area }} == area_name),
                       aes(fill = {{ fill }}),
                       shape = 21,
                       size = point_size, show.legend = F) +
            scale_fill_phe("fingertips")
    } else {
        trends <- trends +
            geom_point(data = filter(data, {{ area }} == area_name),
                       fill = "#C9C9C9",
                       shape = 21,
                       size = point_size, show.legend = F)
    }
    if (!missing(lowerci) & !missing(upperci)) {
        trends <- trends +
            geom_errorbar(data = filter(data, {{ area }} == area_name),
                          aes(ymin = {{ lowerci }},
                              ymax = {{ upperci }}),
                          width=.2)

    }
    trends <- trends +
        guides(shape = "none",
               linetype = "none")
    return(trends)
}

#' Plot population pyramid
#'
#' @return a ggplot of a population pyramid against 2 optional comparators
#' @inheritParams compare_areas
#' @inheritParams compare_indicators
#' @inheritParams trends
#' @param value unquoted field name for the field containing the population
#'   values for each age band
#' @param sex unquoted field name for the field containing sex variable
#' @param age unquoted field name for the field containing age band variable
#' @param area unquoted field name for the field containing the area names
#' @param area_name string; name of the area to display the bars for (this
#'   should exist in the field described by the area parameter)
#' @param comparator_1 string; name of comparator area (this should exist in the
#'   field described by the area parameter)
#' @param comparator_2 string; name of comparator area (this should exist in the
#'   field described by the area parameter)
#' @family quick charts
#' @import ggplot2
#' @import dplyr
#' @importFrom rlang quo_text `:=`
#' @importFrom scales breaks_pretty
#' @examples
#' library(dplyr)
#' agelevels <- c("0-4", "5-9","10-14","15-19",
#'                "20-24","25-29","30-34",
#'                "35-39","40-44","45-49",
#'                "50-54","55-59","60-64",
#'                "65-69","70-74","75-79",
#'                "80-84","85-89","90+")
#' areas <- c("Area 1", "Area 2", "Area 3")
#' pops <- data.frame(Age = factor(rep(agelevels, length(areas) * 2),
#'                                 levels = agelevels),
#'                    Value = rep(sample(1000:3000, length(agelevels), replace = TRUE),
#'                                length(areas) * 2),
#'                    Sex = rep(rep(c("Male", "Female"),
#'                                  each = length(agelevels)), length(areas)),
#'                    AreaName = rep(areas, each = length(agelevels) * 2))
#'
#' p <- population(pops,
#'                 value = Value,
#'                 sex = Sex,
#'                 age = Age,
#'                 area = AreaName,
#'                 area_name = "Area 1",
#'                 comparator_1 = "Area 3",
#'                 comparator_2 = "Area 2",
#'                 title = "Age Profile",
#'                 subtitle = "2015/16",
#'                 xlab = "% of total population")
#' p
#' @export
population <- function(data, value, sex, age,
                       area, area_name, comparator_1, comparator_2,
                       title, subtitle, xlab) {

    if(!missing(area_name) &
       !missing(comparator_1) &
       !missing(comparator_2)) {
        areas <- c(area_name, comparator_1, comparator_2)
    } else if (!missing(area_name) &
               !missing(comparator_1) &
               missing(comparator_2)) {
        areas <- c(area_name, comparator_1)
    } else if (!missing(area_name) &
               missing(comparator_1) &
               missing(comparator_2)) {
        areas <- area_name
    } else {
        stop("area_name must be complete for a population pyramid to be drawn")
    }



    data <- data %>%
        filter({{ area }} %in% areas) %>%
        group_by({{ area }}) %>%
        mutate({{ value }} :=
                   100 * ({{ value }}) / sum({{ value }}),
               {{ value }} :=
                   ifelse({{ sex }} == "Male",
                          -({{ value }}), ({{ value }})))
    extremex <- breaks_pretty(n = 3)(0:max(abs(pull(data, {{ value }})),
                                           na.rm = T))
    population <- ggplot(filter(data, {{ area }} == area_name),
                         aes(y = {{ value }},
                             x = {{ age }},
                             fill = {{ sex }})) +
        geom_col(col = "black", width = 0.7) +
        coord_flip() +
        scale_y_continuous(breaks = c(rev(-extremex), extremex[2:length(extremex)]),
                           labels = abs(c(rev(extremex), extremex[2:length(extremex)]))) +
        scale_fill_manual(name = "",
                          values = c("Male" = "#5555E6",
                                     "Female" = "#C2CCFF"),
                          breaks = c("Male", "Female"),
                          labels = c(paste(area_name, "(Male)"),
                                     paste(area_name, "(Female)"))) +
        labs(title = title,
             subtitle = subtitle,
             y = xlab) +
        theme(legend.position = "bottom",
              legend.key = element_blank(),
              axis.title.y = element_blank(),
              line = element_blank(),
              rect = element_blank(),
              panel.grid.major.x = element_line(colour = "gray80"))
    if (!missing(comparator_1)) {
        compdata1 <- filter(data, {{ area }} == comparator_1)
        population <- population +
            geom_line(data = compdata1,
                      aes(y = {{ value }},
                          x = {{ age }},
                          group = interaction(pull(compdata1, {{ sex }}),
                                              pull(compdata1, {{ area }})),
                          col = {{ area }}),
                      size = 1.5)
        if (!missing(comparator_2)) {
            compdata2 <- filter(data, {{ area }} == comparator_2)
            population <- population +
                geom_line(data = compdata2,
                          aes(y = {{ value }},
                              x = {{ age }},
                              group = interaction(pull(compdata2, {{ sex }}),
                                                  pull(compdata2, {{ area }})),
                              col = {{ area }}),
                          size = 1.5) +
                scale_colour_manual(name = "",
                                    breaks = c(comparator_1, comparator_2),
                                    limits = c(comparator_1, comparator_2),
                                    values = c("black","#E563F9"))

        } else {
            population <- population +
                scale_colour_manual(name = "",
                                    breaks = c(comparator_1),
                                    limits = c(comparator_1),
                                    values = c("black"))
        }
    }
    return(population)

}

#' Plot a series of boxplots
#'
#' @return a ggplot of boxplots for an indicator for containing values for
#'   multiple areas over time
#' @param value unquoted field name for the field containing the values for the
#'   indicator that is being plotted
#' @inheritParams compare_indicators
#' @inheritParams compare_areas
#' @inheritParams overview
#' @inheritParams trends
#' @family quick charts
#' @import ggplot2
#' @import dplyr
#' @importFrom rlang quo_text
#' @importFrom stats median quantile
#' @examples
#' library(dplyr)
#' df <- create_test_data()
#'
#' df_box <- df %>%
#'         filter(AreaType == "Local") %>%
#'         arrange(IndicatorName) %>%
#'         mutate(Timeperiod = rep(c("2011", "2012", "2013", "2014", "2015", "2016"),
#'                                 each = 100))
#' p <- box_plots(df_box,
#'                timeperiod = Timeperiod,
#'                value = Value,
#'                title = "Title of chart",
#'                subtitle = "Boxplot over time",
#'                ylab = "Proportion (%)")
#' @export
box_plots <- function(data, timeperiod, value,
                      title = "", subtitle = "",
                      xlab = "", ylab = "") {
    data <- data %>%
        group_by({{ timeperiod }}) %>%
        summarise(y5 = quantile({{ value }}, 0.05, na.rm = TRUE),
                  y25 = quantile({{ value }}, 0.25, na.rm = TRUE),
                  y50 = median({{ value }}, na.rm = TRUE),
                  y75 = quantile({{ value }}, 0.75, na.rm = TRUE),
                  y95 = quantile({{ value }}, 0.95, na.rm = TRUE))
    boxplots <- ggplot(data, aes(x = {{ timeperiod }})) +
        geom_boxplot(aes(ymin = .data$y5, lower = .data$y25,
                         middle = .data$y50, upper = .data$y75,
                         ymax = .data$y95),
                     fill = "#CCCCCC",
                     stat = "identity") +
        labs(title = title,
             subtitle = subtitle,
             x = xlab,
             y = ylab) +
        theme_phe("fingertips")
    dat <- ggplot_build(boxplots)$data[[1]]

    boxplots <- boxplots +
        geom_segment(data = dat,
                     aes(x = .data$xmin + ((.data$xmax - .data$xmin) * 0.25),
                         xend = .data$xmin + ((.data$xmax - .data$xmin) * 0.75),
                         y = .data$ymax,
                         yend = .data$ymax)) +
        geom_segment(data = dat,
                     aes(x = .data$xmin + ((.data$xmax - .data$xmin) * 0.25),
                         xend = .data$xmin + ((.data$xmax - .data$xmin) * 0.75),
                         y = .data$ymin,
                         yend = .data$ymin)) +
        geom_segment(data = dat,
                     aes(x = .data$xmin,
                         xend = .data$xmax,
                         y = .data$middle,
                         yend = .data$middle),
                     colour="red", size = 1)
    return(boxplots)

}

#' Plot a choropleth map for an indicator
#'
#' @return a either a static or interactive ggplot choropleth map
#' @inheritParams compare_areas
#' @inheritParams trends
#' @param area_code field containing area codes to join to shape file imported
#'   from ONS API
#' @param type string; the output map required. Can be "static" or "interactive"
#' @param ons_api string; GeoJSON url of a shape file. This can be found on the
#'   ONS geography portal
#' @param copyright_size number; used to control the size of the copyright text
#' @param copyright_year number (length 4 characters) or Date class; the
#'   copyright year displayed at bottom of the map. Applies to static maps only
#' @param name_for_label if interactive map, unquoted field name for the field
#'   containing area names to be used for label of polygons - optional
#' @param value if interactive map, unquoted field name for the field containing
#'   values to be used for label of polygons when hovering
#' @family quick charts
#' @import ggplot2
#' @import dplyr
#' @importFrom rlang quo_text quo sym as_name enquo
#' @importFrom leaflet colorFactor leaflet addTiles addPolygons addLegend
#' @importFrom stats setNames
#' @importFrom sf st_as_sf
#' @examples
#' \dontrun{
#' ons_api <- "https://opendata.arcgis.com/datasets/687f346f5023410ba86615655ff33ca9_4.geojson"
#'
#' p <- map(mapdata,
#'          ons_api = ons_api,
#'          area_code = AreaCode,
#'          fill = Significance,
#'          title = "Map example",
#'          subtitle = "An indicator for Upper Tier Local Authorities England",
#'          copyright_year = 2019)
#'
#' p
#'
#' ## For an interactive (leaflet) map
#' p <- map(mapdata,
#'          ons_api = ons_api,
#'          area_code = AreaCode,
#'          fill = Significance,
#'          type = "interactive",
#'          value = Value,
#'          name_for_label = AreaName,
#'          title = "An indicator for Upper Tier<br>Local Authorities England")
#' p}
#' @export
map <- function(data, ons_api, area_code, fill, type = "static", value, name_for_label,
                title = "", subtitle = "", copyright_size = 4, copyright_year = Sys.Date()) {
    if (missing(ons_api)) stop("ons_api must contain a string to a geojson url on the ONS geography portal")
    if (ensure_ons_api_available(ons_api)) {
        shp <- sf::read_sf(ons_api)
        all_area_codes <- data %>%
            pull({{ area_code }}) %>%
            unique()
        join_field <- vapply(shp, function(x) sum(x %in% all_area_codes),
                             numeric(1))
        join_field <- names(join_field[join_field == max(join_field)])
        if (length(join_field) != 1) stop("There is no clear field in the shape file that contains the area codes in the field you have identified")
        shp <- shp %>%
            filter(grepl("^E", !! quo(!! sym(join_field))))
        if (type == "static") {
            data <- data %>%
                mutate({{ area_code }} :=
                           as.character({{ area_code }}))
            shp <- shp %>%
                mutate(AreaCode = as.character(!! quo(!! sym(join_field)))) %>%
                left_join(data, setNames("AreaCode", rlang::as_name(rlang::enquo(area_code))))
            if (is.numeric(copyright_year) & nchar(copyright_year) == 4) {
                copyright_year <- as.character(copyright_year)
            } else if (inherits(copyright_year, 'Date')) {
                copyright_year <- format(copyright_year, "%Y")
            } else {
                stop("copyright_year must be either a 4 digit numeric class or Date class")
            }
            copyright <- data.frame(val = paste0("Contains Ordnance Survey data\n",
                                                 paste0("\uA9 Crown copyright and database right ",
                                                        copyright_year),
                                                 "\n",
                                                 "Contains National Statistics data\n",
                                                 paste0("\uA9 Crown copyright and database right ",
                                                        copyright_year)),
                                    x = max(shp$long),
                                    y = min(shp$lat))
            map <- ggplot(shp) +
                geom_sf(aes(fill = {{ fill }})) +
                coord_sf(datum = NA) +
                scale_fill_phe(theme = "fingertips") +
                theme_void() +
                geom_text(data = copyright,
                          aes(x = .data$x,
                              y = .data$y,
                              label = .data$val),
                          colour = "black",
                          hjust = 1,
                          vjust = 0,
                          size = copyright_size) +
                labs(title = title,
                     subtitle = subtitle,
                     fill = "")

        } else if (type == "interactive") { # nocov start
            ftipspal <- scale_fill_phe("fingertips")
            ftipspal <- ftipspal$palette(1)

            if (is.factor(pull(data, {{ fill }}))) {
                factpal <- colorFactor(ftipspal[levels(pull(data, {{ fill }}))],
                                       domain = pull(data, {{ fill }}),
                                       ordered = TRUE)
            } else {
                factpal <- colorFactor(ftipspal[unique(pull(data, {{ fill }}))],
                                       domain = pull(data, {{ fill }}),
                                       ordered = TRUE)
            }


            data <- data %>%
                mutate({{ area_code }} :=
                           as.character({{ area_code }}))
            shp <- shp %>%
                mutate(AreaCode = as.character(!! quo(!! sym(join_field)))) %>%
                left_join(data, setNames("AreaCode", rlang::as_name(rlang::enquo(area_code))))
            if (!missing(name_for_label)) {
                labels <- sprintf("<strong>%s</strong><br/>Value: %g",
                                  pull(shp, {{ name_for_label }}),
                                  pull(shp, {{ value }}))
            } else {
                labels <- sprintf("<strong>%s</strong><br/>Value: %g",
                                  pull(shp, {{ join_field }}),
                                  pull(shp, {{ value }}))
            }

            map <- leaflet(shp)  %>%
                addTiles() %>%
                addPolygons(fillColor =
                                ~ factpal(pull(shp, {{ fill }})),
                            weight = 2,
                            opacity = 1,
                            color = "white",
                            dashArray = "3",
                            fillOpacity = 0.7,
                            popup = labels) %>%
                addLegend("topright",
                          pal = factpal,
                          values = ~ pull(shp, {{ fill }}),
                          title = title,
                          opacity = 1)
        } # nocov end
        return(map)
    }


}

#' Plot spine chart
#'
#' Returns ggplot of spine chart
#' @return a ggplot object containing a spine chart
#' @details The function draws a bar chart (which is the spine) and then plots
#'   the data table (if datatable = TRUE) using geom_text. The bar chart is
#'   always plotted between 0 and 1 on the x scale, which isn't visible in the
#'   output. The column locations in the data table are controlled by the
#'   header_positions argument. To adjust the length of the bars in the
#'   visualisation, amend the header_positions argument. The more negative the
#'   first value of the vector that goes into header_positions, the more
#'   condensed the bar part of the visualisation will be.
#' @param data a data frame to create the spine chart from. The data frame
#'   should contain records for all area types included in the chart (eg, if
#'   plotting for County & UA with a comparator of region and a median line for
#'   national, the data frame should contain records for all of these data). The
#'   minimum field requirements in the data frame are; value, count, area_code,
#'   indicator, timeperiod, polarity, significance, area_type. See below for the
#'   definitions of these fields
#' @param value unquoted field name for the field containing the values to be
#'   plotted
#' @param count unquoted field name for the field where the count (numerator) is
#'   stored. This is provided to the accompanying data table
#' @param area_code unquoted field name for the field where area codes are
#'   stored (local_area_code, median_line_area_code and comparator_area_code, if
#'   using, should all exist in this field)
#' @param local_area_code string; the code of the area whose data is being
#'   presented
#' @param indicator unquoted field name for the field of the field containing
#'   the indicator labels. Take care as errors will occur where indicator labels
#'   are the same but data exist for multiple sub-categories (for example, sex
#'   or age)
#' @param timeperiod unquoted field name for the field of the time period field.
#'   This gets used in the accompanying data table
#' @param trend unquoted field name for the field of the trend field; if the
#'   user doesn't want to display trend information then leave this incomplete
#'   and amend the header_labels argument by replacing the "Trend" header with
#'   "". Text within this field should contain one of the following words to
#'   control the arrows that are displayed; "decreasing", "increasing", "no
#'   significant change", "could not be calculated". The text within this field
#'   should contain one of the following words to control the colour; "better",
#'   "worse", "no significant change". If none of these words appear in the
#'   string, the words "increasing" or "decreasing" will be used to colour the
#'   arrows in different shades of blue
#' @param polarity unquoted field name for the field containing the polarity
#'   information (currently only handles "Not applicable", "RAG - Low is good",
#'   "RAG - High is good", "BOB - Blue orange blue")
#' @param significance unquoted field name for the field describing the
#'   statistical significance for that indicator. This determines the colour of
#'   the point within the spine for the area. Colours are built in for the
#'   following significances; 'Better', 'Same', 'Worse', 'Not compared', 'None',
#'   'Higher', 'Similar', 'Lower'. Use the cols argument to colour other
#'   significance values
#' @param area_type unquoted field name for the field containing area type
#'   information. This ensures the vertabra are only plotted for the same area
#'   types as the local_area area type (eg, when plotting a spine chart for
#'   County & UA areas, regions and national area types will be removed)
#' @param cols named character vector for the cols that will be applied to the
#'   significance field. The names should contain all of the levels in the
#'   significance field of the data frame. Defaults to the Fingertips colours
#' @param median_line_area_code string; area code for the median line. Defaults
#'   to "E92000001" (England)
#' @param comparator_area_code string; area code for the comparator point.
#'   Defaults to NA
#' @param bar_width numeric value; the distance between bars (0 to 1)
#' @param local_point_shape numeric value; shape type for local area point
#'   (defaults to 21, circle). See ggplot2 shape types for different values
#' @param local_point_outline string; control colour of the outline of the local
#'   point in the spine chart
#' @param comparator_point_shape numeric value; shape type for regional area
#'   point (defaults to 23, diamond). See ggplot2 shape types for different
#'   values
#' @param comparator_point_outline string; control colour of the outline of the
#'   regional point in the spine chart
#' @param comparator_point_fill string; control the fill colour of the regional
#'   point in the spine chart
#' @param relative_text_size numeric value; control the size of the text in the
#'   accompanying table
#' @param relative_point_size numeric value; control the size of the points on
#'   the spine chart
#' @param header_positions numeric vector; used to adjust columns of data table
#'   if they are overlapping. The final value shouldn't be less than 1. Must
#'   have a length of 7, if not using the trend argument, or 8 otherwise.
#'   Defaults to c(-1.83, -1.13, -.53, -.35, -.25, -.15, -0.05, 1.05)
#' @param header_labels character vector; labels used for the titles of the
#'   columns for a data table. Must have a length of 7, if not using the trend
#'   argument, or 8 otherwise. Defaults to c("Indicator", "Trend", "Time
#'   period", "Local count","Local value", "England value",
#'   "Worst/Lowest","Best/Highest")
#' @param domain unquoted field name for the field describing the grouping of
#'   the domains if wishing to split the spine chart into domains
#' @param relative_domain_text_size numeric; control the text size for the
#'   domain labels (if include.domains = TRUE) relative to 1
#' @param datatable logical; default = TRUE, display data table alongside spine
#'   chart
#' @param indicator_label_nudgex number; nudge the placement of the indicator
#'   label in the x direction. Negative values nudge to the left
#' @param show_dividers string; whether to display horizontal lines between
#'   indicators. Values can be "all" or "outer". Any other value will not
#'   generate lines
#' @param dps number; number of decimal places to be displayed in the data
#'   table. The default is 1. Set to NA if this should be the same as the input
#'   data
#' @param datatable_line_height number; height of wrapped lines in the data
#'   table
#' @param percent_display number between 0 and 1; the percentage of values that
#'   needs to exist for a spine to display. Default is 0.25
#' @param arrow_length number to control the length of the trend arrow
#' @param arrow_thickness number to control the thickness of the trend arrow
#' @param arrow_head_length number to control the length of the arrow head
#' @param arrow_head_angle number to control the angle of the arrow head
#' @param horizontal_arrow_multiplier number to scale horizontal trend arrows. A
#'   value below 1 will shorten the arrows
#' @details This function filters for the area type that is the same as your
#'   local area type and then calculates the "vertebra" from those data.
#'   Therefore, if you are comparing outputs with those seen on the Fingertips
#'   website, ensure you perform the same preprocessing. For example, some
#'   profiles display spine charts where small areas, such as Isles of Scilly,
#'   are removed before the spine is produced.
#' @import ggplot2
#' @import dplyr
#' @importFrom grDevices rgb
#' @importFrom rlang quo_text .data `:=`
#' @importFrom utils tail
#' @importFrom stats reformulate
#' @importFrom stringr str_trim
#' @examples
#' ## An example with differing decimal places for individual indicators and no trend
#'
#' library(dplyr)
#' df <- create_test_data() %>%
#' mutate(Value = case_when(
#'         grepl("2$|4$|6$", IndicatorName) ~ round(Value,1),
#'         TRUE ~ round(Value, 0)))
#' full_p <- area_profiles(df,
#'                         value = Value,
#'                         count = Count,
#'                         area_code = AreaCode,
#'                         local_area_code = "AC122",
#'                         indicator = IndicatorName,
#'                         timeperiod = Timeperiod,
#'                         polarity = Polarity,
#'                         significance = Significance,
#'                         area_type = AreaType,
#'                         median_line_area_code = "C001",
#'                         comparator_area_code = "PAC12",
#'                         datatable = TRUE,
#'                         relative_domain_text_size = 0.75,
#'                         relative_text_size = 1.2,
#'                         bar_width = 0.68,
#'                         indicator_label_nudgex = -0.1,
#'                         show_dividers = "outer",
#'                         header_positions = c(-1, -0.7, -0.44, -0.35, -0.25,
#'                                              -0.15, -0.05, 1.08),
#'                         header_labels = c("Indicator", "",
#'                                           "Time\nperiod",
#'                                           "Local\ncount","Local\nvalue",
#'                                           "England\nvalue",
#'                                           "Worst/\nLowest","Best/\nHighest"),
#'                         dps = NA)
#' full_p
#'
#' ## An example with domains and non-default indicator ordering
#'
#' df <- create_test_data()
#' label_order <- c(1, 2, 4, 3, 6, 5)
#' df <- df %>%
#'         mutate(IndicatorName = factor(IndicatorName,
#'                                       levels = paste("Indicator", label_order)))
#'
#' p <- area_profiles(df,
#'                    value = Value,
#'                    count = Count,
#'                    area_code = AreaCode,
#'                    local_area_code = "AC122",
#'                    indicator = IndicatorName,
#'                    timeperiod = Timeperiod,
#'                    trend = Trend,
#'                    polarity = Polarity,
#'                    significance = Significance,
#'                    area_type = AreaType,
#'                    median_line_area_code = "C001",
#'                    comparator_area_code = "PAC12",
#'                    datatable = TRUE,
#'                    relative_domain_text_size = 0.75,
#'                    relative_text_size = 1.2,
#'                    bar_width = 0.68,
#'                    indicator_label_nudgex = -0.1,
#'                    show_dividers = "outer",
#'                    header_positions = c(-1, -0.7, -0.53, -0.35, -0.25,
#'                                         -0.15, -0.05, 1.05),
#'                    domain = Domain
#' )
#' p
#'
#' @export
area_profiles <- function(data,
                          value,
                          count,
                          area_code,
                          local_area_code,
                          indicator,
                          timeperiod,
                          trend = NA,
                          polarity,
                          significance,
                          area_type,
                          cols = "fingertips",
                          median_line_area_code = "E92000001",
                          comparator_area_code = NA,
                          bar_width = 0.75,
                          local_point_shape = 21,
                          local_point_outline = "black",
                          comparator_point_shape = 23,
                          comparator_point_outline = "gray30",
                          comparator_point_fill = "gray30",
                          relative_point_size = 1,
                          relative_text_size = 1,
                          header_positions  = c(-1.83, -1.13, -.53, -.35, -.25, -.15, -0.05, 1.05),
                          header_labels = c("Indicator", "Trend",
                                            "Time\nperiod",
                                            "Local\ncount","Local\nvalue",
                                            "England\nvalue",
                                            "Worst/\nLowest","Best/\nHighest"),
                          indicator_label_nudgex = -0.075,
                          domain = no_domains,
                          relative_domain_text_size = 1,
                          show_dividers = "none",
                          datatable = TRUE,
                          datatable_line_height = 0.6,
                          dps = 1,
                          percent_display = 0.25,
                          arrow_length = 1,
                          arrow_thickness = 2,
                          arrow_head_length = arrow_length / 3,
                          arrow_head_angle = 25,
                          horizontal_arrow_multiplier = 1) {


    if (sum(median_line_area_code %in% pull(data, {{ area_code }})) < 1)
        stop(paste0(median_line_area_code, " not in area_code field provided"))
    if (sum(local_area_code %in% pull(data, {{ area_code }})) < 1)
        stop(paste0(local_area_code, " not in area_code field provided"))
    if (!is.na(comparator_area_code) &
        sum(comparator_area_code %in% pull(data, {{ area_code }})) < 1)
        stop(paste0(comparator_area_code, " not in area_code field provided"))
    if (length(header_labels) != 8)
        stop("header_labels argument must have a length of 8")
    if (length(header_positions) != 8)
        stop("header_positions argument must have a length of 8")


    # check for multiple values for an area per indicator
    check_message <- spine_data_check(data, {{ indicator }}, {{ area_code }})
    if (!is.na(check_message)) stop(check_message)

    # create data table
    if (is.factor(pull(data, {{ indicator }}))) {
        ind_order <- levels(pull(data, {{ indicator }}))
    } else {
        data <- data %>%
            mutate({{ indicator }} :=
                       factor({{ indicator }}))
        ind_order <- levels(pull(data, {{ indicator }}))
    }

    if (datatable == TRUE) {
        if (!is.na(dps) & !is.integer(dps) & !is.numeric(dps)) stop("The dps argument must be a number or NA")
        dftable <- create_datatable(data,
                                    {{ indicator }},
                                    {{ area_code }},
                                    {{ timeperiod }},
                                    {{ trend }},
                                    {{ count }},
                                    {{ value }},
                                    local_area_code,
                                    median_line_area_code,
                                    comparator_area_code,
                                    dps = dps,
                                    header_width = diff(range(header_positions)),
                                    horizontal_arrow_multiplier = horizontal_arrow_multiplier)
        dftable <- dftable %>%
            mutate({{ indicator }} :=
                       factor({{ indicator }},
                              levels = ind_order))
    } else {
        dftable <- NA
    }

    # rescale data for charting
    dfrescaled <- spine_rescaler(data,
                                 {{ area_code }},
                                 {{ indicator }},
                                 {{ significance }},
                                 {{ polarity }},
                                 {{ area_type }},
                                 {{ value }},
                                 {{ timeperiod }},
                                 local_area_code,
                                 median_line_area_code,
                                 comparator_area_code,
                                 percent_display,
                                 dps = dps)
    if (rlang::as_name(rlang::enquo(domain)) != "no_domains") {
        domain_lu <- data %>%
            select({{ indicator }}, {{ domain }}) %>%
            mutate({{ domain }} := factor({{ domain }})) %>%
            unique
        dfrescaled$bars <- dfrescaled$bars %>%
            merge(domain_lu,
                  by = rlang::as_name(rlang::enquo(indicator)),
                  all.x =TRUE)
        dfrescaled$points <- dfrescaled$points %>%
            merge(domain_lu,
                  by = rlang::as_name(rlang::enquo(indicator)),
                  all.x =TRUE)
        if (is.data.frame(dftable))
            dftable <- dftable %>%
            merge(domain_lu,
                  by = rlang::as_name(rlang::enquo(indicator)),
                  all.x =TRUE)
    }

    fingertips_cols <- c('Better' = '#92D050', 'Same' = '#FFC000',
                         'Worse' = '#C00000', 'Not compared' = '#C9C9C9',
                         'None' = '#A6A6A6', 'Higher' = '#BED2FF',
                         'Similar' = '#FFC000', 'Lower'='#5555E6',
                         'Worst' = '#FFFFFF','Q25' = '#C9C9C9',
                         'Q75' = '#8B8B8B','Best' = '#C9C9C9')
    if (length(cols) == 1) {
        if (cols == "fingertips") {
            cols <- fingertips_cols
        }
    }

    missing_cols <- setdiff(names(fingertips_cols),
                            names(cols))
    if (length(missing_cols) > 0) cols <- c(cols, fingertips_cols[missing_cols])

    vline_length <- dfrescaled$bars %>%
        pull({{ indicator }}) %>%
        unique %>%
        length

    dfrescaled$bars <- dfrescaled$bars %>%
        mutate(y = case_when(
            y == 1.05 ~ header_positions[length(header_positions)],
            y == -0.05 ~ header_positions[length(header_positions) - 1]
        ),
        {{ indicator }} :=
            factor({{ indicator }},
                   levels = ind_order))
    dfrescaled$points <- dfrescaled$points  %>%
        mutate({{ indicator }} :=
                   factor({{ indicator }},
                          levels = ind_order))
    p <- ggplot(dfrescaled$bars,
                aes(x = {{ indicator }},
                    y = .data$quantiles)) +
        geom_col(aes(fill = .data$GraphPoint),
                 width = bar_width,
                 na.rm = TRUE)


    if (!is.na(comparator_area_code)) {
        rescaled_comparator_field <- "region"
        p <- p +
            geom_point(data = dfrescaled$points,
                       aes(x = {{ indicator }},
                           y = !! sym(rescaled_comparator_field)),
                       shape = comparator_point_shape,
                       colour = comparator_point_outline,
                       fill = comparator_point_fill,
                       size = 2.5 * relative_point_size,
                       na.rm = TRUE)
    }
    p <- p +
        geom_point(data = dfrescaled$points,
                   aes(x = {{ indicator }},
                       y = .data$area,
                       fill = {{ significance }}),
                   shape = local_point_shape,
                   colour = local_point_outline,
                   size = 2.5 * relative_point_size,
                   na.rm = TRUE) +
        geom_hline(yintercept = 0.5, col = "darkred") +
        coord_flip() +
        scale_fill_manual(values = cols) +
        scale_colour_manual(values = cols) +
        theme_minimal() +
        theme(panel.grid.major = element_blank(),
              axis.text = element_text(colour = "black")) +
        labs(x = "", y = "")

    if (is.data.frame(dftable)) {
        dt_area_field <- "Area_value"
        dt_comparator_field <- "Comparator_value"
        dt_median_field <- "Median_value"
        dftable <- dftable %>%
            mutate(ind = {{ indicator }},
                   count = {{ count }},
                   tp = {{ timeperiod }})
        lims <- range(header_positions)
        lims[1] <- lims[1] + indicator_label_nudgex
        lims <- lims * 1.06
        p <- p +
            scale_y_continuous(position = "right",
                               breaks = header_positions,
                               limits = lims,
                               labels = header_labels,
                               expand = c(0, 0)) +
            geom_text(data = dfrescaled$bars[!dfrescaled$bars$GraphPoint %in%
                                                 c("Q75", "Q25"), ],
                      aes(label = .data$label, y = .data$y),
                      col = "black",
                      size = 2.5 * relative_text_size,
                      lineheight = datatable_line_height,
                      hjust = 1,
                      na.rm = TRUE) +
            geom_text(data = dftable,
                      aes(label = !! sym(dt_median_field),
                          x = .data$ind),
                      y = header_positions[6],
                      col = "black",
                      size = 2.5 * relative_text_size,
                      parse = TRUE,
                      lineheight = datatable_line_height,
                      hjust = 1,
                      na.rm = TRUE) +
            geom_text(data = dftable,
                      aes(label = !! sym(dt_area_field),
                          x = .data$ind),
                      y = header_positions[5],
                      col = "black",
                      size = 2.5 * relative_text_size,
                      parse = TRUE,
                      lineheight = datatable_line_height,
                      hjust = 1,
                      na.rm = TRUE) +
            geom_text(data = dftable,
                      aes(label = count, x = .data$ind),
                      y = header_positions[4],
                      col = "black",
                      size = 2.5 * relative_text_size,
                      parse = TRUE,
                      lineheight = datatable_line_height,
                      hjust = 1,
                      na.rm = TRUE) +
            geom_text(data = dftable,
                      aes(label = .data$tp, x = .data$ind),
                      y = header_positions[3],
                      col = "black",
                      size = 2.5 * relative_text_size,
                      lineheight = datatable_line_height,
                      hjust = 1,
                      na.rm = TRUE) +
            geom_spoke(data = dftable,
                       aes(x = .data$ind,
                           y = header_positions[2],
                           angle = .data$direction,
                           colour = .data$trend_sig,
                           radius = .data$radius * arrow_length),
                       size = arrow_thickness,
                       arrow = arrow(length = unit(arrow_head_length, "cm"),
                                     type = "open",
                                     angle = arrow_head_angle),
                       na.rm = TRUE) +
            geom_spoke(data = dftable,
                       aes(x = .data$ind,
                           y = header_positions[2],
                           angle = .data$direction + pi,
                           colour = .data$trend_sig,
                           radius = .data$radius * arrow_length),
                       size = arrow_thickness,
                       na.rm = TRUE) +
            geom_text(data = dftable,
                      aes(label = .data$ind,
                          y = header_positions[1],
                          x = .data$ind),
                      hjust = 0,
                      nudge_y = indicator_label_nudgex,
                      col = "black",
                      size = 2.5 * relative_text_size,
                      lineheight = datatable_line_height,
                      na.rm = TRUE) +
            theme(axis.text.x = element_text(size = 0.9 * rel(relative_text_size)),
                  axis.text.y = element_blank(),
                  panel.grid.minor = element_blank(),
                  legend.position = "none")
    } else {
        p <- p +
            theme(axis.text.x = element_blank(),
                  panel.grid.minor = element_blank(),
                  legend.position = "none")
    }
    if (rlang::as_name(rlang::enquo(domain)) != "no_domains") {
        p <- p +
            facet_grid(rows = vars({{ domain }}),
                       space = "free_y",
                       scales = "free_y",
                       switch = "y") +
            theme(panel.spacing = unit(0, "lines"),
                  strip.placement = "outside",
                  strip.background = element_blank(),
                  strip.text = element_text(size = rel(relative_domain_text_size))
            )

    }
    if (show_dividers == "all") {
        p <- p +
            geom_vline(xintercept = seq(-0.5, vline_length + 0.5),
                       colour = "black",
                       size = 0.2)
    } else if (show_dividers == "outer") {
        p <- p +
            annotate("segment", x = -Inf, xend = -Inf, y = -Inf, yend = Inf) +
            theme(axis.line.x = element_line())
    }
    if (header_positions[length(header_positions)] < 1) {
        warning("Some bars may not display if the final value of the header_positions argument is less than 1")
    }
    return(p)

}
