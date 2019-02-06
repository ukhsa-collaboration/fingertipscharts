#' Plot compare areas chart
#'
#' Returns ggplot of compare areas chart
#' @return a ggplot of a compare areas chart
#' @param data data.frame object to plot using ggplot2 functions
#' @param area field containing variable to be plotted on y axis (unquoted)
#' @param value field containing variable to be plotted on x axis (unquoted)
#' @param lowerci field containing variable to be plotted as lower confidence
#'   interval (unquoted - not required)
#' @param upperci string; field containing variable to be plotted as upper confidence
#'   interval (unquoted - not required)
#' @param fill field to be used to determine the colouring of the bars (unquoted)
#' @param order one of "alphabetical", "asc" or "desc" - to determine how to
#'   order the bars
#' @param top_areas character vector; the areas to fix at the top of the chart
#' @param title string; title of chart
#' @param xlab string; x-axis title
#' @param ylab string; y-axis title
#' @param legend.position the position of legends ("none", "left", "right",
#'   "bottom", "top", or two-element numeric vector)
#' @family quick charts
#' @import ggplot2
#' @import dplyr
#' @importFrom rlang quo_text
#' @examples
#' library(fingertipsR)
#' library(dplyr)
#' region <- "South East region"
#' top_names <- c("England", region)
#' ordered_levels <- c("Better",
#'                     "Similar",
#'                     "Worse",
#'                     "Not compared")
#' df <- fingertips_data(90316) %>%
#'         filter(is.na(CategoryType) &
#'                        Timeperiod == "2016/17" &
#'                        (AreaName %in% top_names |
#'                                 ParentName == region) &
#'                        Sex == "Persons") %>%
#'         mutate(ComparedtoEnglandvalueorpercentiles =
#'                        factor(ComparedtoEnglandvalueorpercentiles,
#'                               levels = ordered_levels))
#' p <- compare_areas(df, AreaName, Value,
#'                    fill = ComparedtoEnglandvalueorpercentiles,
#'                    lowerci = LowerCI95.0limit,
#'                    upperci = UpperCI95.0limit,
#'                    order = "desc",
#'                    top_areas = top_names,
#'                    title = unique(df$IndicatorName))
#' p
#' @export
compare_areas <- function(data, area, value,
                          lowerci, upperci,
                          fill, order = "desc", top_areas,
                          title = "", xlab = "", ylab = "",
                          legend.position = "bottom") {
        area <- enquo(area)
        value <- enquo(value)
        if (order == "desc") {
                if (!missing(top_areas)) {
                        levels <- data %>%
                                filter(!((!!area) %in% top_areas)) %>%
                                droplevels() %>%
                                arrange(-(!!value)) %>%
                                select(!!area) %>%
                                pull() %>%
                                as.character()
                        levels <- rev(c(top_areas, levels))
                        data <- data %>%
                                mutate(!!quo_name(area) :=
                                               factor((!!area),
                                                      levels = levels))
                } else {
                        levels <- data %>%
                                droplevels() %>%
                                arrange(-(!!value)) %>%
                                select(!!area) %>%
                                pull() %>%
                                as.character() %>%
                                rev()
                        data <- data %>%
                                mutate(!!quo_name(area) :=
                                               factor((!!area),
                                                      levels = levels))

                }
        } else if (order == "asc") {
                if (!missing(top_areas)) {
                        levels <- data %>%
                                filter(!((!!area) %in% top_areas)) %>%
                                droplevels() %>%
                                arrange(!!value) %>%
                                select(!!area) %>%
                                pull() %>%
                                as.character() %>%
                                unique
                        levels <- rev(c(top_areas, levels))
                        data <- data %>%
                                mutate(!!quo_name(area) :=
                                               factor((!!area),
                                                      levels = levels))
                } else {
                        levels <- data %>%
                                droplevels() %>%
                                arrange(!!value) %>%
                                select(!!area) %>%
                                pull() %>%
                                as.character() %>%
                                rev()
                        data <- data %>%
                                mutate(!!quo_name(area) :=
                                               factor((!!area),
                                                      levels = levels))

                }

        }

        compare_areas <- ggplot(data,
                                aes_string(x = quo_text(area),
                                           y = quo_text(value))) +
                coord_flip() +
                labs(title = title,
                     x = ylab,
                     y = xlab)

        if (!missing(fill)) {
                fill <- enquo(fill)
                compare_areas <- compare_areas +
                        geom_col(aes_string(fill = quo_text(fill))) +
                        scale_fill_phe(name = "Area compared to England",
                                       theme = "fingertips")

        } else {
                compare_areas <- compare_areas +
                        geom_col()
        }
        if (!missing(lowerci) & !missing(upperci)) {
                lowerci <- enquo(lowerci)
                upperci <- enquo(upperci)
                compare_areas <- compare_areas +
                        geom_errorbar(aes_string(ymin = quo_text(lowerci),
                                                 ymax = quo_text(upperci)),
                                      width=.2, show.legend = FALSE)
        }
        compare_areas <- compare_areas +
                theme_phe("fingertips") +
                theme(legend.position = legend.position,
                      panel.grid.major.y = element_blank())
}

#' Plot an overview (tartan rug) of multiple indicators
#'
#' @return a ggplot of the overview/tartan rug plot
#' @inheritParams compare_areas
#' @param area field containing area names (unquoted)
#' @param indicator field containing indicator names (unquoted)
#' @param value field containing variable to be plotted (unquoted)
#' @param timeperiod field containing the time period (unquoted)
#' @param top_areas character vector; the areas to fix at the left
#' @param wrap_length number; maximum number of characters in indicator before
#'   wrapping it
#' @param value_label_size number; amount to scale the size of the value label
#' @family quick charts
#' @import ggplot2
#' @import dplyr
#' @importFrom rlang quo_text
#' @importFrom stringr str_wrap
#' @examples
#' library(fingertipsR)
#' library(dplyr)
#' region <- "North East region"
#' top_names <- c("England", region)
#' dfdom <- fingertips_data(DomainID = 1938133060) %>%
#'         filter(Timeperiod == "2016",
#'                Age == "All ages",
#'                (AreaName %in% top_names | ParentName == region)) %>%
#'         mutate(Value = round(Value, 1))
#' p <- overview(dfdom,
#'               area = AreaName,
#'               indicator = IndicatorName,
#'               value = Value,
#'               fill = ComparedtoEnglandvalueorpercentiles,
#'               top_areas = top_names, wrap_length = 40,
#'               value_label_size = 0.8)
#' p
#' @export
overview <- function(data, area, indicator, value,
                     fill, timeperiod, top_areas, wrap_length = 50,
                     value_label_size = 1) {
        area <- enquo(area)
        value <- enquo(value)
        indicator <- enquo(indicator)
        fill <- enquo(fill)
        value <- enquo(value)
        timeperiod <- enquo(timeperiod)

        if (!missing(top_areas)) {
                levels <- data %>%
                        filter(!((!!area) %in% top_areas)) %>%
                        droplevels() %>%
                        arrange((!!area)) %>%
                        select(!!area) %>%
                        pull() %>%
                        as.character() %>%
                        unique()
                levels <- c("Period", top_areas, levels)
                data <- data %>%
                        mutate(!!quo_name(area) :=
                                       factor((!!area),
                                              levels = levels))
        } else {
                levels <- data %>%
                        droplevels() %>%
                        arrange(!!area) %>%
                        select(!!area) %>%
                        pull() %>%
                        as.character() %>%
                        unique()
                data <- data %>%
                        mutate(!!quo_name(area) :=
                                       factor((!!area),
                                              levels = c("Period", levels)))
        }
        tp <- data %>%
                filter((!!area) == levels[2]) %>%
                mutate(!!quo_name(area) := "Period",
                       !!quo_name(fill) := NA,
                       !!quo_name(value) :=
                               as.character(str_wrap((!!timeperiod), 9)))
        data <- rbind(data, tp)
        levels <- data %>%
                droplevels() %>%
                arrange(!!indicator) %>%
                select(!!indicator) %>%
                pull() %>%
                as.character() %>%
                rev() %>%
                unique() %>%
                str_wrap(wrap_length)
        data <- data %>%
                mutate(!!quo_name(indicator) :=
                               factor(str_wrap((!!indicator), wrap_length),
                                      levels = levels))

        overview <- ggplot(data, aes_string(x = quo_text(area),
                                            y = quo_text(indicator))) +
                geom_tile(aes_string(fill = quo_text(fill)),
                          colour = "white") +
                geom_text(aes_string(label = quo_text(value)),
                          size = value_label_size * 4) +
                scale_fill_phe("fingertips") +
                scale_x_discrete(position = "top") +
                theme(legend.position = "none",
                      axis.text.x = element_text(angle = 90,
                                                 hjust = 0),
                      axis.text.y = element_text(size = rel(1)),
                      axis.title = element_blank(),
                      line = element_blank(),
                      rect = element_blank())
}

#' Plot compare indicators plot
#'
#' @return a ggplot of compare indicators for 2 indicators
#' @inheritParams compare_areas
#' @param x field containing x variable (unquoted)
#' @param y field containing y variable (unquoted)
#' @param point_size number; size of point
#' @param highlight_area character vector; list of areas for highlighting
#' @param area field containing areas - should contain contents of
#'   highlight_area. Only required if highlight_area has a value (unquoted)
#' @param add_R2 boolean; should R2 be displayed?
#' @family quick charts
#' @import ggplot2
#' @import dplyr
#' @importFrom rlang quo_text
#' @importFrom stats lm as.formula
#' @examples
#' \donttest{
#' # This example is untested because of the time required to retrieve the data
#' library(fingertipsR)
#' library(dplyr)
#' library(tidyr)
#' df <- fingertips_data(c(90362, 90366)) %>%
#'         group_by(IndicatorID) %>%
#'         filter(Timeperiod == "2014 - 16" &
#'                        Sex == "Female") %>%
#'         ungroup() %>%
#'         select(IndicatorID, AreaName, Value) %>%
#'         mutate(IndicatorID = paste0("x", IndicatorID)) %>%
#'         spread(IndicatorID, Value)
#' p <- compare_indicators(df,
#'                         x = x90362,
#'                         y = x90366,
#'                         xlab = "Healthy life expectancy at birth",
#'                         ylab = "Life expectancy at birth",
#'                         highlight_area = c("England", "Dorset"),
#'                         area = AreaName)
#' p}
#' @export
compare_indicators <- function(data, x, y,
                               xlab = "", ylab = "",
                               point_size = 4, highlight_area,
                               area, add_R2 = FALSE) {

        x <- enquo(x)
        y <- enquo(y)

        compare_indicators <- ggplot(data, aes_string(x = quo_text(x),
                                                      y = quo_text(y))) +
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
                area <- enquo(area)
                data <- data %>%
                        mutate(highlight = ifelse((!!area) %in% highlight_area, T, F))
                compare_indicators <- compare_indicators +
                        geom_point(data = data,
                                   aes(shape = highlight,
                                       fill = highlight),
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
                form <- as.formula(paste(y, " ~ ", x)[2])
                r2 <- summary(lm(form, data = data))
                r2frame <- data.frame(val = ifelse(r2$r.squared > 0.15,
                                                   paste("R^2:",round2(r2$r.squared, 2)),
                                              "R2 below 0.15;\nNot displayed"),
                                 x = -Inf,
                                 y = Inf)
                if (r2$r.squared > 0.15) {
                        compare_indicators <- compare_indicators +
                                geom_text(data = r2frame, aes(x = x,
                                                              y = y,
                                                              label = val),
                                          hjust = 0,
                                          vjust = 1,
                                          parse = TRUE) +
                                geom_abline(intercept = r2$coefficients[1, 1],
                                            slope = r2$coefficients[2, 1],
                                            colour = "#ED1F52")
                } else {
                        compare_indicators <- compare_indicators +
                                geom_text(data = r2frame, aes(x = x,
                                                              y = y,
                                                              label = val),
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
#' @param comparator string; name of comparator area (this should exist in the
#'   field described by the area parameter)
#' @param area_name string; name of the local area (this should exist in the
#'   field described by the area parameter)
#' @param subtitle string; text to use as subtitle to graph
#' @family quick charts
#' @import ggplot2
#' @import dplyr
#' @importFrom rlang quo_text
#' @examples
#' \donttest{
#' # This example is untested because of the time required to retrieve the data
#' library(fingertipsR)
#' library(dplyr)
#' df <- fingertips_data(90366) %>%
#'           filter(Sex == "Male")
#' p <- trends(df,
#'             timeperiod = Timeperiod,
#'             value = Value,
#'             area = AreaName,
#'             comparator = "England",
#'             area_name = "Cambridgeshire",
#'             fill = ComparedtoEnglandvalueorpercentiles,
#'             title = "Life expectancy at birth",
#'             subtitle = "Cambridgeshire compared to England",
#'             xlab = "Year",
#'             ylab = "Age (years)")
#' p}
#' @export
trends <- function(data, timeperiod, value,
                   area, comparator, area_name, fill,
                   lowerci, upperci,
                   title = "", subtitle = "",
                   xlab = "", ylab = "", point_size = 4) {
        timeperiod <- enquo(timeperiod)
        value <- enquo(value)
        area <- enquo(area)

        trends <- ggplot(data,
                         aes_string(x = quo_text(timeperiod),
                                    y = quo_text(value),
                                    group = quo_text(area))) +
                geom_line(data = filter(data, (!!area) == comparator),
                          colour = "black",
                          aes_string(linetype = quo_text(area))) +
                geom_point(data = filter(data, (!!area) == comparator),
                           fill = "black",
                           aes_string(shape = quo_text(area)),
                           size = point_size) +
                geom_line(data = filter(data, (!!area) == area_name),
                          colour = "#7CB5EC") +
                scale_linetype_manual(name = "",
                                      values = "solid",
                                      labels = comparator) +
                scale_shape_manual(name = "",
                                   values = 21,
                                   labels = comparator) +
                labs(title = title,
                     subtitle = subtitle,
                     x = xlab,
                     y= ylab) +
                theme_phe("fingertips") +
                theme(legend.position = "bottom")
        if (!missing(fill)) {
                fill <- enquo(fill)
                trends <- trends +
                        geom_point(data = filter(data, (!!area) == area_name),
                                   aes_string(fill = quo_text(fill)),
                                   shape = 21,
                                   size = point_size, show.legend = F) +
                        scale_fill_phe("fingertips")
        } else {
                trends <- trends +
                        geom_point(data = filter(data, (!!area) == area_name),
                                   fill = "#C9C9C9",
                                   shape = 21,
                                   size = point_size, show.legend = F)
        }
        if (!missing(lowerci) & !missing(upperci)) {
                lowerci <- enquo(lowerci)
                upperci <- enquo(upperci)
                trends <- trends +
                        geom_errorbar(data = filter(data, (!!area) == area_name),
                                      aes_string(ymin= quo_text(lowerci),
                                          ymax = quo_text(upperci)),
                                      width=.2)

        }
        return(trends)
}

#' Plot population pyramid
#'
#' @return a ggplot of a population pyramid against 2 optional comparators
#' @inheritParams compare_areas
#' @inheritParams compare_indicators
#' @inheritParams trends
#' @param sex field containing sex variable (unquoted)
#' @param age field containing age variable (unquoted)
#' @param comparator_1 string; name of comparator area (this should exist in the
#'   field described by the area parameter)
#' @param comparator_2 string; name of comparator area (this should exist in the
#'   field described by the area parameter)
#' @family quick charts
#' @import ggplot2
#' @import dplyr
#' @importFrom rlang quo_text
#' @importFrom scales pretty_breaks
#' @examples
#' \donttest{
#' # This example is untested because of the time required to retrieve the data
#' library(fingertipsR)
#' library(dplyr)
#' agelevels <- c("0-4", "5-9","10-14","15-19",
#'                "20-24","25-29","30-34",
#'                "35-39","40-44","45-49",
#'                "50-54","55-59","60-64",
#'                "65-69","70-74","75-79",
#'                "80-84","85-89","90+")
#' pops <- fingertips_data(92708) %>%
#'         filter(Timeperiod == "2015" &
#'                        Sex %in% c("Male", "Female") &
#'                        Age != "All ages") %>%
#'         mutate(Age = gsub(" yrs","", Age),
#'                Age = factor(Age,
#'                             levels = agelevels)) %>%
#'         droplevels()
#' p <- population(pops,
#'                 value = Value,
#'                 sex = Sex,
#'                 age = Age,
#'                 area = AreaName,
#'                 area_name = "Nottingham",
#'                 comparator_1 = "England",
#'                 comparator_2 = "East Midlands region",
#'                 title = "Age Profile",
#'                 subtitle = paste(unique(pops$IndicatorName), unique(pops$Timeperiod)),
#'                 xlab = "% of total population")
#' p}
#' @export
population <- function(data, value, sex, age,
                       area, area_name, comparator_1, comparator_2,
                       title, subtitle, xlab) {
        value <- enquo(value)
        sex <- enquo(sex)
        age <- enquo(age)
        area <- enquo(area)

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
                filter((!!area) %in% areas) %>%
                group_by(!!area) %>%
                mutate(!!quo_name(value) :=
                               100 * (!!value) / sum(!!value),
                       !!quo_name(value) :=
                               ifelse((!!sex) == "Male",
                                      -(!!value), (!!value)))
        extremex <- pretty_breaks(n = 3)(0:max(abs(pull(data, !!value)),
                                               na.rm = T))
        population <- ggplot(filter(data, (!!area) == area_name),
                             aes_string(y = quo_text(value),
                                        x = quo_text(age),
                                        fill = quo_text(sex))) +
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
                compdata1 <- filter(data, (!!area) == comparator_1)
                population <- population +
                        geom_line(data = compdata1,
                                  aes_string(y = quo_text(value),
                                             x = quo_text(age),
                                             group = interaction(pull(compdata1, !!sex), pull(compdata1, !!area)),
                                             col = quo_text(area)),
                                  size = 1.5)
                if (!missing(comparator_2)) {
                        compdata2 <- filter(data, (!!area) == comparator_2)
                        population <- population +
                                geom_line(data = compdata2,
                                          aes_string(y = quo_text(value),
                                                     x = quo_text(age),
                                                     group = interaction(pull(compdata2, !!sex), pull(compdata2, !!area)),
                                                     col = quo_text(area)),
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
#' @return a ggplot of boxplots for many areas over time
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
#' \donttest{
#' # This example is untested because of the time required to retrieve the data
#' library(fingertipsR)
#' library(dplyr)
#' df <- fingertips_data(90366) %>%
#'       filter(Sex == "Male" &
#'              AreaType == "County & UA")
#' p <- box_plots(df,
#'                timeperiod = Timeperiod,
#'                value = Value,
#'                title = "Life expectancy at birth",
#'                subtitle = "Males in Uper Tier Local Authorities England",
#'                ylab = "Age (years)")
#' p}
#' @export
box_plots <- function(data, timeperiod, value,
                      title = "", subtitle = "",
                      xlab = "", ylab = "") {
        timeperiod <- enquo(timeperiod)
        value <- enquo(value)
        data <- data %>%
                group_by(!!timeperiod) %>%
                summarise(y5 = quantile((!!value), 0.05, na.rm = TRUE),
                          y25 = quantile((!!value), 0.25, na.rm = TRUE),
                          y50 = median((!!value), na.rm = TRUE),
                          y75 = quantile((!!value), 0.75, na.rm = TRUE),
                          y95 = quantile((!!value), 0.95, na.rm = TRUE))
        boxplots <- ggplot(data, aes_string(x = quo_text(timeperiod))) +
                geom_boxplot(aes(ymin = y5, lower = y25,
                                 middle = y50, upper = y75,
                                 ymax = y95),
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
                             aes(x = xmin + ((xmax - xmin) * 0.25),
                                 xend = xmin + ((xmax - xmin) * 0.75),
                                 y = ymax,
                                 yend = ymax)) +
                geom_segment(data=dat,
                             aes(x = xmin + ((xmax - xmin) * 0.25),
                                 xend = xmin + ((xmax - xmin) * 0.75),
                                 y = ymin,
                                 yend = ymin)) +
                geom_segment(data=dat,
                             aes(x = xmin,
                                 xend = xmax,
                                 y = middle,
                                 yend = middle),
                             colour="red", size = 1)

}

#' Plot a choropleth map for an indicator
#'
#' @return a either a static or interactive ggplot choropleth map
#' @inheritParams compare_areas
#' @inheritParams trends
#' @param area_code field containing area codes to join to shape file imported
#'   from ONS API
#' @param type string; the output map required. Can be "static" or "interactive"
#' @param ons_api string; GeoJSON address provided from the ONS geography portal
#' @param copyright_size number; determine size of the copyright text
#' @param copyright_year number (length 4) or Date class; the copyright year
#'   displayed at bottom of the map. Applies to static maps only
#' @param name_for_label if interactive map, name of field containing area names
#'   to be used for label (unquoted) - optional
#' @param fill field to be used to determine the colouring of the areas
#'   (unquoted)
#' @family quick charts
#' @import ggplot2
#' @import dplyr
#' @importFrom rlang quo_text quo sym
#' @importFrom geojsonio geojson_read
#' @importFrom leaflet colorFactor leaflet addTiles addPolygons addLegend
#' @importFrom stats setNames
#' @importFrom fingertipsR fingertips_data
#' @importFrom sf st_as_sf
#' @examples
#' \donttest{
#' # This example is untested because of the time required to retrieve the data
#' library(fingertipsR)
#' library(dplyr)
#' df <- fingertips_data(90366) %>%
#'         filter(Sex == "Male" &
#'                        AreaType == "County & UA" &
#'                        Timeperiod == "2014 - 16")
#' ons_api <- "https://opendata.arcgis.com/datasets/687f346f5023410ba86615655ff33ca9_4.geojson"
#'
#' p <- map(df,
#'          ons_api = ons_api,
#'          area_code = AreaCode,
#'          fill = ComparedtoEnglandvalueorpercentiles,
#'          title = "Life expectancy at birth",
#'          subtitle = "Males in Upper Tier Local Authorities England",
#'          copyright_year = 2018)
#'
#' ## For an interactive (leaflet) map
#' p <- map(df,
#'          ons_api = ons_api,
#'          area_code = AreaCode,
#'          fill = ComparedtoEnglandvalueorpercentiles,
#'          type = "interactive",
#'          value = Value,
#'          name_for_label = AreaName,
#'          title = "Life expectancy at birth<br>Males within UTLAs in England")
#' p}
#' @export
map <- function(data, ons_api, area_code, fill, type = "static", value, name_for_label,
                title = "", subtitle = "", copyright_size = 4, copyright_year = Sys.Date()) {
        area_code <- enquo(area_code)
        fill <- enquo(fill)
        shp <- geojson_read(ons_api, what = "sp") %>%
                st_as_sf()
        all_area_codes <- data %>%
                pull(!!area_code) %>%
                unique()
        join_field <- vapply(shp, function(x) sum(x %in% all_area_codes),
                             numeric(1))
        join_field <- names(join_field[join_field == max(join_field)])
        if (length(join_field) != 1) stop("There is no clear field in the shape file that contains the area codes in the field you have identified")
        shp <- shp %>%
                filter(grepl("^E", !! quo(!! sym(join_field))))
        if (type == "static") {
                data <- data %>%
                        mutate(!!quo_name(area_code) :=
                                       as.character(!!area_code))
                shp <- shp %>%
                        mutate(AreaCode = as.character(!! quo(!! sym(join_field)))) %>%
                        merge(data,
                              by.x = "AreaCode",
                              by.y = quo_text(area_code),
                              all.x = TRUE)
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
                        geom_sf(aes_string(fill = quo_text(fill))) +
                        coord_sf(datum = NA) +
                        scale_fill_phe(name = "",
                                       "fingertips") +
                        theme_void() +
                        geom_text(data = copyright,
                                  aes(x = x,
                                      y = y,
                                      label = val),
                                  colour = "black",
                                  hjust = 1,
                                  vjust = 0,
                                  size = copyright_size) +
                        labs(title = title,
                             subtitle = subtitle)

        } else if (type == "interactive") { # nocov start
                ftipspal <- scale_fill_phe("fingertips")
                ftipspal <- ftipspal$palette(1)
                data <- data %>%
                        mutate(!!quo_name(fill) :=
                                       factor(!!fill,
                                              levels = names(ftipspal)))
                factpal <- colorFactor(ftipspal,
                                       domain = pull(data, !!fill),
                                       ordered = TRUE)
                data <- data %>%
                        mutate(!!quo_name(area_code) :=
                                       as.character(!!area_code))
                shp <- shp %>%
                        mutate(AreaCode = as.character(!! quo(!! sym(join_field)))) %>%
                        merge(data,
                              by.x = "AreaCode",
                              by.y = quo_text(area_code),
                              all.x = TRUE)
                value <- enquo(value)
                if (!missing(name_for_label)) {
                        name_for_label <- enquo(name_for_label)
                        labels <- sprintf("<strong>%s</strong><br/>Value: %g",
                                          pull(shp, !!name_for_label),
                                          pull(shp, !!value))
                } else {
                        labels <- sprintf("<strong>%s</strong><br/>Value: %g",
                                          pull(shp, !!join_field),
                                          pull(shp, !!value))
                }

                map <- leaflet(shp)  %>%
                        addTiles() %>%
                        addPolygons(fillColor =
                                            ~factpal(pull(shp, !!fill)),
                                    weight = 2,
                                    opacity = 1,
                                    color = "white",
                                    dashArray = "3",
                                    fillOpacity = 0.7,
                                    popup = labels) %>%
                        addLegend("topright",
                                  pal = factpal,
                                  values = fill,
                                  title = title,
                                  opacity = 1)
        } # nocov end
        return(map)

}

#' Plot spine chart
#'
#' Returns ggplot of spine chart
#' @return a ggplot object containing a spine chart
#' @details the function draws a bar chart (which is the spine) and then plots
#'   the data table (if datatable = TRUE) using geom_text. The bar chart is
#'   always plotted between 0 and 1 on the x scale. The columns in the data
#'   table are controlled by the header_positions argument. To adjust the length
#'   of the bars in the visualisation, amend the header_positions argument. The
#'   more negative the first value of the vector that goes into
#'   header_positions, the more condensed the bar part of the visualisation will
#'   be.
#' @param data a data frame to create the spine chart from. the data frame
#'   should contain data for all area types included in the chart (eg, if
#'   plotting for County & UA with a comparator of region and a median line for
#'   national, the data frame should contain all of these data)
#' @param value unquoted field name containing the values to be plotted
#' @param count unquoted field name where the count (numerator) is stored
#' @param area_code unquoted field name where area codes are stored
#'   (local_area_code, median_line_area_code and comparator_area_code, if using,
#'   should all exist in this field)
#' @param local_area_code string; the code of the area that the spine chart is
#'   being drawn for
#' @param indicator unquoted field name of the field containing the indicator
#'   labels. Take care as errors will occur where indicator labels are the same
#'   but data exist for multiple sub-categories (for example, sex or age)
#' @param timeperiod unquoted field name of the time period field
#' @param polarity unquoted field name containing the polarity information
#'   (currently only handles polarity returned by fingertipsR package)
#' @param significance unquoted field name describing the statistical
#'   significance for that indicator (eg, Better, Worse, Similar etc)
#' @param area_type unquoted field name containing area type information. This
#'   ensures the vertabra are only plotted for the same area types as the
#'   local_area area type (eg, when plotting a spine chart for County & UA
#'   areas, regions and national area types will be removed)
#' @param cols named character vector for the cols that will be applied to the
#'   significance field. The names should contain all of the levels in the
#'   significance field of the data frame. Defaults to the Fingertips colours
#'   based on the outputs from the API
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
#'   if they are overlapping. Must have a length of 7. Defaults to c(-1.43,
#'   -.53, -.35, -.25, -.15, -0.05, 1.05)
#' @param header_labels character vector; labels used for the titles of the
#'   columns for a data table. Must have a length of 7. Defaults to
#'   c("Indicator", "Time period", "Local count","Local value", "England value",
#'   "Worst/Lowest","Best/Highest")
#' @param domain unquoted field name describing the grouping of the domains if
#'   wishing to split the spine chart into domains
#' @param relative_domain_text_size numeric; control the text size for the
#'   domain labels (if include.domains = TRUE) relative to 1
#' @param datatable logical; default = TRUE, display data table alongside spine
#'   chart
#' @param indicator_label_nudgex number; nudge the placement of the indicator
#'   label in the x direction. Negative values nudge to the left
#' @param show_dividers string; whether to display horizontal lines between
#'   indicators. Values can be "all" or "outer". Any other value will not
#'   generate lines
#' @param datatable_line_height number; height of wrapped lines in the data
#'   table
#' @param percent_display number between 0 and 1; the percentage of values that
#'   needs to exist for a spine to display. Default is 0.25
#' @details This function filters for the area type that is the same as your
#'   local area type and then calculates the "vertebra" from those data.
#'   Therefore, if you are comparing outputs with those seen on the Fingertips
#'   website, ensure you perform the same preprocessing. For example, some
#'   profiles display spine charts where small areas, such as Isles of Scilly,
#'   are removed before the spine is produced.
#' @import ggplot2
#' @import dplyr
#' @importFrom grDevices rgb
#' @importFrom rlang quo_text
#' @importFrom utils tail
#' @importFrom stats reformulate
#' @importFrom stringr str_trim
#' @importFrom lemon facet_rep_grid
#' @examples
#' \donttest{
#' # This example is untested because of the time required to retrieve the data
#' library(fingertipsR)
#' library(dplyr)
#' df <- fingertips_data(DomainID = 1938133222, rank = TRUE) %>%
#'            filter(Timeperiod == "2016")
#' p <- area_profiles(df,
#'                    value = Value,
#'                    count = Count,
#'                    area_code = AreaCode,
#'                    local_area_code = "E06000020",
#'                    indicator = IndicatorName,
#'                    timeperiod = Timeperiod,
#'                    polarity = Polarity,
#'                    significance = ComparedtoEnglandvalueorpercentiles,
#'                    area_type = AreaType,
#'                    cols = "fingertips",
#'                    median_line_area_code = "E92000001",
#'                    comparator_area_code = "E12000005",
#'                    datatable = TRUE,
#'                    relative_domain_text_size = 0.75,
#'                    relative_text_size = 1.2,
#'                    bar_width = 0.68,
#'                    indicator_label_nudgex = -0.5)
#' p}
#'
#' @export
area_profiles <- function(data,
                          value,
                          count,
                          area_code,
                          local_area_code,
                          indicator,
                          timeperiod,
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
                          header_positions  = c(-1.43, -.53, -.35, -.25, -.15, -0.05, 1.05),
                          header_labels = c("Indicator", "Time\nperiod",
                                            "Local\ncount","Local\nvalue",
                                            "England\nvalue",
                                            "Worst/\nLowest","Best/\nHighest"),
                          indicator_label_nudgex = -0.075,
                          domain = no_domains,
                          relative_domain_text_size = 1,
                          show_dividers = "none",
                          datatable = TRUE,
                          datatable_line_height = 0.6,
                          percent_display = 0.25) {

        test_area_code <- enquo(area_code)
        dummy_polarity <- enquo(polarity)

        if (sum(median_line_area_code %in% pull(data, !!test_area_code)) < 1)
                stop(paste0(median_line_area_code, " not in area_code field provided"))
        if (sum(local_area_code %in% pull(data, !!test_area_code)) < 1)
                stop(paste0(local_area_code, " not in area_code field provided"))
        if (!is.na(comparator_area_code) &
            sum(comparator_area_code %in% pull(data, !!test_area_code)) < 1)
                stop(paste0(comparator_area_code, " not in area_code field provided"))

        data <- data %>%
                mutate(!!quo_name(dummy_polarity) :=
                               stringr::str_trim(!!dummy_polarity))

        area_code <- enquo(area_code)
        indicator <- enquo(indicator)

        # check for multiple values for an area per indicator
        check_message <- spine_data_check(data, indicator, area_code)
        if (!is.na(check_message)) stop(check_message)

        # create data table
        value <- enquo(value)
        count <- enquo(count)
        timeperiod <- enquo(timeperiod)
        if (is.factor(pull(data, !!indicator))) {
                ind_order <- levels(pull(data, !!indicator))
        } else {
                data <- data %>%
                        mutate(!!quo_name(indicator) :=
                                       factor(!!indicator))
                ind_order <- levels(pull(data, !!indicator))
        }

        if (datatable == TRUE) {
                dftable <- create_datatable(data,
                                            indicator,
                                            area_code,
                                            timeperiod,
                                            count, value,
                                            local_area_code,
                                            median_line_area_code,
                                            comparator_area_code)
                dftable <- dftable %>%
                        mutate(!!quo_name(indicator) :=
                                                  factor(!!indicator,
                                                         levels = ind_order))
        } else {
                dftable <- NA
        }

        # rescale data for charting
        significance <- enquo(significance)
        polarity <- enquo(polarity)
        area_type <- enquo(area_type)
        dfrescaled <- spine_rescaler(data,
                                     area_code,
                                     indicator,
                                     significance,
                                     polarity,
                                     area_type,
                                     value,
                                     timeperiod,
                                     local_area_code,
                                     median_line_area_code,
                                     comparator_area_code,
                                     percent_display)
        domain <- enquo(domain)
        if (quo_text(domain) == "no_domains") {
                domain_field <- NA
        } else {
                domain_field <- domain
                domain_lu <- data %>%
                        select(!!indicator, !!domain) %>%
                        mutate(!!quo_name(domain) := factor(!!domain)) %>%
                        unique
                dfrescaled$bars <- dfrescaled$bars %>%
                        merge(domain_lu,
                              by = rlang::quo_text(indicator),
                              all.x =TRUE)
                dfrescaled$points <- dfrescaled$points %>%
                        merge(domain_lu,
                              by = rlang::quo_text(indicator),
                              all.x =TRUE)
                if (is.data.frame(dftable))
                        dftable <- dftable %>%
                        merge(domain_lu,
                              by = rlang::quo_text(indicator),
                              all.x =TRUE)
        }

        if (cols == "fingertips") {
                cols <-  c('Better' = '#92D050', 'Same' = '#FFC000',
                           'Worse' = '#C00000', 'Not compared' = '#C9C9C9',
                           'None' = '#A6A6A6', 'Higher' = '#BED2FF',
                           'Similar' = '#FFC000', 'Lower'='#5555E6',
                           'Worst' = '#FFFFFF','Q25' = '#C9C9C9',
                           'Q75' = '#8B8B8B','Best' = '#C9C9C9')
        }

        vline_length <- dfrescaled$bars %>%
                pull(!!indicator) %>%
                unique %>%
                length

        dfrescaled$bars <- dfrescaled$bars %>%
                mutate(y = case_when(
                        y == 1.05 ~ header_positions[length(header_positions)],
                        y == -0.05 ~ header_positions[length(header_positions) - 1]
                        ),
                       !!quo_name(indicator) :=
                               factor(!!indicator,
                                      levels = ind_order))
        dfrescaled$points <- dfrescaled$points  %>%
                mutate(!!quo_name(indicator) :=
                               factor(!!indicator,
                                      levels = ind_order))
        p <- ggplot(dfrescaled$bars,
                    aes_string(x = quo_text(indicator),
                               y = "quantiles")) +
                geom_bar(stat = "identity", width = bar_width,
                         aes_string(fill = "GraphPoint"))


        if (!is.na(comparator_area_code)) {
                rescaled_comparator_field <- "region"
                p <- p +
                        geom_point(data = dfrescaled$points,
                                   aes_string(x = quo_text(indicator),
                                              y = rescaled_comparator_field),
                                   shape = comparator_point_shape,
                                   colour = comparator_point_outline,
                                   fill = comparator_point_fill,
                                   size = 2.5 * relative_point_size)
        }
        p <- p +
                geom_point(data = dfrescaled$points,
                           aes_string(x = quo_text(indicator),
                                      y = "area",
                                      fill = quo_text(significance)),
                           shape = local_point_shape,
                           colour = local_point_outline,
                           size = 2.5 * relative_point_size) +
                geom_hline(yintercept = 0.5, col = "darkred") +
                coord_flip() +
                scale_fill_manual(values = cols) +
                theme_minimal() +
                theme(panel.grid.major = element_blank(),
                      axis.text = element_text(colour = "black")) +
                labs(x = "", y = "")

        if (is.data.frame(dftable)) {
                dt_indicator <- indicator
                dt_area_field <- "Area_value"
                dt_comparator_field <- "Comparator_value"
                dt_median_field <- "Median_value"
                dt_area_count <- count
                dt_timeperiod <- timeperiod
                dftable <- dftable %>%
                        rename(ind = !!dt_indicator,
                               count = !!dt_area_count,
                               tp = !!dt_timeperiod)
                lims <- range(header_positions)
                lims[1] <- lims[1] + indicator_label_nudgex
                lims <- lims * 1.06
                p <- p +
                        scale_y_continuous(position = "bottom",
                                           breaks = header_positions,
                                           limits = lims,
                                           labels = header_labels,
                                           expand = c(0, 0)) +
                        geom_text(aes(label = label, y = y),
                                  col = "black",
                                  size = 2.5 * relative_text_size,
                                  lineheight = datatable_line_height,
                                  hjust = 1
                        ) +
                        geom_text(data = dftable,
                                  aes_string(label = dt_median_field,
                                             x = "ind"),
                                  y = header_positions[5],
                                  col = "black",
                                  size = 2.5 * relative_text_size,
                                  parse = TRUE,
                                  lineheight = datatable_line_height,
                                  hjust = 1
                        ) +
                        geom_text(data = dftable,
                                  aes_string(label = dt_area_field,
                                             x = "ind"),
                                  y = header_positions[4],
                                  col = "black",
                                  size = 2.5 * relative_text_size,
                                  parse = TRUE,
                                  lineheight = datatable_line_height,
                                  hjust = 1
                        ) +
                        geom_text(data = dftable,
                                  aes(label = count, x = ind),
                                  y = header_positions[3],
                                  col = "black",
                                  size = 2.5 * relative_text_size,
                                  parse = TRUE,
                                  lineheight = datatable_line_height,
                                  hjust = 1
                        ) +
                        geom_text(data = dftable,
                                  aes(label = tp, x = ind),
                                  y = header_positions[2],
                                  col = "black",
                                  size = 2.5 * relative_text_size,
                                  lineheight = datatable_line_height,
                                  hjust = 1
                        ) +
                        geom_text(data = dftable,
                                  aes(label = ind,
                                      y = header_positions[1],
                                      x = ind),
                                  hjust = 0,
                                  nudge_y = indicator_label_nudgex,
                                  col = "black",
                                  size = 2.5 * relative_text_size,
                                  lineheight = datatable_line_height
                        ) +
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
        if (quo_text(domain) != "no_domains") {
                p <- p +
                        facet_rep_grid(reformulate(".", quo_text(domain)),
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
                        geom_vline(xintercept=seq(-0.5, vline_length + 0.5),
                                   colour="black",
                                   size = 0.2)
        } else if (show_dividers == "outer") {
                p <- p +
                        # geom_vline(xintercept = 0.54,
                        #            colour="black",
                        #            size = 0.2) +
                        # scale_x_discrete(expand = c(0.05, 0.05))
                        # scale_y_continuous() +
                        theme(axis.line.x = element_line())
        }
        p + theme(axis.text.y = element_blank(),
                  axis.title.y = element_blank(),
                  axis.ticks.length = unit(0, "mm"))
        return(p)

}
