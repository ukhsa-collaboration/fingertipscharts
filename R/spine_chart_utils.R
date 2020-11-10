#' Preprocess  data for spine chart
#'
#' Returns a data frame with the latest time period of data for each indicator
#' name.
#' @details This processing only takes place on the indicator field and the time
#'   period field provided. If the data contains multiple sexes or age groups
#'   for an indicator, make sure the indicator field reflects this.
#' @inheritParams area_profiles
#' @param indicator unquoted field name for indicators. This should be what is
#'   presented as the label for the final spine chart, hence should be unique
#'   for each vertabra. Be careful the indicator doesn't have sub-categories
#'   based on other fields, such as sex (male, female, persons) or age group
#' @param timeperiod_sortable unquoted field name containing the time period
#'   that is numeric and sortable, such that higher values are a later time
#'   period
#' @import dplyr
#' @return A processed data frame for latest time periods of given indicators
#' @export
spine_preprocess <- function(data, indicator, timeperiod_sortable) {
        # timeperiod_sortable <- enquo(timeperiod_sortable)
        # indicator <- enquo(indicator)
        data <- data %>%
                group_by({{ indicator }}) %>%
                filter({{ timeperiod_sortable }} ==
                               max({{ timeperiod_sortable }})) %>%
                ungroup()
}

#' Check function for multiple values for an area in an indicator for spine chart
#' @inheritParams area_profiles
spine_data_check <- function(data, indicator, area_code) {
        data <- data %>%
                group_by({{ indictor }}, {{ area_code }}) %>%
                count() %>%
                filter(n > 1)
        if (nrow(data) > 0) {
                area <- data %>%
                        slice(1) %>%
                        pull({{ area_code }})
                indicatorname <- data %>%
                        slice(1) %>%
                        pull({{ indicator }}) %>%
                        as.character()
                message <- paste("Some areas have multiple values for an indicator. An example is",
                                 area,
                                 "for the indicator",
                                 indicatorname)
        } else {
                message <- NA
        }
        return(message)

}

#' Data table supporting information
#'
#' Returns a data frame containing the data that sits next to the spine chart
#' @inheritParams area_profiles
#' @param dps number of decimal places to use in the data table
#' @param header_width x dimension of chart to be used for normalising the arrow
#'   length when horizonal
#' @import dplyr
#' @importFrom tidyr pivot_wider
#' @importFrom rlang  .data eval_tidy as_name enquo
#' @importFrom scales comma
#' @return A data frame containing the information that sits alongside the spine
#'   chart
create_datatable <- function(data, indicator,
                             area_code, timeperiod,
                             trend,
                             count, value,
                             local_area_code,
                             median_line_area_code,
                             comparator_area_code,
                             dps = 1,
                             header_width,
                             horizontal_arrow_multiplier) {
        if (is.na(comparator_area_code)) {
                area_codes <- c(local_area_code, median_line_area_code)
        } else {
                area_codes <- c(local_area_code, median_line_area_code, comparator_area_code)
        }
        data_temp <- data %>%
                filter({{ area_code }} %in% area_codes) %>%
                mutate({{ area_code }} :=
                               case_when({{ area_code }} == local_area_code ~ "Area_value",
                                         {{ area_code }} == median_line_area_code ~ "Median_value",
                                         {{ area_code }} == comparator_area_code ~ "Comparator_value",
                                         TRUE ~ "Error"),
                       {{ value }} := as.character({{ value }}),
                       dps_2b_removed = {{ dps }},
                       {{ value }} := suppressWarnings(case_when(
                               is.na(as.numeric({{ value }})) ~ {{ value }},
                               TRUE ~ ifelse(
                                       !is.na(dps_2b_removed), paste0("\'",
                                                                      format(comma(round2(as.numeric({{ value }}), dps),
                                                                                   accuracy = 1 / (10 ^ dps)), nsmall = 1),
                                                                      "\'"),
                                       paste0("\'",
                                              formatC(as.numeric({{ value }}),
                                                      format = "f",
                                                      big.mark = ",",
                                                      drop0trailing = TRUE),
                                              "\'")
                                       )))) %>%
                select({{ indicator }}, {{ area_code }}, {{ timeperiod }}, {{ value }}) %>%
                tidyr::pivot_wider(names_from = {{ area_code }},
                                   values_from = {{ value }})
        data_count <- data %>%
                filter({{ area_code }} == local_area_code) %>%
                select({{ indicator }}, {{ count }}) %>%
                mutate({{ count }} :=
                               suppressWarnings(ifelse(is.na(as.integer({{ count }})),
                                                       {{ count }},
                                                       paste0("'",
                                                              scales::comma(round2(as.numeric({{ count }}), 0),
                                                                            accuracy = 1),
                                                              "'"))))
        if (is.character(rlang::eval_tidy(trend, data)) |
            is.factor(rlang::eval_tidy(trend, data))) {

                data_trend <- data %>%
                        filter(({{ area_code }}) == local_area_code) %>%
                        select({{ indicator }}, {{ trend }}) %>%
                        mutate(direction = case_when(
                                grepl("decreasing", tolower({{ trend }})) ~ pi,
                                grepl("increasing", tolower({{ trend }})) ~ 0,
                                grepl("no significant change", tolower({{ trend }})) ~ pi / 2,
                                TRUE ~ NA_real_),
                               trend_sig = case_when(
                                       grepl("better", tolower({{ trend }})) ~ "Better",
                                       grepl("worse", tolower({{ trend }})) ~ "Worse",
                                       grepl("no significant change", tolower({{ trend }})) ~ "Similar",
                                       grepl("increasing", tolower({{ trend }})) ~ "Higher",
                                       grepl("decreasing", tolower({{ trend }})) ~ "Lower",
                                       TRUE ~ "Not compared"),
                               radius = case_when(
                                       grepl("no significant change", tolower({{ trend }})) ~ 0.1 * header_width * horizontal_arrow_multiplier / (n() * 1.5),
                                       grepl("increasing|decreasing", tolower({{ trend }})) ~ 0.1,
                                       TRUE ~ NA_real_
                                       )) %>%
                        select(-{{ trend }})
        } else {
                data_trend <- data %>%
                        select({{ indicator }}) %>%
                        unique() %>%
                        mutate(direction = NA,
                               trend_sig = "",
                               radius = NA)

        }
        data_temp <- merge(data_temp, data_count,
                           by = rlang::as_name(rlang::enquo(indicator)),
                           all.x = TRUE) %>%
                merge(data_trend,
                      by = rlang::as_name(rlang::enquo(indicator)),
                      all.x = TRUE) %>%
                select({{ indicator }}, .data$direction, .data$trend_sig, {{ timeperiod }}, {{ count }}, everything())


        return(data_temp)
}


#' Rescale spine data
#'
#' Rescales data so it can be plotted on a spine chart
#' @return A list containing "bars" and "points" which contains data that can be
#'   passed to the phe_spine_chart function
#' @inheritParams area_profiles
#' @inheritParams create_datatable
#' @import dplyr
#' @importFrom tibble column_to_rownames rownames_to_column
#' @importFrom purrr map map_df pmap
#' @importFrom stringr str_trim str_locate str_extract
#' @importFrom tidyr gather
#' @importFrom stats quantile
#' @importFrom scales comma
#' @importFrom rlang as_name enquo
spine_rescaler <- function(data,
                           area_code,
                           indicator,
                           significance,
                           polarity,
                           area_type,
                           value,
                           timeperiod,
                           local_area_code,
                           median_line_area_code,
                           comparator_area_code = NA,
                           percent_display,
                           dps = 1) {
        # make sure value field is numeric and doesn't contain annotations (based on Health Profiles annotations)
        data <- data %>%
                mutate({{ value }} :=
                               suppressWarnings(as.character({{ value }})),
                       {{ value }} :=
                               ifelse(grepl("^[0-9]", {{ value }}),
                                      str_extract({{ value }}, "^\\d+\\.*\\d*"),
                                      NA),
                       {{ value }} :=
                               suppressWarnings(as.numeric({{ value }})))

        areatype <- filter(data, {{ area_code }} == local_area_code) %>%
                pull({{ area_type }}) %>%
                unique
        remove_data <- data %>%
                filter({{ area_type }} == areatype) %>%
                group_by({{ indicator }}) %>%
                summarise(percent_na = sum(is.na({{ value }})) / n()) %>%
                filter(percent_na >= percent_display) %>%
                pull({{ indicator }})

        # convert indicators to remove to na
        if (length(remove_data) > 0) {
                data <- data %>%
                        mutate({{ value }} :=
                                       ifelse({{ indicator }} %in% remove_data,
                                              NA,
                                              {{ value }}))
        }

        create_point_data <- function(data, areacode){
                if (areacode %in% c(median_line_area_code, comparator_area_code)){
                        data <- data %>%
                                filter({{ area_code }} == areacode) %>%
                                select({{ indicator }}, {{ value }}) %>%
                                rename(regionalvalue = {{ value }})
                } else {
                        data <- data %>%
                                filter({{ area_code }} == areacode) %>%
                                select({{ indicator }}, {{ significance }},
                                       {{ polarity }}, {{ value }}) %>%
                                rename(areavalue = {{ value }},
                                       Significance = {{ significance }})
                }
                data <- data.frame(data)
        }
        if (!is.na(comparator_area_code))
                parentdata <- create_point_data(data, comparator_area_code)
        areadata <- create_point_data(data, local_area_code)
        mean <- create_point_data(data, median_line_area_code)
        data <- filter(data, {{ area_type }} == areatype)

        quantiles <- data %>%
                split(pull(data, {{ indicator }})) %>%
                purrr::map(rlang::as_name(rlang::enquo(value))) %>%
                map_df(quantile, na.rm = TRUE, .id = rlang::as_name(rlang::enquo(indicator))) %>%
                data.frame()

        names(quantiles)[-1] <- c(paste0("Q", 100 * seq(0, 1, by = 0.25)))
        quantiles[, "Q50"] <- NULL
        quantiles <- quantiles %>%
                merge(mean,
                      by = rlang::as_name(rlang::enquo(indicator)),
                      all.x = TRUE) %>%
                rename(mean = regionalvalue)

        scaled_spine_inputs <- function(IndicatorName, Q0, Q25, mean, Q75, Q100, Significance, Polarity, areavalue, regionalvalue) {
                Polarity <- stringr::str_trim(Polarity)
                quantiles <- structure(as.numeric(c(Q0, Q25, mean, Q75, Q100)),
                                       names = c("0%", "25%", "mean", "75%", "100%"))
                areavalue <- as.numeric(areavalue)
                if (!is.na(regionalvalue))
                        regionalvalue <- as.numeric(regionalvalue)
                if (grepl("Low is good",Polarity)) {
                        quantiles <- rev(quantiles)
                }
                scale_min <- ifelse(quantiles["mean"] - quantiles["0%"] >
                                            quantiles["100%"] - quantiles["mean"],
                                    quantiles["0%"],
                                    quantiles["mean"] - (quantiles["100%"] - quantiles["mean"]))
                scale_max <- ifelse(scale_min == quantiles["0%"],
                                    quantiles["mean"] + (quantiles["mean"] - quantiles["0%"]),
                                    quantiles["100%"])

                rescale <- function(val){
                        rescale <- (val - scale_min) / (scale_max - scale_min)
                        return(rescale)
                }
                quantiles <- rescale(quantiles[names(quantiles) != "mean"])
                if (!is.na(regionalvalue)) {
                        pointdata <- rescale(c(areavalue,regionalvalue))
                        names(pointdata) <- c("area","region")
                } else {
                        pointdata <- rescale(areavalue)
                        names(pointdata) <- "area"
                }

                if (grepl("Low is good",Polarity)) {
                        quantiles <- 1 - quantiles
                        quantiles <- diff(c(0,quantiles))
                        pointdata <- 1 - pointdata
                } else {
                        quantiles <- diff(c(0,quantiles))
                }

                graphpoints <- c("Worst","Q25","Q75","Best")
                scaled_spine_inputs <- list(bars = tibble({{ indicator }} := IndicatorName,
                                                          quantiles = quantiles,
                                                          GraphPoint = factor(graphpoints, levels = rev(graphpoints))),
                                            points = tibble({{ indicator }} := IndicatorName,
                                                            significance = Significance,
                                                            area = pointdata[1],
                                                            region = pointdata[2]))
        }

        dfgraph <- merge(quantiles, areadata,
                         by = rlang::as_name(rlang::enquo(indicator)),
                         all.x = TRUE)
        if (!is.na(comparator_area_code)) {
                dfgraph <- dfgraph %>%
                        merge(parentdata,
                              by = rlang::as_name(rlang::enquo(indicator)),
                              all.x =TRUE)
        } else {
                dfgraph <- dfgraph %>%
                        mutate(regionalvalue = NA)
        }

        dfgraph <- dfgraph %>%
                rename(IndicatorName = {{ indicator }},
                       Polarity = {{ polarity }}) %>%
                lapply(purrr::map, .f = as.character) %>%
                pmap(scaled_spine_inputs)
        dfgraphfinal <- list(bars = suppressWarnings(map_df(dfgraph, "bars")),
                             points = suppressWarnings(map_df(dfgraph, "points")))
        dfpolarity <- areadata %>%
                select({{ indicator }}, {{ polarity }})
        dfannotate <- quantiles %>%
                merge(dfpolarity,
                      by = rlang::as_name(rlang::enquo(indicator)),
                      all.x =TRUE) %>%
                mutate(reverse = ifelse(grepl("Low is good", {{ polarity }}),
                                        TRUE,
                                        FALSE),
                       Worst = ifelse(reverse == TRUE, Q100, Q0),
                       Best = ifelse(reverse == TRUE, Q0, Q100)) %>%
                select({{ indicator }}, Best, Worst) %>%
                gather(GraphPoint, label, Best:Worst) %>%
                mutate(y = ifelse(GraphPoint == "Best", 1.05, -0.05),
                       dps_2b_removed = dps,
                       label = ifelse(is.na(label),
                                      NA,
                                      ifelse(!is.na(dps_2b_removed),
                                             format(comma(round2(as.numeric(label), dps),
                                                          accuracy = 1 / (10 ^ dps)), nsmall = 1),
                                             formatC(as.numeric(label),
                                                     format = "f",
                                                     big.mark = ",",
                                                     drop0trailing = TRUE))),
                       GraphPoint = factor(GraphPoint, levels = c("Best","Q75","Q25","Worst"))) %>%
                select(-(dps_2b_removed))

        timeperiod <- data %>%
                select({{ indicator }}, {{ timeperiod }}) %>%
                ungroup %>%
                unique %>%
                mutate({{ indicator }} := as.character({{ indicator }}))

        areadata <- areadata %>%
                select({{ indicator }}, areavalue)
        mean <- mean %>%
                rename(England = regionalvalue)

        if (!is.na(comparator_area_code))
                mean <- merge(parentdata, mean,
                              by = rlang::as_name(rlang::enquo(indicator)),
                              all =TRUE)
        dfannotatepoints <- merge(mean, areadata,
                                  by = rlang::as_name(rlang::enquo(indicator)),
                                  all =TRUE) %>%
                merge(timeperiod,
                      by = rlang::as_name(rlang::enquo(indicator)),
                      all = TRUE) %>%
                mutate(England = as.character(England),
                       England = case_when(
                        is.na(as.numeric(England)) ~ England,
                        TRUE ~ format(round2(as.numeric(England), dps), nsmall = 1)),
                       areavalue = as.character(areavalue),
                       areavalue = case_when(
                               is.na(as.numeric(areavalue)) ~ areavalue,
                               TRUE ~ format(round2(as.numeric(areavalue), dps), nsmall = 1)))
        if (!is.na(comparator_area_code))
                dfannotatepoints <- dfannotatepoints %>%
                mutate(regionalvalue = as.character(regionalvalue),
                       regionalvalue = case_when(
                        is.na(as.numeric(regionalvalue)) ~ regionalvalue,
                        TRUE ~ format(round2(as.numeric(regionalvalue), dps), nsmall = 1)))

        dfgraphfinal$bars <- merge(dfgraphfinal$bars,
                                       dfannotate,
                                       by = c(rlang::as_name(rlang::enquo(indicator)), "GraphPoint"),
                                       all.x =TRUE)
        dfgraphfinal$points <- merge(dfgraphfinal$points,
                                         dfannotatepoints,
                                         by = rlang::as_name(rlang::enquo(indicator)),
                                         all.x =TRUE) %>%
                rename({{ significance }} := significance)
        if (is.na(comparator_area_code)) dfgraphfinal$points$region <- NULL
        return(dfgraphfinal)
}

#' Proper rounding of values
#' @param val numeric value to round
#' @param dps numeric, number of decimal places
#' @details function taken from this link (\url{https://stackoverflow.com/questions/12688717/round-up-from-5})
round2 <- function(val, dps) {
        posneg = sign(val)
        z = abs(val)*10^dps
        z = z + 0.5
        z = trunc(z)
        z = z/10^dps
        z*posneg
}
