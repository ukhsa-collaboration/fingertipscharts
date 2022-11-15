#' Create test data
#'
#' Create dataset to be used in tests (useful for demo purposes too)
#' @return a dummy data.frame of data
#' @import dplyr
#' @importFrom tidyselect where
#' @examples
#' df <- create_test_data()
#' @export
create_test_data <- function() {

        if (R.Version()$major >= 3 & R.Version()$minor >= 6.0) {
                suppressWarnings(set.seed(42, sample.kind = "Rounding"))
        } else {
                set.seed(42)
        }

        df_local <- data.frame(
            AreaCode = rep(paste0("AC", 100:199), 6),
            ParentAreaCode = rep(
                rep(
                    paste0("PAC", 10:19), 6),
                each = 10),
            IndicatorName = rep(
                paste("Indicator", 1:6),
                each = 100),
            Timeperiod = rep(c("2016", "2015/16", "2014 - 2016",
                               "Aug 2014", "2017 Q1", "2011 - 2016"),
                             each = 100),
            Polarity = rep(c("Not applicable      ", "RAG - Low is good   ",
                             "RAG - Low is good   ", "RAG - High is good  ",
                             "BOB - Blue orange blue", "RAG - High is good  "),
                           each = 100),
            AreaType = "Local",
            Count = sample(20:500, 600,
                           replace = T),
            Denominator = sample(500:1000, 600,
                                 replace = T)
        ) %>%
                mutate(
                    Value = 100 * .data$Count / .data$Denominator,
                    LCI = .data$Value * 0.95,
                    UCI = .data$Value * 1.05
                ) %>%
                mutate(across(tidyselect::where(is.factor),
                              as.character))

        df_parent <- df_local %>%
                group_by(.data$IndicatorName,
                         .data$ParentAreaCode,
                         .data$Timeperiod,
                         .data$Polarity) %>%
                summarise(Count = sum(.data$Count),
                          Denominator = sum(.data$Denominator),
                          .groups = "drop") %>%
                mutate(
                    Value = 100 * .data$Count / .data$Denominator,
                    LCI = .data$Value * 0.95,
                    UCI = .data$Value * 1.05,
                    AreaType = "Parent",
                    AreaCode = .data$ParentAreaCode,
                    ParentAreaCode = "C001"
                )

        df_country <- df_parent %>%
                group_by(.data$IndicatorName,
                         .data$ParentAreaCode,
                         .data$Timeperiod,
                         .data$Polarity) %>%
                summarise(Count = sum(.data$Count),
                          Denominator = sum(.data$Denominator),
                          .groups = "drop") %>%
                mutate(
                    Value = 100 * .data$Count / .data$Denominator,
                    LCI = .data$Value * 0.95,
                    UCI = .data$Value * 1.05,
                    AreaType = "Country",
                    AreaCode = .data$ParentAreaCode,
                    ParentAreaCode = NA,
                    Significance = "Not compared"
                )

        country_values <- df_country %>%
                select("IndicatorName",
                       EngVal = "Value")

        df_local <- df_local %>%
                left_join(country_values, by = "IndicatorName") %>%
                mutate(Significance = case_when(
                    grepl("^Not", .data$Polarity) ~ "None",
                    grepl("^RAG", .data$Polarity) ~ case_when(
                        (grepl("Low", .data$Polarity) &
                            .data$UCI < .data$EngVal) ~ "Better",
                        (grepl("Low", .data$Polarity) &
                             .data$LCI > .data$EngVal) ~ "Worse",
                        (grepl("High", .data$Polarity) &
                             .data$UCI < .data$EngVal) ~ "Worse",
                        (grepl("High", .data$Polarity) &
                             .data$LCI > .data$EngVal) ~ "Better",
                        TRUE ~ "Similar"),
                    .data$UCI < .data$EngVal ~ "Lower",
                    .data$LCI > .data$EngVal ~ "Higher",
                    TRUE ~ "Similar")) %>%
                select(!c("EngVal"))
        df_parent <- df_parent %>%
                left_join(country_values, by = "IndicatorName") %>%
                mutate(Significance = ifelse(grepl("^Not", .data$Polarity), "None",
                                             ifelse(grepl("^RAG", .data$Polarity),
                                                    ifelse(grepl("Low", .data$Polarity),
                                                           ifelse(.data$UCI < .data$EngVal, "Better",
                                                                  ifelse(.data$LCI > .data$EngVal, "Worse",
                                                                         "Similar")),
                                                           ifelse(.data$UCI < .data$EngVal, "Worse",
                                                                  ifelse(.data$LCI > .data$EngVal, "Better",
                                                                         "Similar"))),
                                                    ifelse(.data$UCI < .data$EngVal, "Lower",
                                                           ifelse(.data$LCI > .data$EngVal, "Higher",
                                                                  "Similar"))))) %>%
                select(!c("EngVal"))

        trend_categories <- c("Increasing and getting better",
                              "Increasing and getting worse",
                              "Decreasing and getting better",
                              "Decreasing and getting worse",
                              "No significant change",
                              "Could not be calculated",
                              "Increasing",
                              "Decreasing")
        df <- bind_rows(df_local,
                        df_parent,
                        df_country) %>%
                mutate(Domain = case_when(
                        grepl("1$", .data$IndicatorName) ~ "Dom 1",
                        grepl("2$", .data$IndicatorName) ~ "Dom 2",
                        TRUE ~ "Dom 3"),
                       Trend = sample(trend_categories, n(), replace = TRUE))
        return(df)

}
