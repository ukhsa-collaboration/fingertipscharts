#' Create test data
#'
#' Create dataset to be used in tests (useful for demo purposes too)
#' @return a dummy data.frame of data
#' @import dplyr
#' @examples
#' df <- create_test_data()
#' @export
create_test_data <- function() {

        if (R.Version()$major >= 3 & R.Version()$minor >= 6.0) {
                suppressWarnings(set.seed(42, sample.kind = "Rounding"))
        } else {
                set.seed(42)
        }

        df_local <- data.frame(AreaCode = rep(paste0("AC", 100:199), 6),
                               ParentAreaCode = rep(rep(paste0("PAC", 10:19), 6), each = 10),
                               IndicatorName = rep(paste("Indicator", 1:6), each = 100),
                               Timeperiod = rep(c("2016", "2015/16", "2014 - 2016",
                                                  "Aug 2014", "2017 Q1", "2011 - 2016"), each = 100),
                               Polarity = rep(c("Not applicable      ", "RAG - Low is good   ",
                                                "RAG - Low is good   ", "RAG - High is good  ",
                                                "BOB - Blue orange blue", "RAG - High is good  "), each = 100),
                               AreaType = "Local",
                               Count = sample(20:500, 600, replace = T),
                               Denominator = sample(500:1000, 600, replace = T)) %>%
                mutate(Value = 100 * Count / Denominator,
                       LCI = Value * 0.95,
                       UCI = Value * 1.05) %>%
                mutate_if(is.factor, as.character)

        df_parent <- df_local %>%
                group_by(IndicatorName, ParentAreaCode, Timeperiod, Polarity) %>%
                summarise(Count = sum(Count),
                          Denominator = sum(Denominator)) %>%
                ungroup() %>%
                mutate(Value = 100 * Count / Denominator,
                       LCI = Value * 0.95,
                       UCI = Value * 1.05,
                       AreaType = "Parent",
                       AreaCode = ParentAreaCode,
                       ParentAreaCode = "C001")
        df_country <- df_parent %>%
                group_by(IndicatorName, ParentAreaCode, Timeperiod, Polarity) %>%
                summarise(Count = sum(Count),
                          Denominator = sum(Denominator)) %>%
                ungroup() %>%
                mutate(Value = 100 * Count / Denominator,
                       LCI = Value * 0.95,
                       UCI = Value * 1.05,
                       AreaType = "Country",
                       AreaCode = ParentAreaCode,
                       ParentAreaCode = NA,
                       Significance = "Not compared")

        country_values <- df_country %>%
                select(IndicatorName, EngVal = Value)

        df_local <- df_local %>%
                left_join(country_values, by = "IndicatorName") %>%
                mutate(Significance = ifelse(grepl("^Not", Polarity), "None",
                                             ifelse(grepl("^RAG", Polarity),
                                                    ifelse(grepl("Low", Polarity),
                                                           ifelse(UCI < EngVal, "Better",
                                                                  ifelse(LCI > EngVal, "Worse",
                                                                         "Similar")),
                                                           ifelse(UCI < EngVal, "Worse",
                                                                  ifelse(LCI > EngVal, "Better",
                                                                         "Similar"))),
                                                    ifelse(UCI < EngVal, "Lower",
                                                           ifelse(LCI > EngVal, "Higher",
                                                                  "Similar"))))) %>%
                select(-EngVal)
        df_parent <- df_parent %>%
                left_join(country_values, by = "IndicatorName") %>%
                mutate(Significance = ifelse(grepl("^Not", Polarity), "None",
                                             ifelse(grepl("^RAG", Polarity),
                                                    ifelse(grepl("Low", Polarity),
                                                           ifelse(UCI < EngVal, "Better",
                                                                  ifelse(LCI > EngVal, "Worse",
                                                                         "Similar")),
                                                           ifelse(UCI < EngVal, "Worse",
                                                                  ifelse(LCI > EngVal, "Better",
                                                                         "Similar"))),
                                                    ifelse(UCI < EngVal, "Lower",
                                                           ifelse(LCI > EngVal, "Higher",
                                                                  "Similar"))))) %>%
                select(-EngVal)

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
                        grepl("1$", IndicatorName) ~ "Dom 1",
                        grepl("2$", IndicatorName) ~ "Dom 2",
                        TRUE ~ "Dom 3"),
                       Trend = sample(trend_categories, n(), replace = TRUE))
        return(df)

}
