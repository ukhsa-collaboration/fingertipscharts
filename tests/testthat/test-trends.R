context("trends")
library(fingertipscharts)
df <- create_test_data()

df_trend <- df %>%
        arrange(IndicatorName) %>%
        mutate(Timeperiod = rep(c("2011", "2012", "2013", "2014", "2015", "2016"),
                                each = 111))
with_fill_p <- trends(df_trend,
                      timeperiod = Timeperiod,
                      value = Value,
                      area = AreaCode,
                      comparator = "C001",
                      area_name = "AC142",
                      fill = Significance,
                      lowerci = LCI,
                      upperci = UCI,
                      title = "Trend compared to country",
                      subtitle = "For area AC142",
                      xlab = "Year",
                      ylab = "Value (%)")

without_fill_p <- trends(df_trend,
                         timeperiod = Timeperiod,
                         value = Value,
                         area = AreaCode,
                         comparator = "C001",
                         area_name = "AC142",
                         lowerci = LCI,
                         upperci = UCI,
                         title = "Trend compared to country",
                         subtitle = "For area AC142",
                         xlab = "Year",
                         ylab = "Value (%)")



# Visual tests ------------------------------------------------------------

test_that("trends with fill draws correctly", {
        vdiffr::expect_doppelganger("with fill trends",
                                    with_fill_p
        )
})

test_that("trends without fill draws correctly", {
        vdiffr::expect_doppelganger("without fill trends",
                                    without_fill_p
        )
})
