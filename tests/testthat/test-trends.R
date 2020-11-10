
# Visual tests ------------------------------------------------------------

test_that("trends with fill draws correctly", {
        df_trend <- create_test_data() %>%
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
        vdiffr::expect_doppelganger("with fill trends",
                                    with_fill_p,
                                    path = "")
})

test_that("trends without fill draws correctly", {
        df_trend <- create_test_data() %>%
                arrange(IndicatorName) %>%
                mutate(Timeperiod = rep(c("2011", "2012", "2013", "2014", "2015", "2016"),
                                        each = 111))
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

        vdiffr::expect_doppelganger("without fill trends",
                                    without_fill_p,
                                    path = "")
})
