
# Visual tests ------------------------------------------------------------

test_that("box plots draws correctly", {
        df_box <- create_test_data() %>%
                filter(AreaType == "Local") %>%
                arrange(IndicatorName) %>%
                mutate(Timeperiod = rep(c("2011", "2012", "2013", "2014", "2015", "2016"),
                                        each = 100))
        p <- box_plots(df_box,
                       timeperiod = Timeperiod,
                       value = Value,
                       title = "Title of chart",
                       subtitle = "Boxplot over time",
                       ylab = "Proportion (%)")

        vdiffr::expect_doppelganger("box-plots",
                                    p,
                                    path = "")
})
