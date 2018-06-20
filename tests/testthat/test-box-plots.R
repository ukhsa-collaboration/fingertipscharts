context("box-plots")

df_box <- df %>%
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

# Visual tests ------------------------------------------------------------

test_that("box plots draws correctly", {
        vdiffr::expect_doppelganger("box-plots",
                                    p
        )
})
