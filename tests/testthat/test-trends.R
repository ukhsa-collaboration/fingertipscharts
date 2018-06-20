context("trends")

df_trend <- df %>%
        arrange(IndicatorName) %>%
        mutate(Timeperiod = rep(c("2011", "2012", "2013", "2014", "2015", "2016"),
                                each = 111))
p <- trends(df_trend,
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

# Visual tests ------------------------------------------------------------

test_that("trends draws correctly", {
        vdiffr::expect_doppelganger("trends",
                                    p
        )
})
