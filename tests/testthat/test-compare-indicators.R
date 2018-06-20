context("compare_indicators")

library(tidyr)
df_ci <- df %>%
        filter(IndicatorName %in% c("Indicator 1", "Indicator 3")) %>%
        select(IndicatorName, AreaCode, Value) %>%
        spread(IndicatorName, Value) %>%
        rename(Ind1 = `Indicator 1`,
               Ind3 = `Indicator 3`)
p <- compare_indicators(df_ci,
                        x = Ind1,
                        y = Ind3,
                        xlab = "Indicator 1 label",
                        ylab = "Indicator 3 label",
                        highlight_area = c("C001", "AC172"),
                        area = AreaCode,
                        add_R2 = TRUE)

# Visual tests ------------------------------------------------------------

test_that("compare indicators draws correctly", {
        vdiffr::expect_doppelganger("compare-indicators",
                                    p
        )
})
