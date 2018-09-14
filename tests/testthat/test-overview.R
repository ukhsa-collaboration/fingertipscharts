context("overview")
library(fingertipscharts)
df <- create_test_data()

parent <- "PAC14"
top_names <- c("C001", parent)
df_over <- df %>%
    filter((AreaCode %in% top_names |
                    ParentAreaCode == parent)) %>%
    mutate(Value = round(Value, 1))
top_areas_p <- overview(df_over,
                        area = AreaCode,
                        indicator = IndicatorName,
                        value = Value,
                        fill = Significance,
                        timeperiod = Timeperiod,
                        top_areas = top_names,
                        wrap_length = 40,
                        value_label_size = 0.8)

no_top_areas_p <- overview(df_over,
                        area = AreaCode,
                        indicator = IndicatorName,
                        value = Value,
                        fill = Significance,
                        timeperiod = Timeperiod,
                        wrap_length = 40,
                        value_label_size = 0.8)


# Visual tests ------------------------------------------------------------

test_that("overview with top areas draws correctly", {
        vdiffr::expect_doppelganger("overview top areas",
                                    top_areas_p
        )
})

test_that("overview without top areas draws correctly", {
        vdiffr::expect_doppelganger("overview no top areas",
                                    no_top_areas_p
        )
})
