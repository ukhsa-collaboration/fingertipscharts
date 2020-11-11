# Visual tests ------------------------------------------------------------

test_that("overview with top_areas draws correctly", {
    parent <- "PAC14"
    top_names <- c("C001", parent)
    df_over <- create_test_data() %>%
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

    vdiffr::expect_doppelganger("overview with top_areas",
                                top_areas_p,
                                path = "")
})

test_that("overview without top_areas draws correctly", {
    parent <- "PAC14"
    top_names <- c("C001", parent)
    df_over <- create_test_data() %>%
        filter((AreaCode %in% top_names |
                    ParentAreaCode == parent)) %>%
        mutate(Value = round(Value, 1))
    no_top_areas_p <- overview(df_over,
                               area = AreaCode,
                               indicator = IndicatorName,
                               value = Value,
                               fill = Significance,
                               timeperiod = Timeperiod,
                               wrap_length = 40,
                               value_label_size = 0.8)
    vdiffr::expect_doppelganger("overview without top_areas",
                                no_top_areas_p,
                                path = "")
})
