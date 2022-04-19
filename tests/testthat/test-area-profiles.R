test_that("Error for duplicate values in spine chart works", {
        df <- create_test_data()

        df2 <- df %>%
                mutate(IndicatorName = factor(IndicatorName,
                                              levels = rev(unique(df$IndicatorName))),
                       Value = ifelse(IndicatorName == "Indicator 5", NA, Value + 1))

        df_dps <- df %>%
                mutate(Value = case_when(
                        grepl("2$|4$|6$", IndicatorName) ~ round(Value,1),
                        TRUE ~ round(Value, 0)))

        df_error <- rbind(df, df2)
        expect_error(area_profiles(df_error,
                                   value = Value,
                                   count = Count,
                                   area_code = AreaCode,
                                   local_area_code = "AC122",
                                   indicator = IndicatorName,
                                   timeperiod = Timeperiod,
                                   trend = Trend,
                                   polarity = Polarity,
                                   significance = Significance,
                                   area_type = AreaType,
                                   median_line_area_code = "C001",
                                   comparator_area_code = "PAC12",
                                   datatable = TRUE,
                                   show_dividers = "outer"
        ),
                     "Some areas have multiple values for an indicator\\. An example is AC100 for the indicator Indicator 1")
})

test_that("error for incorrect area_code supplied for local_area_code", {
        df <- create_test_data()

        expect_error(area_profiles(df,
                                   value = Value,
                                   count = Count,
                                   area_code = AreaCode,
                                   local_area_code = "BB2",
                                   indicator = IndicatorName,
                                   timeperiod = Timeperiod,
                                   trend = Trend,
                                   polarity = Polarity,
                                   significance = Significance,
                                   area_type = AreaType,
                                   median_line_area_code = "C001"),
                     "BB2 not in area_code field provided")
})

test_that("error for incorrect area_code supplied for median_line_area_code", {
        df <- create_test_data()
        expect_error(area_profiles(df,
                                   value = Value,
                                   count = Count,
                                   area_code = AreaCode,
                                   local_area_code = "AC122",
                                   indicator = IndicatorName,
                                   timeperiod = Timeperiod,
                                   trend = Trend,
                                   polarity = Polarity,
                                   significance = Significance,
                                   area_type = AreaType,
                                   median_line_area_code = "AB1"),
                     "AB1 not in area_code field provided")
})

test_that("error for incorrect area_code supplied for comparator_area_code", {
        df <- create_test_data()
        expect_error(area_profiles(df,
                                   value = Value,
                                   count = Count,
                                   area_code = AreaCode,
                                   local_area_code = "AC122",
                                   indicator = IndicatorName,
                                   timeperiod = Timeperiod,
                                   trend = Trend,
                                   polarity = Polarity,
                                   significance = Significance,
                                   area_type = AreaType,
                                   median_line_area_code = "C001",
                                   comparator_area_code = "CB1"),
                     "CB1 not in area_code field provided")
})

test_that("error for incorrect lengthof header_labels", {
        df <- create_test_data()
        expect_error(full_no_dt_p <- area_profiles(df,
                                                   value = Value,
                                                   count = Count,
                                                   area_code = AreaCode,
                                                   local_area_code = "AC122",
                                                   indicator = IndicatorName,
                                                   timeperiod = Timeperiod,
                                                   trend = Trend,
                                                   polarity = Polarity,
                                                   significance = Significance,
                                                   area_type = AreaType,
                                                   median_line_area_code = "C001",
                                                   comparator_area_code = "PAC12",
                                                   header_labels = c("Indicator", "Time\nperiod",
                                                                     "Local\ncount", "Local\nvalue",
                                                                     "England\nvalue", "Worst/\nLowest",
                                                                     "Best/\nHighest"),
                                                   bar_width = 0.68,
                                                   datatable = FALSE),
                     "header_labels argument must have a length of 8")
})
test_that("error for incorrect length of header_positions", {
        df <- create_test_data()
        expect_error(full_no_dt_p <- area_profiles(df,
                                                   value = Value,
                                                   count = Count,
                                                   area_code = AreaCode,
                                                   local_area_code = "AC122",
                                                   indicator = IndicatorName,
                                                   timeperiod = Timeperiod,
                                                   trend = Trend,
                                                   polarity = Polarity,
                                                   significance = Significance,
                                                   area_type = AreaType,
                                                   median_line_area_code = "C001",
                                                   comparator_area_code = "PAC12",
                                                   header_positions  = c(-1.83, -0.53,
                                                                         -0.35, -0.25,
                                                                         -0.15, -0.05, 1.05),
                                                   bar_width = 0.68,
                                                   datatable = FALSE),
                     "header_positions argument must have a length of 8")
})

test_that("warning messages work for area_profiles", {
        df <- create_test_data()

        df_dps <- df %>%
                mutate(Value = case_when(
                        grepl("2$|4$|6$", IndicatorName) ~ round(Value,1),
                        TRUE ~ round(Value, 0)))

        expect_warning(area_profiles(df_dps,
                                     value = Value,
                                     count = Count,
                                     area_code = AreaCode,
                                     local_area_code = "AC122",
                                     indicator = IndicatorName,
                                     timeperiod = Timeperiod,
                                     trend = Trend,
                                     polarity = Polarity,
                                     significance = Significance,
                                     area_type = AreaType,
                                     median_line_area_code = "C001",
                                     comparator_area_code = "PAC12",
                                     datatable = TRUE,
                                     relative_domain_text_size = 0.75,
                                     relative_text_size = 1.2,
                                     bar_width = 0.68,
                                     indicator_label_nudgex = -0.1,
                                     show_dividers = "outer",
                                     header_positions = c(-1, -0.7, -0.44, -0.35, -0.25,
                                                          -0.15, -0.05, 0.9),
                                     dps = NA),
                       "Some bars may not display if the final value of the header_positions argument is less than 1")

})

test_that("area_profiles preprocessing function works", {
        df <- create_test_data()

        df_preprocess1 <- df %>%
                filter(IndicatorName == "Indicator 1")
        df_preprocess <- df_preprocess1 %>%
                mutate(Timeperiod = "2015") %>%
                rbind(df_preprocess1) %>%
                mutate(TimeperiodSortable = as.numeric(Timeperiod))

        df_preprocess_test <- spine_preprocess(df_preprocess,
                                               IndicatorName,
                                               TimeperiodSortable)
        expect_equal(unique(df_preprocess_test$Timeperiod), "2016")
})


# Visual tests ------------------------------------------------------------

test_that("area profiles with all main inputs draws correctly", {
        df <- create_test_data()

        full_p <- area_profiles(df,
                                value = Value,
                                count = Count,
                                area_code = AreaCode,
                                local_area_code = "AC122",
                                indicator = IndicatorName,
                                timeperiod = Timeperiod,
                                trend = Trend,
                                polarity = Polarity,
                                significance = Significance,
                                area_type = AreaType,
                                median_line_area_code = "C001",
                                comparator_area_code = "PAC12",
                                datatable = TRUE,
                                relative_domain_text_size = 0.75,
                                relative_text_size = 1.2,
                                bar_width = 0.68,
                                indicator_label_nudgex = -0.1,
                                show_dividers = "outer",
                                header_positions = c(-1, -0.7, -0.44, -0.35, -0.25,
                                                     -0.15, -0.05, 1.08)
        )
        vdiffr::expect_doppelganger("ap with all main inputs",
                                    fig = full_p)
})

test_that("area profiles without trend draws correctly", {
        df <- create_test_data()

        full_p_no_trend <- area_profiles(df,
                                         value = Value,
                                         count = Count,
                                         area_code = AreaCode,
                                         local_area_code = "AC100",
                                         indicator = IndicatorName,
                                         timeperiod = Timeperiod,
                                         polarity = Polarity,
                                         significance = Significance,
                                         area_type = AreaType,
                                         median_line_area_code = "C001",
                                         comparator_area_code = "PAC12",
                                         datatable = TRUE,
                                         relative_domain_text_size = 0.75,
                                         relative_text_size = 1.2,
                                         bar_width = 0.68,
                                         indicator_label_nudgex = -0.1,
                                         show_dividers = "outer",
                                         header_positions = c(-0.7, -0.7, -0.44, -0.35, -0.25,
                                                              -0.15, -0.05, 1.08),
                                         header_labels = c("Indicator", "",
                                                           "Time\nperiod", "Local\ncount",
                                                           "Local\nvalue", "England\nvalue",
                                                           "Worst/\nLowest", "Best/\nHighest")
        )
        vdiffr::expect_doppelganger("ap without trend",
                                    fig = full_p_no_trend)
})

test_that("area profiles using factor indicator variable draws correctly", {
        df <- create_test_data()
        df2 <- create_test_data() %>%
                mutate(IndicatorName = factor(IndicatorName,
                                              levels = rev(unique(df$IndicatorName))))

        full_with_factor_indicators_p <- area_profiles(df2,
                                                       value = Value,
                                                       count = Count,
                                                       area_code = AreaCode,
                                                       local_area_code = "AC100",
                                                       indicator = IndicatorName,
                                                       timeperiod = Timeperiod,
                                                       trend = Trend,
                                                       polarity = Polarity,
                                                       significance = Significance,
                                                       area_type = AreaType,
                                                       median_line_area_code = "C001",
                                                       comparator_area_code = "PAC12",
                                                       datatable = TRUE,
                                                       relative_domain_text_size = 0.75,
                                                       relative_text_size = 1.2,
                                                       bar_width = 0.68,
                                                       indicator_label_nudgex = -0.1,
                                                       show_dividers = "outer",
                                                       header_positions = c(-1, -0.7, -0.53, -0.35, -0.25,
                                                                            -0.15, -0.05, 1.08))

        vdiffr::expect_doppelganger("ap with factor",
                                    fig = full_with_factor_indicators_p)
})

test_that("area profiles without data table draws correctly", {
        df <- create_test_data()

        full_no_dt_p <- area_profiles(df,
                                      value = Value,
                                      count = Count,
                                      area_code = AreaCode,
                                      local_area_code = "AC122",
                                      indicator = IndicatorName,
                                      timeperiod = Timeperiod,
                                      trend = Trend,
                                      polarity = Polarity,
                                      significance = Significance,
                                      area_type = AreaType,
                                      median_line_area_code = "C001",
                                      comparator_area_code = "PAC12",
                                      bar_width = 0.68,
                                      datatable = FALSE)

        vdiffr::expect_doppelganger("ap without data table",
                                    fig = full_no_dt_p)
})

test_that("area profiles with colour modification draws correctly", {
        df <- create_test_data()

        full_no_dt_p_modified_colours <- area_profiles(df,
                                                       value = Value,
                                                       count = Count,
                                                       area_code = AreaCode,
                                                       local_area_code = "AC122",
                                                       indicator = IndicatorName,
                                                       timeperiod = Timeperiod,
                                                       trend = Trend,
                                                       polarity = Polarity,
                                                       significance = Significance,
                                                       area_type = AreaType,
                                                       cols = c("Worse" = "purple"),
                                                       median_line_area_code = "C001",
                                                       comparator_area_code = "PAC12",
                                                       bar_width = 0.68,
                                                       datatable = FALSE)
        vdiffr::expect_doppelganger("ap with modified colours",
                                    fig = full_no_dt_p_modified_colours)
})

test_that("area profiles with domains draws correctly", {
        df <- create_test_data()

        full_with_domains_p <- area_profiles(df,
                                             value = Value,
                                             count = Count,
                                             area_code = AreaCode,
                                             local_area_code = "AC122",
                                             indicator = IndicatorName,
                                             timeperiod = Timeperiod,
                                             trend = Trend,
                                             polarity = Polarity,
                                             significance = Significance,
                                             area_type = AreaType,
                                             median_line_area_code = "C001",
                                             comparator_area_code = "PAC12",
                                             datatable = TRUE,
                                             relative_domain_text_size = 0.75,
                                             relative_text_size = 1.2,
                                             bar_width = 0.68,
                                             indicator_label_nudgex = -0.1,
                                             show_dividers = "outer",
                                             header_positions = c(-1, -0.7, -0.53, -0.35, -0.25,
                                                                  -0.15, -0.05, 1.05),
                                             domain = Domain)
        vdiffr::expect_doppelganger("ap with domains",
                                    fig = full_with_domains_p)
})

test_that("area profiles with dividers between indicators draws correctly", {
        df <- create_test_data()

        full_all_dividers_p <- area_profiles(df,
                                             value = Value,
                                             count = Count,
                                             area_code = AreaCode,
                                             local_area_code = "AC122",
                                             indicator = IndicatorName,
                                             timeperiod = Timeperiod,
                                             trend = Trend,
                                             polarity = Polarity,
                                             significance = Significance,
                                             area_type = AreaType,
                                             median_line_area_code = "C001",
                                             datatable = TRUE,
                                             relative_domain_text_size = 0.75,
                                             relative_text_size = 1.2,
                                             bar_width = 0.68,
                                             indicator_label_nudgex = -0.1,
                                             show_dividers = "all",
                                             header_positions = c(-1, -0.7, -0.53, -0.35, -0.25,
                                                                  -0.15, -0.05, 1.05))

        vdiffr::expect_doppelganger("ap with dividers",
                                    fig = full_all_dividers_p)
})


test_that("area profiles with different decimal places draws correctly", {
        df <- create_test_data()

        df_dps <- df %>%
                mutate(Value = case_when(
                        grepl("2$|4$|6$", IndicatorName) ~ round(Value,1),
                        TRUE ~ round(Value, 0)))

        dps_p <- area_profiles(df_dps,
                               value = Value,
                               count = Count,
                               area_code = AreaCode,
                               local_area_code = "AC122",
                               indicator = IndicatorName,
                               timeperiod = Timeperiod,
                               trend = Trend,
                               polarity = Polarity,
                               significance = Significance,
                               area_type = AreaType,
                               median_line_area_code = "C001",
                               comparator_area_code = "PAC12",
                               datatable = TRUE,
                               relative_domain_text_size = 0.75,
                               relative_text_size = 1.2,
                               bar_width = 0.68,
                               indicator_label_nudgex = -0.1,
                               show_dividers = "outer",
                               header_positions = c(-1, -0.7, -0.44, -0.35, -0.25,
                                                    -0.15, -0.05, 1.08),
                               dps = NA)
        vdiffr::expect_doppelganger("ap with different decimal places",
                                    fig = dps_p)
})
