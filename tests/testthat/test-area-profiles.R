context("area-profiles")
library(dplyr)
library(fingertipscharts)
df <- create_test_data()

df_preprocess1 <- df %>%
        filter(IndicatorName == "Indicator 1")
df_preprocess <- df_preprocess1 %>%
        mutate(Timeperiod = "2015") %>%
        rbind(df_preprocess1) %>%
        mutate(TimeperiodSortable = as.numeric(Timeperiod))

df2 <- df %>%
        mutate(IndicatorName = factor(IndicatorName,
                                      levels = rev(unique(df$IndicatorName))),
               Value = ifelse(IndicatorName == "Indicator 5", NA, Value + 1))

df_error <- rbind(df, df2)

full_p <- area_profiles(df,
                        value = Value,
                        count = Count,
                        area_code = AreaCode,
                        local_area_code = "AC122",
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
                        indicator_label_nudgex = -0.5,
                        show_dividers = "outer",
                        header_positions = c(-0.7, -0.53, -0.35, -0.25,
                                             -0.15, -0.05, 1.05)
)

full_with_factor_indicators_p <- area_profiles(df2,
                                               value = Value,
                                               count = Count,
                                               area_code = AreaCode,
                                               local_area_code = "AC122",
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
                                               indicator_label_nudgex = -0.5,
                                               show_dividers = "outer",
                                               header_positions = c(-0.7, -0.53, -0.35, -0.25,
                                                                    -0.15, -0.05, 1.05)
)


full_no_dt_p <- area_profiles(df,
                              value = Value,
                              count = Count,
                              area_code = AreaCode,
                              local_area_code = "AC122",
                              indicator = IndicatorName,
                              timeperiod = Timeperiod,
                              polarity = Polarity,
                              significance = Significance,
                              area_type = AreaType,
                              median_line_area_code = "C001",
                              comparator_area_code = "PAC12",
                              bar_width = 0.68,
                              datatable = FALSE)

full_with_domains_p <- area_profiles(df,
                                     value = Value,
                                     count = Count,
                                     area_code = AreaCode,
                                     local_area_code = "AC122",
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
                                     indicator_label_nudgex = -0.5,
                                     show_dividers = "outer",
                                     header_positions = c(-0.7, -0.53, -0.35, -0.25,
                                                          -0.15, -0.05, 1.05),
                                     domain = Domain
)

full_all_dividers_p <- area_profiles(df,
                                     value = Value,
                                     count = Count,
                                     area_code = AreaCode,
                                     local_area_code = "AC122",
                                     indicator = IndicatorName,
                                     timeperiod = Timeperiod,
                                     polarity = Polarity,
                                     significance = Significance,
                                     area_type = AreaType,
                                     median_line_area_code = "C001",
                                     datatable = TRUE,
                                     relative_domain_text_size = 0.75,
                                     relative_text_size = 1.2,
                                     bar_width = 0.68,
                                     indicator_label_nudgex = -0.5,
                                     show_dividers = "all",
                                     header_positions = c(-0.7, -0.53, -0.35, -0.25,
                                                          -0.15, -0.05, 1.05)
)

test_that("Error for duplicate values in spine chart works", {
        expect_error(area_profiles(df_error,
                                   value = Value,
                                   count = Count,
                                   area_code = AreaCode,
                                   local_area_code = "AC122",
                                   indicator = IndicatorName,
                                   timeperiod = Timeperiod,
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



# Visual tests ------------------------------------------------------------

test_that("full area profiles draws correctly", {
        vdiffr::expect_doppelganger("full area profiles",
                                    full_p
        )
})

test_that("full area profiles with factor indicator name draws correctly", {
        vdiffr::expect_doppelganger("full with factor area profiles",
                                    full_with_factor_indicators_p
        )
})

test_that("area profiles with no dt draws correctly", {
        vdiffr::expect_doppelganger("no dt area profiles",
                                    full_no_dt_p
        )
})



test_that("area profiles with domains draws correctly", {
        vdiffr::expect_doppelganger("domains included area profiles",
                                    full_with_domains_p
        )
})

test_that("full area profiles all dividers draws correctly", {
        vdiffr::expect_doppelganger("full area all dividers profiles",
                                    full_all_dividers_p
        )
})

test_that("error messages work for area_profiles", {
        expect_error(area_profiles(df,
                                   value = Value,
                                   count = Count,
                                   area_code = AreaCode,
                                   local_area_code = "BB2",
                                   indicator = IndicatorName,
                                   timeperiod = Timeperiod,
                                   polarity = Polarity,
                                   significance = Significance,
                                   area_type = AreaType,
                                   median_line_area_code = "C001"),
                     "BB2 not in area_code field provided")
        expect_error(area_profiles(df,
                                   value = Value,
                                   count = Count,
                                   area_code = AreaCode,
                                   local_area_code = "AC122",
                                   indicator = IndicatorName,
                                   timeperiod = Timeperiod,
                                   polarity = Polarity,
                                   significance = Significance,
                                   area_type = AreaType,
                                   median_line_area_code = "AB1"),
                     "AB1 not in area_code field provided")
        expect_error(area_profiles(df,
                                   value = Value,
                                   count = Count,
                                   area_code = AreaCode,
                                   local_area_code = "AC122",
                                   indicator = IndicatorName,
                                   timeperiod = Timeperiod,
                                   polarity = Polarity,
                                   significance = Significance,
                                   area_type = AreaType,
                                   median_line_area_code = "C001",
                                   comparator_area_code = "CB1"),
                     "CB1 not in area_code field provided")
})

df_preprocess_test <- spine_preprocess(df_preprocess,
                                       IndicatorName,
                                       TimeperiodSortable)
test_that("area_profiles preprocessing function works", {
        expect_equal(unique(df_preprocess_test$Timeperiod), "2016")
})
