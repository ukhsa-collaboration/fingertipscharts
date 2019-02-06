context("compare-indicators-example")

library(fingertipsR)
library(tidyr)
library(dplyr)
library(fingertipscharts)

if(curl::has_internet()) {
        df_ex <- fingertips_data(c(90362, 90366)) %>%
                group_by(IndicatorID) %>%
                filter(Timeperiod == "2014 - 16",
                       Sex == "Female",
                       AreaType %in% c("County & UA", "England")) %>%
                ungroup() %>%
                select(IndicatorID, AreaName, Value) %>%
                mutate(IndicatorID = paste0("x", IndicatorID)) %>%
                spread(IndicatorID, Value)
        p <- compare_indicators(df_ex,
                                x = x90362,
                                y = x90366,
                                xlab = "Healthy life expectancy at birth",
                                ylab = "Life expectancy at birth",
                                highlight_area = c("England", "Dorset"),
                                area = AreaName)

        test_that("compare indicators example draws correctly", {
                vdiffr::expect_doppelganger("compare indicators example",
                                            p
                )
        })


        context("trends-example")

        df_ex <- fingertips_data(90366) %>%
                  filter(Sex == "Male")
        p <- trends(df_ex,
                    timeperiod = Timeperiod,
                    value = Value,
                    area = AreaName,
                    comparator = "England",
                    area_name = "Cambridgeshire",
                    fill = ComparedtoEnglandvalueorpercentiles,
                    title = "Life expectancy at birth",
                    subtitle = "Cambridgeshire compared to England",
                    xlab = "Year",
                    ylab = "Age (years)")

        test_that("trends example draws correctly", {
                vdiffr::expect_doppelganger("trends example",
                                            p
                )
        })


        context("population-example")
        agelevels <- c("0-4", "5-9","10-14","15-19",
                       "20-24","25-29","30-34",
                       "35-39","40-44","45-49",
                       "50-54","55-59","60-64",
                       "65-69","70-74","75-79",
                       "80-84","85-89","90+")
        pops <- fingertips_data(92708) %>%
                filter(Timeperiod == "2015" &
                               Sex %in% c("Male", "Female") &
                               Age != "All ages") %>%
                mutate(Age = gsub(" yrs","", Age),
                       Age = factor(Age,
                                    levels = agelevels)) %>%
                droplevels()
        p <- population(pops,
                        value = Value,
                        sex = Sex,
                        age = Age,
                        area = AreaName,
                        area_name = "Nottingham",
                        comparator_1 = "England",
                        comparator_2 = "East Midlands region",
                        title = "Age Profile",
                        subtitle = paste(unique(pops$IndicatorName), unique(pops$Timeperiod)),
                        xlab = "% of total population")
        test_that("population example draws correctly", {
                vdiffr::expect_doppelganger("population example",
                                            p
                )
        })


        context("box-plots-example")
        df_ex <- fingertips_data(90366) %>%
              filter(Sex == "Male" &
                     AreaType == "County & UA")
        p <- box_plots(df_ex,
                       timeperiod = Timeperiod,
                       value = Value,
                       title = "Life expectancy at birth",
                       subtitle = "Males in Uper Tier Local Authorities England",
                       ylab = "Age (years)")
        test_that("box plot example draws correctly", {
                vdiffr::expect_doppelganger("box plot example",
                                            p
                )
        })


        context("map-example")
        df_ex <- fingertips_data(90366) %>%
                filter(Sex == "Male" &
                               AreaType == "County & UA" &
                               Timeperiod == "2014 - 16")
        ons_api <- "https://opendata.arcgis.com/datasets/687f346f5023410ba86615655ff33ca9_4.geojson"

        p <- fingertipscharts::map(df_ex,
                                   ons_api = ons_api,
                                   area_code = AreaCode,
                                   fill = ComparedtoEnglandvalueorpercentiles,
                                   title = "Life expectancy at birth",
                                   subtitle = "Males in Upper Tier Local Authorities England",
                                   copyright_year = 2018)

        test_that("map example draws correctly", {
                vdiffr::expect_doppelganger("map example",
                                            p
                )
        })


        context("area-profiles-example")
        df_ex <- fingertips_data(DomainID = 1938133222, rank = TRUE) %>%
                filter(Timeperiod == "2016")
        p <- area_profiles(df_ex,
                           value = Value,
                           count = Count,
                           area_code = AreaCode,
                           local_area_code = "E06000020",
                           indicator = IndicatorName,
                           timeperiod = Timeperiod,
                           polarity = Polarity,
                           significance = ComparedtoEnglandvalueorpercentiles,
                           area_type = AreaType,
                           cols = "fingertips",
                           median_line_area_code = "E92000001",
                           comparator_area_code = "E12000005",
                           datatable = TRUE,
                           relative_domain_text_size = 0.75,
                           relative_text_size = 1.2,
                           bar_width = 0.68,
                           indicator_label_nudgex = -0.5)
        test_that("area profiles example draws correctly", {
                vdiffr::expect_doppelganger("area profiles example",
                                            p
                )
        })

}
