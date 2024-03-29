# Visual tests ------------------------------------------------------------

test_that("compare indicators example draws correctly", {
        df <- create_test_data()

        df_ci <- df %>%
                filter(IndicatorName %in% c("Indicator 1", "Indicator 3")) %>%
                select(IndicatorName, AreaCode, Value) %>%
                tidyr::pivot_wider(names_from = IndicatorName,
                                   values_from = Value) %>%
                rename(Ind1 = `Indicator 1`,
                       Ind3 = `Indicator 3`) %>%
                mutate(Ind2 = runif(nrow(.), min = Ind1 * 0.5, max = Ind1 * 1.5))
        p <- compare_indicators(df_ci,
                                x = Ind1,
                                y = Ind3,
                                xlab = "Indicator 1 label",
                                ylab = "Indicator 3 label",
                                highlight_area = c("C001", "AC172"),
                                area = AreaCode,
                                add_R2 = TRUE)
        vdiffr::expect_doppelganger("example compare indicators",
                                    fig = p)
})


test_that("trends example draws correctly", {
        df <- create_test_data()

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
        vdiffr::expect_doppelganger("example trends",
                                    fig = p)
})


test_that("population example draws correctly", {
        set.seed(1234)
        agelevels <- c("0-4", "5-9","10-14","15-19",
                       "20-24","25-29","30-34",
                       "35-39","40-44","45-49",
                       "50-54","55-59","60-64",
                       "65-69","70-74","75-79",
                       "80-84","85-89","90+")
        areas <- c("Area 1", "Area 2", "Area 3")
        pops <- data.frame(Age = factor(rep(agelevels, length(areas) * 2),
                                        levels = agelevels),
                           Value = rep(sample(1000:3000, length(agelevels), replace = TRUE),
                                       length(areas) * 2),
                           Sex = rep(rep(c("Male", "Female"),
                                         each = length(agelevels)), length(areas)),
                           AreaName = rep(areas, each = length(agelevels) * 2))

        p <- population(pops,
                        value = Value,
                        sex = Sex,
                        age = Age,
                        area = AreaName,
                        area_name = "Area 1",
                        comparator_1 = "Area 3",
                        comparator_2 = "Area 2",
                        title = "Age Profile",
                        subtitle = "2015/16",
                        xlab = "% of total population")
        vdiffr::expect_doppelganger("example population",
                                    fig = p)
})


test_that("box plot example draws correctly", {
        df <- create_test_data()

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
        vdiffr::expect_doppelganger("example box plot",
                                    fig = p)
})



test_that("map example draws correctly", {
        skip_on_cran()
        ons_api <- "https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Counties_and_Unitary_Authorities_December_2021_EN_BUC/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson"

        p <- fingertipscharts::map(mapdata,
                                   ons_api = ons_api,
                                   area_code = AreaCode,
                                   fill = Significance,
                                   title = "Map example",
                                   subtitle = "An indicator for Upper Tier Local Authorities England",
                                   copyright_year = 2019)
        vdiffr::expect_doppelganger("example map",
                                    fig = p)
})


test_that("area profiles example with domains", {
        ## An example with domains and non-default indicator ordering

        df <- create_test_data()
        label_order <- c(1, 2, 4, 3, 6, 5)
        df <- df %>%
                mutate(IndicatorName = factor(IndicatorName,
                                              levels = paste("Indicator", label_order)))

        p <- area_profiles(df,
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
                           domain = Domain
        )
        vdiffr::expect_doppelganger("example area profiles with domains",
                                    fig = p)
})

test_that("area profiles example different decimal places and no trend", {
        df <- create_test_data() %>%
                mutate(Value = case_when(
                        grepl("2$|4$|6$", IndicatorName) ~ round(Value,1),
                        TRUE ~ round(Value, 0)))
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
                                indicator_label_nudgex = -0.1,
                                show_dividers = "outer",
                                header_positions = c(-1, -0.7, -0.44, -0.35, -0.25,
                                                     -0.15, -0.05, 1.08),
                                header_labels = c("Indicator", "",
                                                  "Time\nperiod",
                                                  "Local\ncount","Local\nvalue",
                                                  "England\nvalue",
                                                  "Worst/\nLowest","Best/\nHighest"),
                                dps = NA)
        vdiffr::expect_doppelganger("ap example with different dps and no trend",
                                    fig = full_p)
})


