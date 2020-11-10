
# Visual tests ------------------------------------------------------------

test_that("desc compare areas draws correctly", {

        parent <- "PAC11"
        top_names <- c("C001", parent)
        df_ca <- create_test_data() %>%
                filter(IndicatorName == "Indicator 3",
                       (AreaCode %in% top_names |
                                ParentAreaCode == parent))
        desc_p <- compare_areas(df_ca, AreaCode, Value,
                                fill = Significance,
                                lowerci = LCI,
                                upperci = UCI,
                                order = "desc",
                                top_areas = top_names,
                                title = "Compare the local areas")
        vdiffr::expect_doppelganger("desc compare areas",
                                    desc_p,
                                    path = "")
})

test_that("no top desc compare areas draws correctly", {
        parent <- "PAC11"
        top_names <- c("C001", parent)
        df_ca <- create_test_data() %>%
                filter(IndicatorName == "Indicator 3",
                       (AreaCode %in% top_names |
                                ParentAreaCode == parent))

        desc_no_top_p <- compare_areas(df_ca, AreaCode, Value,
                                       fill = Significance,
                                       lowerci = LCI,
                                       upperci = UCI,
                                       order = "desc",
                                       title = "Compare the local areas")

        vdiffr::expect_doppelganger("no top areas desc compare areas",
                                    desc_no_top_p,
                                    path = "")
})

test_that("asc compare areas draws correctly", {
        parent <- "PAC11"
        top_names <- c("C001", parent)
        df_ca <- create_test_data() %>%
                filter(IndicatorName == "Indicator 3",
                       (AreaCode %in% top_names |
                                ParentAreaCode == parent))

        asc_p <- compare_areas(df_ca, AreaCode, Value,
                               fill = Significance,
                               lowerci = LCI,
                               upperci = UCI,
                               order = "asc",
                               top_areas = top_names,
                               title = "Compare the local areas")
        vdiffr::expect_doppelganger("asc compare areas",
                                    asc_p,
                                    path = "")
})

test_that("no top asc compare areas draws correctly", {
        parent <- "PAC11"
        top_names <- c("C001", parent)
        df_ca <- create_test_data() %>%
                filter(IndicatorName == "Indicator 3",
                       (AreaCode %in% top_names |
                                ParentAreaCode == parent))

        asc_no_top_p <- compare_areas(df_ca, AreaCode, Value,
                                      fill = Significance,
                                      lowerci = LCI,
                                      upperci = UCI,
                                      order = "asc",
                                      title = "Compare the local areas")

        vdiffr::expect_doppelganger("no top areas asc compare areas",
                                    asc_no_top_p,
                                    path = "")
})

test_that("desc compare areas no fill draws correctly", {
        parent <- "PAC11"
        top_names <- c("C001", parent)
        df_ca <- create_test_data() %>%
                filter(IndicatorName == "Indicator 3",
                       (AreaCode %in% top_names |
                                ParentAreaCode == parent))

        desc_p_no_fill <- compare_areas(df_ca, AreaCode, Value,
                                        lowerci = LCI,
                                        upperci = UCI,
                                        order = "desc",
                                        top_areas = top_names,
                                        title = "Compare the local areas")
        vdiffr::expect_doppelganger("desc no fill compare areas",
                                    desc_p_no_fill,
                                    path = "")
})


test_that("desc compare areas no fill draws correctly while displaying values to 2 dps", {
        parent <- "PAC11"
        top_names <- c("C001", parent)
        df_ca <- create_test_data() %>%
                filter(IndicatorName == "Indicator 3",
                       (AreaCode %in% top_names |
                                ParentAreaCode == parent))

        desc_p_no_fill_disp_vals <- compare_areas(df_ca, AreaCode, Value,
                                                  lowerci = LCI,
                                                  upperci = UCI,
                                                  order = "desc",
                                                  top_areas = top_names,
                                                  title = "Compare the local areas",
                                                  display.values = TRUE,
                                                  dps = 2)
        vdiffr::expect_doppelganger("desc no fill compare areas displaying values",
                                    desc_p_no_fill_disp_vals,
                                    path = "")
})
