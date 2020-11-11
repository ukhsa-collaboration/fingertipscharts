
# Visual tests ------------------------------------------------------------

test_that("compare areas descending draws correctly", {

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
        vdiffr::expect_doppelganger("compare areas descending",
                                    desc_p,
                                    path = "")
})

test_that("compare areas descending without top_areas draws correctly", {
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

        vdiffr::expect_doppelganger("compare areas descending without top_areas",
                                    desc_no_top_p,
                                    path = "")
})

test_that("compare areas ascending draws correctly", {
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
        vdiffr::expect_doppelganger("compare areas ascending",
                                    asc_p,
                                    path = "")
})

test_that("compare areas ascending without top_areas", {
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

        vdiffr::expect_doppelganger("compare areas ascending without top_areas",
                                    asc_no_top_p,
                                    path = "")
})

test_that("compare areas descending without significance fill draws correctly", {
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
        vdiffr::expect_doppelganger("compare areas descending without significance fill",
                                    desc_p_no_fill,
                                    path = "")
})


test_that("compare areas descending without significance fill and two dps draws correctly", {
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
        vdiffr::expect_doppelganger("compare areas descending without significance fill and two dps",
                                    desc_p_no_fill_disp_vals,
                                    path = "")
})
