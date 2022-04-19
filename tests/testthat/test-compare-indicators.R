
test_that("Error for missing area field works", {
        df_ci <- create_test_data() %>%
                filter(IndicatorName %in% c("Indicator 1", "Indicator 3")) %>%
                select(IndicatorName, AreaCode, Value) %>%
                tidyr::pivot_wider(names_from = IndicatorName,
                                   values_from = Value) %>%
                rename(Ind1 = `Indicator 1`,
                       Ind3 = `Indicator 3`) %>%
                mutate(Ind2 = runif(nrow(.), min = Ind1 * 0.5, max = Ind1 * 1.5))
        expect_error(compare_indicators(df_ci,
                                        x = Ind1,
                                        y = Ind3,
                                        xlab = "Indicator 1 label",
                                        ylab = "Indicator 3 label",
                                        highlight_area = c("C001", "AC172"),
                                        add_R2 = TRUE),
                     "If highlight_area contains a value, so must area_field")
})

# Visual tests ------------------------------------------------------------

test_that("compare indicators without highlighted areas draws correctly", {
        df_ci <- create_test_data() %>%
                filter(IndicatorName %in% c("Indicator 1", "Indicator 3")) %>%
                select(IndicatorName, AreaCode, Value) %>%
                tidyr::pivot_wider(names_from = IndicatorName,
                                   values_from = Value) %>%
                rename(Ind1 = `Indicator 1`,
                       Ind3 = `Indicator 3`) %>%
                mutate(Ind2 = runif(nrow(.), min = Ind1 * 0.5, max = Ind1 * 1.5))
        comp_ind_p <- compare_indicators(df_ci,
                                         x = Ind1,
                                         y = Ind3,
                                         xlab = "Indicator 1 label",
                                         ylab = "Indicator 3 label",
                                         add_R2 = TRUE)

        vdiffr::expect_doppelganger("ci without highlighted areas",
                                    fig = comp_ind_p)
})

test_that("compare indicators with highlighted areas draws correctly", {
        df_ci <- create_test_data() %>%
                filter(IndicatorName %in% c("Indicator 1", "Indicator 3")) %>%
                select(IndicatorName, AreaCode, Value) %>%
                tidyr::pivot_wider(names_from = IndicatorName,
                                   values_from = Value) %>%
                rename(Ind1 = `Indicator 1`,
                       Ind3 = `Indicator 3`) %>%
                mutate(Ind2 = runif(nrow(.), min = Ind1 * 0.5, max = Ind1 * 1.5))
        comp_ind_highlight_p <- compare_indicators(df_ci,
                                                   x = Ind1,
                                                   y = Ind3,
                                                   xlab = "Indicator 1 label",
                                                   ylab = "Indicator 3 label",
                                                   highlight_area = c("C001", "AC172"),
                                                   area = AreaCode,
                                                   add_R2 = TRUE)

        vdiffr::expect_doppelganger("ci with highlighted areas",
                                    fig = comp_ind_highlight_p)
})

test_that("compare indicators with high R2 draws correctly", {
        df_ci <- create_test_data() %>%
                filter(IndicatorName %in% c("Indicator 1", "Indicator 3")) %>%
                select(IndicatorName, AreaCode, Value) %>%
                tidyr::pivot_wider(names_from = IndicatorName,
                                   values_from = Value) %>%
                rename(Ind1 = `Indicator 1`,
                       Ind3 = `Indicator 3`) %>%
                mutate(Ind2 = runif(nrow(.), min = Ind1 * 0.5, max = Ind1 * 1.5))
        comp_ind__R2_p <- compare_indicators(df_ci,
                                             x = Ind1,
                                             y = Ind2,
                                             xlab = "Indicator 1 label",
                                             ylab = "Indicator 2 label",
                                             add_R2 = TRUE)
        vdiffr::expect_doppelganger("compare indicators with high R2",
                                    fig = comp_ind__R2_p)
})
