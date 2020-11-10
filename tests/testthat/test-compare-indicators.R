# Visual tests ------------------------------------------------------------

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

test_that("compare indicators no highlight draws correctly", {
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

        vdiffr::expect_doppelganger("compare indicators no highlight",
                                    comp_ind_p,
                                    path = "")
})

test_that("compare indicators highlight draws correctly", {
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

        vdiffr::expect_doppelganger("compare indicators highlight",
                                    comp_ind_highlight_p,
                                    path = "")
})

test_that("compare indicators highlight high R2 draws correctly", {
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
        vdiffr::expect_doppelganger("compare indicators high R2",
                                    comp_ind__R2_p,
                                    path = "")
})
