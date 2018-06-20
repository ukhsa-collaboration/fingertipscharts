context("compare_areas")

parent <- "PAC11"
top_names <- c("C001", parent)
ordered_levels <- c("Better",
                    "Same",
                    "Worse",
                    "Not compared")
# df <- fingertips_data(90316) %>%
#         filter(is.na(CategoryType) &
#                        Timeperiod == "2016/17" &
#                        (AreaName %in% top_names |
#                                 ParentName == region) &
#                        Sex == "Persons") %>%
#         mutate(ComparedtoEnglandvalueorpercentiles =
#                        factor(ComparedtoEnglandvalueorpercentiles,
#                               levels = ordered_levels))
df_ca <- df %>%
        filter(IndicatorName == "Indicator 3",
               (AreaCode %in% top_names |
                        ParentAreaCode == parent))
p <- compare_areas(df_ca, AreaCode, Value,
                   fill = Significance,
                   lowerci = LCI,
                   upperci = UCI,
                   order = "desc",
                   top_areas = top_names,
                   title = "Compare the local areas")

# Visual tests ------------------------------------------------------------

test_that("compare areas draws correctly", {
        vdiffr::expect_doppelganger("compare areas",
                                    p
        )
})
