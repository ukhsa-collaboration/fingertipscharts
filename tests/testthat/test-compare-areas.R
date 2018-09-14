context("compare_areas")
library(fingertipscharts)
df <- create_test_data()

parent <- "PAC11"
top_names <- c("C001", parent)
ordered_levels <- c("Better",
                    "Similar",
                    "Worse",
                    "Not compared")
df_ca <- df %>%
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

desc_no_top_p <- compare_areas(df_ca, AreaCode, Value,
                   fill = Significance,
                   lowerci = LCI,
                   upperci = UCI,
                   order = "desc",
                   title = "Compare the local areas")

asc_p <- compare_areas(df_ca, AreaCode, Value,
                   fill = Significance,
                   lowerci = LCI,
                   upperci = UCI,
                   order = "asc",
                   top_areas = top_names,
                   title = "Compare the local areas")

asc_no_top_p <- compare_areas(df_ca, AreaCode, Value,
                             fill = Significance,
                             lowerci = LCI,
                             upperci = UCI,
                             order = "asc",
                             title = "Compare the local areas")

desc_p_no_fill <- compare_areas(df_ca, AreaCode, Value,
                        lowerci = LCI,
                        upperci = UCI,
                        order = "desc",
                        top_areas = top_names,
                        title = "Compare the local areas")

# Visual tests ------------------------------------------------------------

test_that("desc compare areas draws correctly", {
        vdiffr::expect_doppelganger("desc compare areas",
                                    desc_p
        )
})

test_that("no top desc compare areas draws correctly", {
        vdiffr::expect_doppelganger("no top areas desc compare areas",
                                    desc_no_top_p
        )
})

test_that("asc compare areas draws correctly", {
        vdiffr::expect_doppelganger("asc compare areas",
                                    asc_p
        )
})

test_that("no top asc compare areas draws correctly", {
        vdiffr::expect_doppelganger("no top areas asc compare areas",
                                    asc_no_top_p
        )
})

test_that("desc compare areas no fill draws correctly", {
        vdiffr::expect_doppelganger("desc no fill compare areas",
                                    desc_p_no_fill
        )
})
