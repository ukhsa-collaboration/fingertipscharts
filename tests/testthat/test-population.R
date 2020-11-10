


test_that("Error for missing area name works", {
        agelevels <- c("0-4", "5-9","10-14","15-19",
                       "20-24","25-29","30-34",
                       "35-39","40-44","45-49",
                       "50-54","55-59","60-64",
                       "65-69","70-74","75-79",
                       "80-84","85-89","90+")
        areas <- c("Area 1", "Area 2", "Area 3")
        set.seed(42)
        pops <- data.frame(Age = factor(rep(agelevels, length(areas) * 2), levels = agelevels),
                           Value = rep(sample(1000:3000, length(agelevels), replace = TRUE), length(areas) * 2),
                           Sex = rep(rep(c("Male", "Female"), each = length(agelevels)), length(areas)),
                           AreaName = rep(areas, each = length(agelevels) * 2))
        expect_error(population(pops,
                                value = Value,
                                sex = Sex,
                                age = Age,
                                area = AreaName,
                                title = "Age Profile",
                                subtitle = paste(unique(pops$IndicatorName), unique(pops$Timeperiod)),
                                xlab = "% of total population"),
                     "area_name must be complete for a population pyramid to be drawn")
})


# Visual tests ------------------------------------------------------------

test_that("full population pyramid draws correctly", {
        agelevels <- c("0-4", "5-9","10-14","15-19",
                       "20-24","25-29","30-34",
                       "35-39","40-44","45-49",
                       "50-54","55-59","60-64",
                       "65-69","70-74","75-79",
                       "80-84","85-89","90+")
        areas <- c("Area 1", "Area 2", "Area 3")
        set.seed(42)
        pops <- data.frame(Age = factor(rep(agelevels, length(areas) * 2), levels = agelevels),
                           Value = rep(sample(1000:3000, length(agelevels), replace = TRUE), length(areas) * 2),
                           Sex = rep(rep(c("Male", "Female"), each = length(agelevels)), length(areas)),
                           AreaName = rep(areas, each = length(agelevels) * 2))
        full_p <- population(pops,
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

        vdiffr::expect_doppelganger("full pop pyramid",
                                    full_p
        )
})

test_that("one comparator population pyramid draws correctly", {
        agelevels <- c("0-4", "5-9","10-14","15-19",
                       "20-24","25-29","30-34",
                       "35-39","40-44","45-49",
                       "50-54","55-59","60-64",
                       "65-69","70-74","75-79",
                       "80-84","85-89","90+")
        areas <- c("Area 1", "Area 2", "Area 3")
        set.seed(42)
        pops <- data.frame(Age = factor(rep(agelevels, length(areas) * 2), levels = agelevels),
                           Value = rep(sample(1000:3000, length(agelevels), replace = TRUE), length(areas) * 2),
                           Sex = rep(rep(c("Male", "Female"), each = length(agelevels)), length(areas)),
                           AreaName = rep(areas, each = length(agelevels) * 2))
        one_comparator_p <- population(pops,
                                       value = Value,
                                       sex = Sex,
                                       age = Age,
                                       area = AreaName,
                                       area_name = "Area 1",
                                       comparator_1 = "Area 3",
                                       title = "Age Profile",
                                       subtitle = "2015/16",
                                       xlab = "% of total population")
        vdiffr::expect_doppelganger("one comparator pop pyramid",
                                    one_comparator_p
        )
})

test_that("no comparator population pyramid draws correctly", {
        agelevels <- c("0-4", "5-9","10-14","15-19",
                       "20-24","25-29","30-34",
                       "35-39","40-44","45-49",
                       "50-54","55-59","60-64",
                       "65-69","70-74","75-79",
                       "80-84","85-89","90+")
        areas <- c("Area 1", "Area 2", "Area 3")
        set.seed(42)
        pops <- data.frame(Age = factor(rep(agelevels, length(areas) * 2), levels = agelevels),
                           Value = rep(sample(1000:3000, length(agelevels), replace = TRUE), length(areas) * 2),
                           Sex = rep(rep(c("Male", "Female"), each = length(agelevels)), length(areas)),
                           AreaName = rep(areas, each = length(agelevels) * 2))
        no_comparator_p <- population(pops,
                                      value = Value,
                                      sex = Sex,
                                      age = Age,
                                      area = AreaName,
                                      area_name = "Area 1",
                                      title = "Age Profile",
                                      subtitle = "2015/16",
                                      xlab = "% of total population")
        vdiffr::expect_doppelganger("no comparator pop pyramid",
                                    no_comparator_p
        )
})
