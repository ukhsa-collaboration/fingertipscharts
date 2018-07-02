context("population")

library(fingertipsR)
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
full_p <- population(pops,
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

one_comparator_p <- population(pops,
                               value = Value,
                               sex = Sex,
                               age = Age,
                               area = AreaName,
                               area_name = "Nottingham",
                               comparator_1 = "England",
                               title = "Age Profile",
                               subtitle = paste(unique(pops$IndicatorName), unique(pops$Timeperiod)),
                               xlab = "% of total population")

no_comparator_p <- population(pops,
                              value = Value,
                              sex = Sex,
                              age = Age,
                              area = AreaName,
                              area_name = "Nottingham",
                              title = "Age Profile",
                              subtitle = paste(unique(pops$IndicatorName), unique(pops$Timeperiod)),
                              xlab = "% of total population")

test_that("Error for missing area name works", {
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
        vdiffr::expect_doppelganger("full pop pyramid",
                                    full_p
        )
})

test_that("one comparator population pyramid draws correctly", {
        vdiffr::expect_doppelganger("one comparator pop pyramid",
                                    one_comparator_p
        )
})

test_that("no comparator population pyramid draws correctly", {
        vdiffr::expect_doppelganger("no comparator pop pyramid",
                                    no_comparator_p
        )
})
