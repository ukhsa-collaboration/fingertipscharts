context("map")

library(fingertipsR)
library(dplyr)
df <- fingertips_data(90366) %>%
        filter(Sex == "Male" &
                       AreaType == "County & UA" &
                       Timeperiod == "2014 - 16")
ons_api <- "https://opendata.arcgis.com/datasets/687f346f5023410ba86615655ff33ca9_4.geojson"

p <- map(df,
         ons_api = ons_api,
         area_code = AreaCode,
         fill = ComparedtoEnglandvalueorpercentiles,
         title = "Life expectancy at birth",
         subtitle = "Males in Upper Tier Local Authorities England",
         copyright_year = 2018)
p1 <- map(df,
          ons_api = ons_api,
          area_code = AreaCode,
          fill = ComparedtoEnglandvalueorpercentiles,
          title = "Life expectancy at birth",
          subtitle = "Males in Upper Tier Local Authorities England",
          copyright_year = as.Date("2018-01-01"))

ons_regions <- "https://opendata.arcgis.com/datasets/f99b145881724e15a04a8a113544dfc5_3.geojson"

# Visual tests ------------------------------------------------------------

test_that("map draws correctly", {
        vdiffr::expect_doppelganger("map", p)
        vdiffr::expect_doppelganger("map", p1)
})

test_that("error messages work for map", {
        expect_error(map(df,
                         ons_api = ons_api,
                         area_code = AreaCode,
                         fill = ComparedtoEnglandvalueorpercentiles,
                         title = "Life expectancy at birth",
                         subtitle = "Males in Upper Tier Local Authorities England",
                         copyright_year = "2018"),
                     "copyright_year must be either a 4 digit numeric class or Date class")
        expect_error(map(df,
                         ons_api = ons_regions,
                         area_code = AreaCode,
                         fill = ComparedtoEnglandvalueorpercentiles,
                         title = "Life expectancy at birth",
                         subtitle = "Males in Upper Tier Local Authorities England"),
                     "There is no clear field in the shape file that contains the area codes in the field you have identified")
})
