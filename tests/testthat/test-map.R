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
         subtitle = "Males in Upper Tier Local Authorities England")

# Visual tests ------------------------------------------------------------

test_that("map draws correctly", {
        vdiffr::expect_doppelganger("map",
                                    p
        )
})
