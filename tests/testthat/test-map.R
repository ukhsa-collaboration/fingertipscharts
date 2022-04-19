test_that("error where string passed to copyright_year in map fails", {
        skip_on_cran()
        df <- mapdata
        ons_api <- "https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Counties_and_Unitary_Authorities_December_2021_UK_BUC/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson"
        expect_error(map(df,
                         ons_api = ons_api,
                         area_code = AreaCode,
                         fill = Significance,
                         title = "Life expectancy at birth",
                         subtitle = "Males in Upper Tier Local Authorities England",
                         copyright_year = "2018"),
                     "copyright_year must be either a 4 digit numeric class or Date class")
})

test_that("error where no obvious join field between shape file and data", {
        skip_on_cran()
        df <- mapdata
        ons_regions <- "https://services1.arcgis.com/ESMARspQHYMw9BZ9/arcgis/rest/services/Regions_December_2021_EN_BUC/FeatureServer/0/query?outFields=*&where=1%3D1&f=geojson"
        expect_error(map(df,
                         ons_api = ons_regions,
                         area_code = AreaCode,
                         fill = Significance,
                         title = "Life expectancy at birth",
                         subtitle = "Males in Upper Tier Local Authorities England"),
                     "There is no clear field in the shape file that contains the area codes in the field you have identified")
})
test_that("error where API is unavailable", {
        df <- mapdata
        expect_error(map(df,
                         ons_api = "httpstat.us/500",
                         area_code = AreaCode,
                         fill = Significance,
                         title = "Life expectancy at birth",
                         subtitle = "Males in Upper Tier Local Authorities England"),
                     "The ons_api provided is currently unavailable")
})

test_that("error where ons api url isn't passed to map function", {
        df <- mapdata
        expect_error(map(df,
                         area_code = AreaCode,
                         fill = Significance,
                         title = "Life expectancy at birth",
                         subtitle = "Males in Upper Tier Local Authorities England"),
                     "ons_api must contain a string to a geojson url on the ONS geography portal")
})


# Visual tests ------------------------------------------------------------

test_that("map renders correctly with year input to copyright_year", {
        skip_on_cran()
        df <- mapdata
        ons_api <- "https://opendata.arcgis.com/datasets/687f346f5023410ba86615655ff33ca9_4.geojson"
        p <- map(df,
                 ons_api = ons_api,
                 area_code = AreaCode,
                 fill = Significance,
                 title = "Life expectancy at birth",
                 subtitle = "Males in Upper Tier Local Authorities England",
                 copyright_year = 2019)

        vdiffr::expect_doppelganger("map with numeric copyright_year",
                                    fig = p)
})
test_that("map renders correctly with date input to copyright_year", {
        skip_on_cran()
        df <- mapdata
        ons_api <- "https://opendata.arcgis.com/datasets/687f346f5023410ba86615655ff33ca9_4.geojson"
        p1 <- map(df,
                  ons_api = ons_api,
                  area_code = AreaCode,
                  fill = Significance,
                  title = "Life expectancy at birth",
                  subtitle = "Males in Upper Tier Local Authorities England",
                  copyright_year = as.Date("2019-01-01"))

        vdiffr::expect_doppelganger("map with date copyright_year",
                                    fig = p1)
})

