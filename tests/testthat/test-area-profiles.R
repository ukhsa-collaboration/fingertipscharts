context("area-profiles")
library(dplyr)


p <- area_profiles(df,
                   value = Value,
                   count = Count,
                   area_code = AreaCode,
                   local_area_code = "AC122",
                   indicator = IndicatorName,
                   timeperiod = Timeperiod,
                   polarity = Polarity,
                   significance = Significance,
                   area_type = AreaType,
                   median_line_area_code = "C001",
                   comparator_area_code = "PAC12",
                   datatable = TRUE,
                   relative_domain_text_size = 0.75,
                   relative_text_size = 1.2,
                   bar_width = 0.68,
                   indicator_label_nudgex = -0.5,
                   show_dividers = "outer",
                   header_positions = c(-0.7, -0.53, -0.35, -0.25,
                                        -0.15, -0.05, 1.05)
)

# Visual tests ------------------------------------------------------------

test_that("area profiles draws correctly", {
        vdiffr::expect_doppelganger("area profiles",
                                    p
        )
})
