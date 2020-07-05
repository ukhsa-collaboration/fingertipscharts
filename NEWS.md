# fingertipscharts 0.0.11.9000

* Improved documentation for `area_profiles()`

# fingertipscharts 0.0.11

* trend argument added to `area_profiles()`
* `trends()` function now displays the area and comparator in the key for the chart
* slight amendment to `compare_areas()` and `map()` functions giving users flexibility to provide their own legend titles using `+ labs(fill = "Blah")` after function (see [issue 16](https://github.com/PublicHealthEngland/fingertipscharts/issues/16))
* `overview()` function now has an argument for `legend_position` whose ordering is controlled by the levels of the field assigned to `fill`
* fixed bug to allow users to modify the significance colours on spine chart

# fingertipscharts 0.0.10 (2019-10-07)

* Unnecessary warning messages for area_profiles() removed
* Fixed bug around ordering of factors filling the interactive maps

# fingertipscharts 0.0.9 (2019-09-09)

* Added example to `area_profiles()` to show how to specify the order of indicators and how to show domains
* Graceful fail if ons_api provided to map() function isn't responding


# fingertipscharts 0.0.8 (2019-07-18)

* No updates from a user perspective

# fingertipscharts 0.0.7 

* No updates from a user perspective

# fingertipscharts 0.0.6 (2019-05-13)

* `display.values` argument added to `compare_areas()` function - allowing users to display the values alongside the bars in the chart

# fingertipscharts 0.0.5 (2019-04-08)

* `area_profiles()` - users have control on number of decimal places to display non-count values at (including different decimal places for different indicators)

# fingertipscharts 0.0.4 (2019-02-07)

* added copyright_year argument to `map` function
* improved code coverage
* `area_profiles()` now should display the "Best/Highest" label better

# fingertipscharts 0.0.3 (2018-12-12)

* Improved functionality for `area_profiles()` to accept field names other than `IndicatorName` and `Polarity` for those two arguments

# fingertipscharts 0.0.2 (2018-09-14)

* Added detail around the `area_profiles()` function
* Added `create_test_data()`


# fingertipscharts 0.0.1

Due to popular demand, `fingertipscharts` has been created to help users create the charts they see on Public Health England's [Fingertips](https://fingertips.phe.org.uk/) website. There is an accompanying vignette that shows users the potential of the functions `browseVignettes("fingertipscharts")`
