This release fixes the package that was breaking due the vignette not compiling properly. I have also added graceful failures with useful messages where the package requires the internet but it is unavailable.

## Test

* local Windows 10 install, R 3.6.1
* used Travis to check on Linux (2019-09-09)

## R CMD check results


There were no NOTEs, WARNINGs or ERRORs.

## Downstream dependencies

No errors with downstream dependencies
