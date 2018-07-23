This is a resubmission of the first submission of this package following feedback on 2018-07-21.

The feedback was to use person only in the Author@R field and to replace all instances of dontrun with donttest where examples are greater than 5 seconds long (and unwrap them otherwise).

This package allows users to easily recreate visualisations seen on the [Fingertips tool](http://fingertips.phe.org.uk) that is made available by Public Health England.

## Test

* local Windows 7 install, R 3.5.1
* used Travis to check on Linux (2018-7-23)

## R CMD check results

There were 2 NOTEs

1. new submission
2. Examples with CPU or elapsed time > 5s (these are wrapped in \\donttest{} as recommended by CRAN in email on 2018-07-21)

There were no WARNINGs or ERRORs.

## Downstream dependencies

No errors with downstream dependencies
