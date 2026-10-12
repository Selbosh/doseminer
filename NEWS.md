# doseminer (development version)

* Fixed the Latin abbreviation "qds" (*quater die sumendus*) being translated as "daily" (frequency 1) instead of "four times a day" (frequency 4). "qd" (*quaque die*) is still translated as "daily". Thanks to Alex Haddy for reporting.

# doseminer 0.2.1

* Fix maintainer e-mail addresses

# doseminer 0.2.0

* Adds fix for breaking change introduced by upstream dependency `stringr`. Thanks to @hadley. 

# doseminer 0.1.2

* Remove the `output` column (only used for debugging) from the data frame returned by `extract_from_prescription`

# doseminer 0.1.1

* Fixed implicit coercion of strings to factors in `extract_from_prescription`, which caused some unit tests to fail on older versions of R (< 4.0.0).
* Fixed some typos in the documentation and vignettes

# doseminer 0.1.0

* Added a `NEWS.md` file to track changes to the package.
* Prepare for initial submission to CRAN
