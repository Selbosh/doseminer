R package doseminer
================
David Selby and Belay Birlie

<!-- badges: start -->
<!-- badges: end -->

An R implementation of the [DOSES text mining algorithm of Karystianis
et al.](http://gnteam.cs.manchester.ac.uk/resources/DOSES/) for
extracting drug dosage information from CPRD prescription data. The aim
of this project is to provide a complete replacement for the algorithm,
entirely written in R with no external dependencies (on Python on Java).
This should make the tool more portable, extensible and suitable for use
across different platforms (Windows, Mac, Unix).

## Roadmap

The package **doseminer** is still in development. Current plans are to
transliterate the existing Python code to R. Later we will tidy up the
user interface and possibly allow for customisation.

The bulk of the work will be in converting the `.mixup` files (see
below) into the R equivalent. As it potentially avoids lots of file IO,
it may ultimately be faster than the original implementation.

## Installation

The package is not yet available on CRAN, so install it from GitHub
using

``` r
# install.packages('remotes')
remotes::install_github('Selbosh/doseminer')
```

## Usage

The workhorse function is called `extract_from_prescription`. Pass it a
character vector of freetext prescriptions and it will try to extract
the following variables:

-   Dose frequency (the number of times per day a dose is administered)
-   Dose interval (the number of days between doses)
-   Dose unit (how individual doses are measured, e.g. millilitres,
    tablets)
-   Dose number (how many of those units comprise a single dose, e.g. 2
    tablets)
-   Optional (should the dose only be taken ‘if required’ / ‘as
    needed’?)

``` r
library(doseminer)
extract_from_prescription('take two and a half tablets every two to three days as needed')
```

<div class="kable-table">

| raw                                                           | output  | freq | itvl | dose | unit | optional |
|:--------------------------------------------------------------|:--------|:-----|:-----|:-----|:-----|---------:|
| take two and a half tablets every two to three days as needed | 2.5 tab | 1    | 2-3  | 2.5  | tab  |        1 |

</div>

Anything not matched is returned as `NA`, though some inferences are
also made. For instance: if a dosage is specified as multiple times per
day, with no explicit interval between days, it’s inferred the interval
is one day. Similarly, if an interval is specified (e.g. every 3 days)
but not a daily frequency, it’s presumed the dose is taken only once
during the day.

To see the package in action, a small vector of example prescriptions is
included in the variable `example_prescriptions`.

``` r
extract_from_prescription(example_prescriptions)
```

<div class="kable-table">

| raw                                                           | output                                   | freq | itvl | dose | unit        | optional |
|:--------------------------------------------------------------|:-----------------------------------------|:-----|:-----|:-----|:------------|---------:|
| 1 tablet to be taken daily                                    | 1 tab to be taken                        | 1    | 1    | 1    | tab         |        0 |
| 2.5ml four times a day when required                          | 2.5 ml                                   | 4    | 1    | 2.5  | ml          |        1 |
| 1.25mls three times a day                                     | 1.25 ml                                  | 3    | 1    | 1.25 | ml          |        0 |
| take 10mls q.d.s. p.r.n.                                      | 10 ml                                    | 1    | 1    | 10   | ml          |        1 |
| take 1 or 2 4 times/day                                       | 1 - 2                                    | 4    | 1    | 1-2  | NA          |        0 |
| 2x5ml spoon 4 times/day                                       | 2 x 5 ml spoonful                        | 4    | 1    | 10   | ml spoonful |        0 |
| take 2 tablets every six hours max eight in twenty four hours | 2 tab 0 - 8 in 24 hours                  | 4    | 1    | 2    | tab         |        0 |
| 1 tab nocte twenty eight tablets                              | 1 tab 28 tab                             | 1    | 1    | 1    | tab         |        0 |
| 1-2 four times a day when required                            | 1 - 2                                    | 4    | 1    | 1-2  | NA          |        1 |
| take one twice daily                                          | 1                                        | 2    | 1    | 1    | NA          |        0 |
| 1 q4h prn                                                     | 1                                        | 6    | 1    | 1    | NA          |        1 |
| take two every three days                                     | 2                                        | 1    | 3    | 2    | NA          |        0 |
| five every week                                               | 5                                        | 1    | 7    | 5    | NA          |        0 |
| every 72 hours                                                |                                          | 1    | 3    | NA   | NA          |        0 |
| 1 x 5 ml spoon 4 / day for 10 days                            | 1 x 5 ml spoonful for 10 days            | 4    | 1    | 5    | ml spoonful |        0 |
| two to three times a day                                      |                                          | 2-3  | 1    | NA   | NA          |        0 |
| three times a week                                            |                                          | 1    | 2-3  | NA   | NA          |        0 |
| three 5ml spoonsful to be taken four times a day after food   | 3 x 5 ml spoonful to be taken after food | 4    | 1    | 15   | ml spoonful |        0 |
| take one or two every 4-6 hrs                                 | 1 - 2                                    | 4-6  | 1    | 1-2  | NA          |        0 |
| 5ml 3 hrly when required                                      | 5 ml                                     | 8    | 1    | 5    | ml          |        1 |
| one every morning to reduce bp                                | 1 to reduce bp                           | 1    | 1    | 1    | NA          |        0 |
| take 1 or 2 6hrly when required                               | 1 - 2                                    | 4    | 1    | 1-2  | NA          |        1 |
| take 1 or 2 four times a day as required for pain             | 1 - 2 for pain                           | 4    | 1    | 1-2  | NA          |        1 |
| take 1 or 2 4 times/day if needed for pain                    | 1 - 2 for pain                           | 4    | 1    | 1-2  | NA          |        1 |
| 1-2 tablets up to four times daily                            | 1 - 2 tab                                | 0-4  | 1    | 1-2  | tab         |        1 |
| take one or two tablets 6-8 hrly every 2-3 days               | 1 - 2 tab                                | 3-4  | 2-3  | 1-2  | tab         |        0 |

</div>

The column `output` represents the ‘residual’ text after other features
have been extracted. It can be ignored for most applications, but is
useful for debugging prescriptions that have not been parsed as
expected.

## Development notes

To do:

1.  Unit tests
2.  Formal benchmarking
3.  Improving algorithmic accuracy (e.g. on half measures)

Built into this package is a series of functions for extracting and
parsing natural language English numbers into their digit-based numeric
form. This could be spun out into its own package for more general use.

``` r
replace_numbers(c('Thirty seven bottles of beer on the wall',
                  'Take one down, pass it around',
                  'Thirty-six bottles of beer on the wall!',
                  'One MILLION dollars.',
                  'We do not take any half measures'))
```

    ## [1] "37 bottles of beer on the wall"  "Take 1 down, pass it around"    
    ## [3] "36 bottles of beer on the wall!" "1e+06 dollars."                 
    ## [5] "We do not take any 0.5 measures"

Inspired by Ben Marwick’s `words2number`
(<https://github.com/benmarwick/words2number>).

## Contributors

Maintained by David Selby (`david.selby@manchester.ac.uk`), Belay Birlie
and (formerly) Katherine Dempsey.

## References

Karystianis, G., Sheppard, T., Dixon, W.G. *et al.* Modelling and
extraction of variability in free-text medication prescriptions from an
anonymised primary care electronic medical record research database.
*BMC Med Inform Decis Mak* **16**, 18 (2015).  
<https://doi.org/10.1186/s12911-016-0255-x>
