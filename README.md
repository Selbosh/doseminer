R package doseminer
================
David Selby and Belay Birlie

<!-- badges: start -->
<!-- badges: end -->

An R implementation of George Karystianis’s text mining algorithm for
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

As this is a private GitHub repository, you might not be able to install
it using

``` r
remotes::install_github('Selbosh/doseminer')
```

and should instead download the source (as a zip/tar) and use

``` r
install.packages('path_to_downloaded_archive.tar.gz', repos = NULL)
```

See also `?remotes::install_local`.

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
extract_from_prescription('take two tablets every three days as needed')
```

<div class="kable-table">

| raw                                         | output     | freq | itvl | dose | unit | optional |
|:--------------------------------------------|:-----------|-----:|:-----|:-----|:-----|---------:|
| take two tablets every three days as needed | take 2 tab |    1 | 3    | 2    | tab  |        1 |

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
| take 10mls q.d.s. p.r.n.                                      | take 10 ml                               | 1    | 1    | 10   | ml          |        1 |
| take 1 or 2 4 times/day                                       | take 1 - 2                               | 4    | 1    | 1-2  | NA          |        0 |
| 2x5ml spoon 4 times/day                                       | 2 x 5 ml spoonful                        | 4    | 1    | 10   | ml spoonful |        0 |
| take 2 tablets every six hours max eight in twenty four hours | take 2 tab max 8 in 24 hours             | 4    | 1    | 2    | tab         |        0 |
| 1 tab nocte twenty eight tablets                              | 1 tab at night 28 tab                    | NA   | NA   | 1    | tab         |        0 |
| 1-2 four times a day when required                            | 1 - 2                                    | 4    | 1    | 1-2  | NA          |        1 |
| take one twice daily                                          | take 1                                   | 2    | 1    | 1    | NA          |        0 |
| 1 q4h prn                                                     | 1                                        | 6    | 1    | 1    | NA          |        1 |
| take two every three days                                     | take 2                                   | 1    | 3    | 2    | NA          |        0 |
| five every week                                               | 5                                        | 1    | 7    | 5    | NA          |        0 |
| every 72 hours                                                |                                          | 1    | 3    | NA   | NA          |        0 |
| 1 x 5 ml spoon 4 / day for 10 days                            | 1 x 5 ml spoonful for 10 days            | 4    | 1    | 5    | ml spoonful |        0 |
| two to three times a day                                      |                                          | 2-3  | 1    | NA   | NA          |        0 |
| three times a week                                            |                                          | 1    | 2-3  | NA   | NA          |        0 |
| three 5ml spoonsful to be taken four times a day after food   | 3 x 5 ml spoonful to be taken after food | 4    | 1    | 15   | ml spoonful |        0 |
| take one or two every 4-6 hrs                                 | take 1 - 2 every 4 - 6 hrs               | NA   | NA   | NA   | NA          |        0 |
| 5ml 3 hrly when required                                      | 5 ml                                     | 8    | 1    | 5    | ml          |        1 |
| one every morning to reduce bp                                | 1 to reduce bp                           | 1    | 1    | 1    | NA          |        0 |

</div>

## Development notes

To do:

1.  Unit tests
2.  Converting extracted frequency, interval and number ranges into
    min/max
3.  Improving algorithmic accuracy

Built into this package is a series of functions for extracting and
parsing natural language English numbers into their digit-based numeric
form. This could be spun out into its own package for more general use.

``` r
replace_numbers(c('Thirty seven bottles of beer on the wall',
                  'Take one down, pass it around',
                  'Thirty-six bottles of beer on the wall!',
                  'One MILLION dollars.'))
```

    ## [1] "37 bottles of beer on the wall"  "Take 1 down, pass it around"    
    ## [3] "36 bottles of beer on the wall!" "1e+06 dollars."

This does not support fractional units (“one and a half tablets”) yet.

## Contributors

Maintained by David Selby (`david.selby@manchester.ac.uk`), Belay Birlie
and (formerly) Katherine Dempsey.
