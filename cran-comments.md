## Test environments
* local R installation, R 4.0.5
* ubuntu 16.04 (on travis-ci), R 4.0.5
* win-builder (devel)

## R CMD check results

0 errors | 0 warnings | 1 note

## Resubmission

* This is a resubmission (of a new release).
* Version 0.1.0 was accepted and sent to CRAN, but then failed on `r-oldrel`, due to `stringsAsFactors` causing some unit test comparisons to fail on older versions of R (i.e. < 4.0.0). This bug is now fixed.
