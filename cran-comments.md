## Test environments
* local R installation, R 4.0.5
* ubuntu 16.04 (on travis-ci), R 4.0.5
* win-builder (devel)

## R CMD check results

0 errors | 0 warnings | 1 note

## Resubmission

This is a resubmission (of a new release). In this version I have

* Added more detail about package functionality to Description, including a reference
* Made the Title of the package more descriptive.
* Removed all instances of `\dontrun{}` from examples.
* Added `\value` to all methods where it was previously missing.
* Removed a call to a non-exported function (`foo:::bar`) from an example.

The 'possibly-misspelt' word in the Description is an author's name.
