---
title: R package doseminer
author:
  - David Selby
  - Belay Birlie
  - Katherine Dempsey
---

<!-- badges: start -->
<!-- badges: end -->

An R implementation of George Karystianis's text mining algorithm for extracting
drug dosage information from CPRD prescription data.
The aim of this project is to provide a complete replacement for the algorithm,
entirely written in R with no external dependencies (on Python on Java).
This should make the tool more portable, extensible and suitable for use across
different platforms (Windows, Mac, Unix).

## Roadmap

The package **doseminer** is still in development.
Current plans are to transliterate the existing Python code to R.
Later we will tidy up the user interface and possibly allow for customisation.

The bulk of the work will be in converting the `.mixup` files (see below) into the R equivalent. As it potentially avoids lots of file IO, it may ultimately be faster than the original implementation.

### To do

1. Unit tests (add a large dataset of expected outputs, based on common dosages)
2. Finish translating `dose_number3.mixup` and others
3. Pre-process strings (remove double-spaces, separate tokens)
3. Docs (explain to KD and BB how to maintain)
4. Front end
5. Upload to GitHub (privately)

Repeated `sprintf` commands have been replaced with `glue::glue()` for
improved code readability. Otherwise the vast majority of code is either
```r
sprintf('(%s)', something)
```
or
```r
paste(something, collapse = '|')
```
and both of these could be abstracted away to make the code more readable.
This might include using `.transformer` functions in the `glue` package, or
simply making a utility function called `or()`, for instance.

## Workflow

The Python implementation works something like the following.

1. Split prescriptions into lots of separate one-line text files, one per prescription.
  - `drug_information_extraction.py`

2. Use the Java library MinorThird to tokenise the free text.
  - `dose_characteristics.mixup`
    + `dose_number3.mixup` (in progress)
    + `dose_unit.mixup`    (done)
    + `dose_interval.mixup`
    + `frequency_phase.mixup`
  
The output of this is a `.labels` file which describes where, in each file
(via indices describing the position of a substring of the freetext) to find
tokens of interest, and their category (a `spanType`). This is achieved via a
sequence of regular expressions and can be (with some effort) translated into R.

3. The remainder of the Python scripts are mostly concerned with housekeeping to
convert the esoteric MinorThird output into a useful format. Though there are
some additional regular expressions in Python, too, such as (I think) checking
whether a drug is 'required' or 'optional', among others.

## Usage

The package is very agricultural at the moment.
For the time being, the workhorse functions are

- `guess_dose_unit` (works so far)
- `guess_dose_number` (may need to return a min and a max)

with plans for

- `guess_dose_frequency`
- `guess_dose_interval`

and so on.

## Development notes

Always use non-capturing groups, `(?: )` in regular expressions. Capture groups
slow down pattern matching and extraction considerably (unit tests took 1.5s
with capture groups; setting all groups to non-capturing reduced runtime to
be nearly instantaneous).

## Contributors

Maintained by David Selby (`david.selby@manchester.ac.uk`), Belay Birlie and
Katherine Dempsey.
