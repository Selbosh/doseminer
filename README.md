R package doseminer
================
David SelbyBelay BirlieKatherine Dempsey

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

### To do

1.  Unit tests (add a large dataset of expected outputs, based on common
    dosages)
2.  Finish translating `dose_number3.mixup` and others
3.  Pre-process strings (remove double-spaces, separate tokens)
4.  Docs (explain to KD and BB how to maintain)
5.  Front end
6.  Upload to GitHub (privately)

Repeated `sprintf` commands have been replaced with `glue::glue()` for
improved code readability. Otherwise the vast majority of code is either

``` r
sprintf('(%s)', something)
```

or

``` r
paste(something, collapse = '|')
```

and both of these could be abstracted away to make the code more
readable. This might include using `.transformer` functions in the
`glue` package, or simply making a utility function called `or()`, for
instance.

## Workflow

The Python implementation works something like the following.

1.  Split prescriptions into lots of separate one-line text files, one
    per prescription.

<!-- end list -->

  - `drug_information_extraction.py`

<!-- end list -->

2.  Use the Java library MinorThird to tokenise the free text.

<!-- end list -->

  - `dose_characteristics.mixup`
      - `dose_number3.mixup` (in progress)
      - `dose_unit.mixup` (done)
      - `dose_interval.mixup`
      - `frequency_phase.mixup`

The output of this is a `.labels` file which describes where, in each
file (via indices describing the position of a substring of the
freetext) to find tokens of interest, and their category (a `spanType`).
This is achieved via a sequence of regular expressions and can be (with
some effort) translated into R.

3.  The remainder of the Python scripts are mostly concerned with
    housekeeping to convert the esoteric MinorThird output into a useful
    format. Though there are some additional regular expressions in
    Python, too, such as (I think) checking whether a drug is ‘required’
    or ‘optional’, among others.

## Usage

The package is very agricultural at the moment. For the time being, the
workhorse functions are

  - `guess_dose_unit` (works so far)
  - `guess_dose_number` (may need to return a min and a max)

with plans for

  - `guess_dose_frequency`
  - `guess_dose_interval`

and so on. Clearly, the functions to convert extracted strings into
numeric dosages are not ready, yet. But the basic principle should be
fairly clear.

``` r
library(doseminer)
results <- tibble(
  input_txt  = common_dosages[1:50, 'PRESCRIPTION'],
  unit       = guess_dose_unit(input_txt),
  dose_txt   = guess_number(input_txt),
  freq_txt   = guess_frequency(input_txt),
  dose_num   = convert_number_text(dose_txt)
)
results
```

<div class="kable-table">

| input\_txt                                      | unit | dose\_txt | freq\_txt         | dose\_num |
| :---------------------------------------------- | :--- | :-------- | :---------------- | :-------- |
| take one daily                                  |      | 1         |                   |           |
| as directed                                     |      |           |                   |           |
| one every day                                   |      | 1         | every day         |           |
| one twice a day                                 |      | 1         | twice a day       |           |
| 1 every day                                     |      | 1         | every day         |           |
| one three times a day                           |      | 1         | three times a day |           |
| take one each morning                           |      | 1         | each morning      |           |
| one every morning                               |      | 1         | every morning     |           |
| twice a day                                     |      |           | twice a day       |           |
| take one at night                               |      | 1         | at night          |           |
| every day                                       |      |           | every day         |           |
| three times a day                               |      |           | three times a day |           |
| take one twice daily                            |      | 1         | twice             |           |
| one daily                                       |      | 1         | daily             |           |
| take one 3 times/day                            |      | 1         | 3 times           |           |
| one every night                                 |      | 1         | every night       |           |
| 1 twice a day                                   |      | 1         | twice a day       |           |
| 1 daily                                         |      | 1         | daily             |           |
| one at night                                    |      | 1         | at night          |           |
| 1 three times a day                             |      | 1         | three times a day |           |
| one four times a day                            |      | 1         | four times a day  |           |
| 1 every morning                                 |      | 1         | every morning     |           |
| take one once daily                             |      | 1         | once              |           |
| when required                                   |      |           |                   |           |
| two every day                                   |      | 2         | every day         |           |
| four times a day                                |      |           | four times a day  |           |
| one in the morning                              |      | 1         | morning           |           |
| two twice a day                                 |      | 2         | twice a day       |           |
| take 1 or 2 4 times/day                         |      | 1 or 2    | 4 times           |           |
| inhale 2 doses as needed                        |      | 2         |                   |           |
| apply twice daily                               |      | 1         | twice             |           |
| take one as directed                            |      | 1         |                   |           |
| two four times a day when required              |      | 2         | four times a day  |           |
| 1 every night                                   |      | 1         | every night       |           |
| use as directed                                 |      |           |                   |           |
| 1 at night                                      |      | 1         | at night          |           |
| take one 4 times/day                            |      | 1         | 4 times           |           |
| inhale 2 doses twice daily                      |      | 2         | twice             |           |
| one 5ml spoonsful to be taken three times a day | ml   | 1 5 ml    | three times a day |           |
| one or two four times a day when required       |      | 1 or 2    | four times a day  |           |
| two three times a day                           |      | 2         | three times a day |           |
| apply 3 times/day                               |      | 1         | 3 times           |           |
| two every night                                 |      | 2         | every night       |           |
| apply as needed                                 |      | 1         |                   |           |
| two puff twice a day                            | puff | 2         | twice a day       |           |
| two four times a day                            |      | 2         | four times a day  |           |
| take two daily                                  |      | 2         |                   |           |
| 1 od                                            |      | 1 1       | od                |           |
| 1 in the morning                                |      | 1         | morning           |           |
| take one twice a day                            |      | 1         | twice a day       |           |

</div>

## Development notes

Always use non-capturing groups, `(?: )` in regular expressions. Capture
groups slow down pattern matching and extraction considerably (unit
tests took 1.5s with capture groups; setting all groups to non-capturing
reduced runtime to be nearly instantaneous).

## Contributors

Maintained by David Selby (`david.selby@manchester.ac.uk`), Belay Birlie
and Katherine Dempsey.
