#' Example freetext prescriptions
#'
#' Adapted from CPRD common dosages
#'
#' @export
example_prescriptions <- c(
  '1 tablet to be taken daily',
  '2.5ml four times a day when required',
  '1.25mls three times a day',
  'take 10mls q.d.s. p.r.n.',
  'take 1 or 2 4 times/day',
  '2x5ml spoon 4 times/day',
  'take 2 tablets every six hours max eight in twenty four hours',
  '1 tab nocte twenty eight tablets',
  '1-2 four times a day when required',
  'take one twice daily', # WARNING: converts to 'take 1 2 daily' - ambiguous
  '1 q4h prn',
  'take two every three days',
  'five every week',
  'every 72 hours',
  '1 x 5 ml spoon 4 / day for 10 days',
  'two to three times a day',
  'three times a week',
  'three 5ml spoonsful to be taken four times a day after food')

#' Clean up raw prescription freetext
#'
#' @param txt a character vector
#'
#' @return a character vector the same length as \code{txt}
#'
#' @examples
#' clean_prescription_text(example_prescriptions)
#'
#' @importFrom stringr str_replace_all str_remove_all str_squish
#' @importFrom magrittr %>%
#'
#' @export
clean_prescription_text <- function(txt) {
  # Make lower case.
  tolower(txt) %>%
    # Add spaces to phrases like "5/day".
    str_replace_all('([0-9]+)([x/])([a-z]+)', '\\1 \\2 \\3') %>%
    # Add spaces between words and numbers ("5ml" -> "5 ml").
    str_replace_all('([a-z]+)([0-9]+)', '\\1 \\2') %>%
    str_replace_all('([0-9]+)([a-z]+)', '\\1 \\2') %>%
    # Strip out all dots except decimal points.
    str_remove_all('(?!<=[0-9])\\.(?![0-9])') %>%
    str_remove_all('(?<=[a-z])\\.(?=[0-9])') %>%
    # Convert English numbers ("one", "two") to decimals (1, 2).
    #str_replace_all(english_nums, english2int) %>%
    replace_numbers %>%
    str_replace_all('([0-9]+) ?(-|to|or) ?([0-9]+)', '\\1 - \\3') %>%
    # Remove spaces from Latin expressions like "q1h"
    str_replace_all('(\\bq) ([1-8]) ([dh])', '\\1\\2\\3') %>%
    # Add spaces to really terse Latin expressions like '100mgbd'
    str_replace_all('(\\w+)(bd|[qt]ds)\\b', '\\1 \\2') %>%
    # Remove irrelevant nuisance phrases like "weekly dispensing"
    str_remove_all('weekly (?:script|disp(?:en[sc](?:ed?|ing))?)') %>%
    str_remove_all('(?:script|disp(?:en[sc](?:ed?|ing))?) weekly') %>%
    # Standardise dose units
    str_replace_all(drug_units) %>%
    # Trim whitespace.
    str_squish
}

# Rather than try to match every possible variant, the first pass should
# simplify the text, then the rules for pattern matching can be much simpler.
# For example, rather than try to match mg, mgs, milligram, milligrams etc.
# we will first standardise the text so it ALWAYS says mg if any thing is found
# Even better: we could replace all units with "unit" so following regexes
# can treat all units the same (once we've extracted unit).

# uber_number <- '\\d{1,5}\\.?\\d{0,5}(?: or| to| [/-])?(?: \\d{1,5}\\.?\\d{0,5})?'
#latin_nums <- paste(names(latin2intdaily(T)), collapse = '|')

#' Convert hourly to daily frequency
#'
#' @examples
#' hourly_to_daily('every 4 hours')
#'
#' @importFrom stringr str_extract
hourly_to_daily <- function(everyDhrs) {
  n <- as.numeric(str_extract(everyDhrs, '\\d+'))
  if (n >= 24) return(paste('every', n / 24, 'days'))
  paste(24 / n, '/ day')
}

weekly_to_daily <- function(Dperweek) {
  n <- 7 / as.numeric(str_extract(Dperweek, '\\d+'))
  min <- floor(n)
  max <- ceiling(n)
  if (min == max) {
    paste('every', n, 'days')
  } else
    paste('every', min, '-', max, 'days')
}

#' @importFrom stringr str_replace
multiply_dose <- function(axb) {
  expr <- str_replace(axb, '(\\d+[.]?\\d*) x (\\d+[.]?\\d*)', '\\1 * \\2')
  eval(parse(text = expr))
}

#' Extract dose frequency information from freetext prescription
#'
#' @param txt A character vector of freetext prescriptions
#'
#' @examples
#' extract_dose_frequency_interval(example_prescriptions)
#'
#' @import magrittr
#' @import stringr
#' @export
extract_dose_frequency_interval <- function(txt) {
  processed <- clean_prescription_text(txt) %>%
    str_replace_all('times(/| a| per) ?', '/ ') %>%
    # Translate from Latin to English.
    str_replace_all(latin_medical_terms) %>%
    # Invert hourly intervals to daily rates.
    str_replace_all('every \\d+ h(?:ou)?r?s?', hourly_to_daily) %>%
    # once, twice, thrice -> n times
    str_replace_all(setNames(paste(1:3, 'times'),
                             c('once', 'twice', 'thrice'))) %>%
    # Convert "x daily" to standardised format.
    str_replace_all('([0-9]+) ((times )?daily|a day)', '\\1 / day') %>%
    # Just "daily" = 1 / day (previous line must run first)
    str_replace_all('daily|every (day|morning|night)', '1 / day') %>%
    # Convert daily intervals.
    str_replace_all('(?:every|per) week|weekly', 'every 7 days') %>%
    str_replace_all('(every )?(on )?alt(ernate)? (day|night|morning)s?|every (other|second) day',
                    'every 2 days') %>%
    str_replace_all('every third (day|night|morning)', 'every 3 days') %>%
    str_replace_all('[0-9]+ / week', weekly_to_daily) %>%
    # Convert phrases like "one 5 ml spoonful" to "1 x 5 ml spoonful"
    str_replace_all('(\\d+[.]?\\d*) (\\d+[.]?\\d* ml spoon)', '\\1 x \\2')

  # NOTE: only retrieves first match.
  freq <- str_extract(
    processed, '(?:\\d+\\.?\\d* ?[-] ?)?\\d+\\.?\\d*(?= / day)') %>%
    str_remove_all(' ')
  itvl <- str_extract(
    processed, '(?<=every )(?:\\d+\\.?\\d* ?[-] ?)?\\d+\\.?\\d*(?= days)') %>%
    str_remove_all(' ')

  # If freq specified but not interval (or vice versa) then implicit = 1.
  # (This may be a risky assumption.)
  itvl <- ifelse(is.na(itvl) & freq >= 1, 1, itvl)
  freq <- ifelse(is.na(freq) & itvl >= 1, 1, freq)

  # Note: only removes first match.
  output <- processed %>%
    str_remove('(?:\\d+\\.?\\d* ?[-] ?)?\\d+\\.?\\d* / (?:day|week)') %>%
    str_remove('every (?:\\d+\\.?\\d* ?[-] ?)?\\d+\\.?\\d* days') %>%
    str_squish

  numeric_range <- '\\d+[.]?\\d*(?: (?:x|-) \\d+[.]?\\d*)?'

  dose <- str_extract(
    output, sprintf('^%s(?: (?:%s))?|%s(?: (?:%s)|$)',
                    numeric_range, paste(drug_units, collapse = '|'),
                    numeric_range, paste(drug_units, collapse = '|'))
  ) %>%
    # Convert doses like "a x b" to the arithmetic result a*b.
    str_replace_all('\\d+[.]?\\d* x \\d+[.]?\\d*', multiply_dose)

  data.frame(raw = txt, output, freq, itvl, dose)
}

#' Extract units of dose from freetext prescriptions.
#'
#' If there are multiple units in a string, only the first is returned.
#'
#' @param txt a character vector
#'
#' @return A character vector the same length as \code{txt}, containing
#' standardised units, or \code{NA} if no units were found in the prescription.
#'
#' @examples
#' extract_dose_unit(example_prescriptions)
#'
#' Based on \code{add_dose_unit.py} from original Python/Java algorithm.
#'
#' @importFrom stringr str_replace_all str_extract
extract_dose_unit <- function(txt) {
  standardised <- stringr::str_replace_all(txt, drug_units)
  stringr::str_extract(standardised, paste(drug_units, collapse = '|'))
}

# examples %>%
#   extract_frequency_text %>%
#   cbind(extract_interval_text(.$freq_extracted)) %>%
#   transform(unit = extract_unit_text(int_extracted))

#' @import stringr
extract_dose_number <- function(txt) {
  # TODO: complicated expressions like "1 - 2 5 ml spoonsful"
  txt %>%
    str_extract_all(
      paste(sep = '|',
            '\\d+\\.?\\d*(?: - \\d)? \\d+ mls? spoon', # spoons
            '\\d+\\.?\\d* x \\d\\.?\\d*(?= \\w+)',
            '(?<=\\w{1,10} )\\d+( ?[.-] ?)?\\d*$',
            '^\\d+( ?[.-] ?)?\\d*',
            '(?<=^\\w{1,10} )\\d+( ?[.-] ?)?\\d*'),
      simplify = TRUE) %>%
    apply(1, function(x) x[which.max(nchar(x))]) %>%
    str_replace_all(' (x|-) ', '\\1') %>%
    str_remove_all('-$| mls? spoon') %>%
    # Advanced: convert "x - y" to "c(x, y)" and "a x b" or "a b" to "a * b".
    str_replace_all('(\\d+\\.?\\d*)-(\\d+\\.?\\d*)', 'c(\\1,\\2)') %>%
    # Evaluate the expression generated.
    trimws %>% str_replace_all('[ x]', '*') -> expr
  # Must return a list to include the min/max values.
  lapply(expr, function(x) eval(parse(text = x)))
}

# examples %>%
#   extract_frequency_text %>%
#   cbind(extract_interval_text(.$freq_extracted)) %>%
#   transform(unit = extract_unit_text(int_extracted),
#             number = extract_dose_number(int_extracted))
#
# sample(common_dosages$PRESCRIPTION, 1000) %>%
#   extract_frequency_text %>%
#   cbind(extract_interval_text(.$freq_extracted)) %>%
#   dplyr::mutate(unit = extract_unit_text(int_extracted),
#                 number = extract_dose_number(int_extracted)) %>%
#   View
