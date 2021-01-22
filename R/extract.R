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
  'three 5ml spoonsful to be taken four times a day after food',
  'take one or two every 4-6 hrs',
  '5ml 3 hrly when required',
  'one every morning to reduce bp')

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
    str_replace_all('([0-9]+) ?(-|(?:up )?to|or) ?([0-9]+)', '\\1 - \\3') %>%
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

#' Extract dose frequency information from freetext prescription
#'
#' This is the main workhorse function for the \code{doseminer} package.
#'
#' @param txt A character vector of freetext prescriptions
#'
#' @examples
#' extract_from_prescription(example_prescriptions)
#'
#' @import magrittr stringr
#' @export
extract_from_prescription <- function(txt) {
  processed <- clean_prescription_text(txt) %>%
    # Translate from Latin to English.
    str_replace_all(latin_medical_terms) %>%
    str_remove_all('as (?:directed|advised|shown(?: on the pack(?:et)?)?)') %>%
    # "Up to (a maximum of) n" = 0 - n
    str_replace_all('\\bup to (?:a maximum of )?|\\bmax(?:imum)?(?: of)? ', '0 - ') %>%
    # Invert hourly intervals to daily rates.
    str_replace_all('every \\d+(?: - \\d+)? h(?:ou)?r?s?|\\d h(?:(?:ou)?r)?ly',
                    hourly_to_daily) %>%
    # once, twice, thrice -> n times
    str_replace_all(setNames(paste(1:3, 'times'),
                             c('once', 'twice', 'thrice'))) %>%
    # Convert "x daily" to standardised format.
    str_replace_all('times(?:/| a| per) ?', '/ ') %>%
    str_replace_all('(?<!take )([0-9]+) (?:(?:times )?daily|a day)', '\\1 / day') %>%
    # Just "daily" = 1 / day (previous line must run first)
    str_replace_all('daily|(?:every|each|at|in the) (?:day|morning|night|bedtime)',
                    '1 / day') %>%
    # Convert daily intervals.
    str_replace_all('(?:every|per) week|weekly', 'every 7 days') %>%
    str_replace_all('(?:every )?(?:on )?alt(?:ernate)? (?:day|night|morning)s?|every (?:other|second) day',
                    'every 2 days') %>%
    str_replace_all('every third (?:day|night|morning)', 'every 3 days') %>%
    str_replace_all('[0-9]+ / week', weekly_to_daily) %>%
    # Convert phrases like "one 5 ml spoonful" to "1 x 5 ml spoonful"
    str_replace_all('(\\d+[.]?\\d*) (\\d+[.]?\\d* ml spoon)', '\\1 x \\2') %>%
    # "Apply to the affected part" = 1 'dose'
    str_replace_all('(?:apply|to be applied)(?: to the affected (?:area|part))?',
                    '1 application')

  # NOTE: only retrieves first match.
  freq <- str_extract(
    processed, '(?:\\d+\\.?\\d* ?[-] ?)?\\d+\\.?\\d*(?= / day)') %>%
    str_remove_all(' ')
  itvl <- str_extract(
    processed, '(?<=every )(?:\\d+\\.?\\d* ?[-] ?)?\\d+\\.?\\d*(?= days)') %>%
    str_remove_all(' ')

  # Change maximum daily frequency if there is phrase like "max N in 24 hrs"
  # max_freq <- str_extract(processed,
  #   '(?<=0 - )\\d+(?=(?: \\w+)? (?:in (?:24 h(?:ou)?rs|1 day)|(?:1 )?/ day))')
  # freq <- ifelse(is.na(max_freq), freq,
  #                paste(str_match(freq, '^\\d+'), max_freq, sep = '-'))

  # If freq specified but not interval (or vice versa) then implicit = 1.
  # (This may be a risky assumption.)
  itvl <- ifelse(is.na(itvl) & freq >= 1, 1, itvl)
  freq <- ifelse(is.na(freq) & itvl >= 1, 1, freq)

  # Note: only removes first match.
  output <- processed %>%
    str_remove('(?:\\d+\\.?\\d* ?[-] ?)?\\d+\\.?\\d* / (?:day|week)') %>%
    str_remove('every (?:\\d+\\.?\\d* ?[-] ?)?\\d+\\.?\\d* days') %>%
    str_remove('(?:as|when|if) (?:req(?:uire)?d?|ne(?:eded|cessary))') %>%
    str_squish

  numeric_range <- '\\d+[.]?\\d*(?: (?:x|-) \\d+[.]?\\d*)?'

  dose <- str_extract(
    output, sprintf('^%s(?: (?:%s))?|%s(?: (?:%s)|$)',
                    numeric_range, paste(drug_units, collapse = '|'),
                    numeric_range, paste(drug_units, collapse = '|'))
  ) %>%
    # Convert doses like "a x b" to the arithmetic result a*b.
    str_replace_all('\\d+[.]?\\d* x \\d+[.]?\\d*', multiply_dose) %>%
    str_extract(numeric_range) %>%
    str_remove_all(' ')

  optional <- as.integer(
    (!is.na(freq) & str_detect(freq, '^0-')) |
      (!is.na(dose) & str_detect(dose, '^0-')) |
      str_detect(processed,
                 '(?:as|when|if) (?:req(?:uire)?d|ne(?:eded|cessary))')
  )

  unit <- extract_dose_unit(output)

  data.frame(raw = txt, output, freq, itvl, dose, unit, optional)
}

#' Convert hourly to daily frequency
#'
#' @examples
#' hourly_to_daily('every 4 hours')
#' hourly_to_daily('every 3 - 4 hours')
#' hourly_to_daily('every 36 - 72 hours')
#'
#' @importFrom stringr str_extract_all
hourly_to_daily <- function(txt) {
  n <- as.numeric(str_extract_all(txt, '\\d+')[[1]])
  if (any(n >= 24))
    return(paste('every', paste(n / 24, collapse = ' - '), 'days'))
  paste(paste(sort(24 / n), collapse = ' - '), '/ day')
}

#' Convert weekly interval to daily interval
#'
#' @examples
#' weekly_to_daily('3 / week')
#'
#' @importFrom stringr str_extract
weekly_to_daily <- function(Dperweek) {
  n <- 7 / as.numeric(str_extract(Dperweek, '\\d+'))
  min <- floor(n)
  max <- ceiling(n)
  if (min == max) {
    paste('every', n, 'days')
  } else paste('every', min, '-', max, 'days')
}
