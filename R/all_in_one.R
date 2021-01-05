# As of 4/1/2020 this script overrules ALL other R script files in this package.

# Functional redesign.
# Process is as follows.
# 1. Sanitize text, removing punctuation, adding spaces between numbers, etc.
# 2. Extract optional / as required text and remove it.
# 3. Extract frequency text and remove it.
# 4. Extract dose number + unit text and remove it.
# 5. Extract dose interval text and remove it.

# Data such as keywords and common regular expressions can just be assigned in
# package scope. No need to wrap inside a function unless it's exported.

# DN = number in 1 dose
# DOSE.UNIT = measurement unit for dose number
# DF = number of doses in a day
# DI = days between doses
# OPTIONAL = can a dose be zero?

#' Example text prescriptions (adapted from common dosages)
examples <- c(
  '1 tablet to be taken daily',
  '2.5ml four times a day when required',
  '1.25mls three times a day',
  'take 10mls q.d.s. p.r.n.',
  'take 1 or 2 4 times/day',
  '2x5ml spoon 4 times/day',
  'take 2 tablets every six hours max eight in twenty four hours',
  '1 tab nocte twenty eight tablets',
  '1-2 four times a day when required',
  'take one twice daily',
  '1 q4h prn',
  'take two every three days',
  'five every week',
  'every 72 hours',
  '1 x 5 ml spoon 4 / day for 10 days')

# DN = 1
# DOSE.UNIT = tablet
# DF = 1 ("daily")
# DI = 1 ("daily")
# OPTIONAL = no

# Use a domain-specific language and then use str_replace_all
# to replace with the actual regular expressions?
# e.g. '{uber_number} x {timely|latin|everyxunit}'
# (But glue::glue already achieves this.)

# numbers <- c('one', 'two', 'three', 'four(?:teen)?', 'five',
#              'six(?:teen)?', 'seven(?:teen)?', 'eight(?:een)?',
#              'nine(?:teen)', 'ten', 'eleven', 'twelve', 'thirteen',
#              'fifteen', 'twenty', '\\d{1,5}\\.?\\d{0,5}', '(?:a )?half')

english_nums <- paste(
  'one(?: half)?', 'two', 'three', 'four(?:teen)?', 'five',
  'six(?:teen)?', 'seven(?:teen)?', 'eight(?:een)?',
  'nine(?:teen)', 'ten', 'eleven', 'twelve', 'thirt(?:een|y)',
  'fifteen',
  'twenty(?:[- ](?:one|two|three|four|five|six|seven|eight|nine))',
  '(?:a )?half', ' (?:and|&) (?:a|one) half', 'once', 'twice', 'thrice',
  sep = '|')

#' Look up numbers by their English names
#'
#' This does the reverse of the \code{english} package, i.e. given a string
#' such as "fifteen" it will return the decimal value "15".
#'
#' This function does the bare minimum for now, and only handles numbers from
#' one to thirty (1-30) as well as supporting phrases like "and a half".
#' If there is a need to expand the scope to cover more cases, it may be worth
#' spinning into its own dedicated package. (There are examples on the web
#' for how to handle numbers up to several trillion, though not necessarily
#' within a string that contains non-number words as well).
#'
#' @note
#' This function is not designed for general inputs. It's assumed you have
#' extracted the relevant word or phrase already.
#'
#' @seealso The \code{words2number} package (which is experimental).
#'
#' @param phrase A character vector comprising English names of numbers
#'
#' @return The decimal representation (as a string).
#' If a number is not yet reported, returns \code{NA}.
english2int <- function(phrase) {
  phrase <- gsub(' ', '-', phrase)
  lookup <- setNames(1:30, english::words(1:30))
  lookup <- c(lookup, c(once = 1, twice = 2, thrice = 3,
                        # Careful: existing regexes won't pick up numbers
                        # starting with a decimal point.
                        '-and-a-half' = '.5',
                        '-and-one-half' = '.5',
                        '-&-a-half' = '.5',
                        '-&-one-half' = '.5',
                        'a-half' = '0.5',
                        'one-half' = '0.5',
                        half = '0.5'))
  lookup[phrase]
}

#' Convert Latin phrases to daily frequencies
#'
#' Tip: use \code{latin2intdaily(TRUE)} to get the whole list.
#'
#' @seealso \code{\link{english2int}}
#'
#' @param phrase a Latin pharmaceutical phrase, such as 'q8h'
#' @return Numeric vector corresponding to numbers of times per day
latin2intdaily <- function(phrase) {
  lookup <-
    c(`dieb alt` = 1, `alt sh` = 12, nocte = 1, noct = 1, mane = 1,
      q8h = 3, q7h = 3.4, q6h = 4, q5h = 4.8, q4h = 6, q3h = 8, bds = 2,
      noc = 1, q2h = 12, q1h = 24, qhs = 1, qpm = 1, qds = 4, qam = 1,
      qad = 1, opd = 1, eod = 1, alt = 1, bid = 2, bis = 2, q1d = 1,
      qid = 4, qod = 1, qqh = 6, qwk = 1, sid = 1, tiw = 3, tid = 3,
      tds = 3, td = 3, qh = 24, pm = 1, om = 1, on = 1, hs = 1, bt = 1,
      am = 1, qd = 1, bd = 2, od = 1)
  lookup[phrase]
}

#' Clean up raw prescription freetext
#'
#' @examples
#' clean_prescription_text(examples)
#' @importFrom stringr str_replace_all str_remove_all
#' @importFrom magrittr %>%
#'
#' @return a character vector the same length as \code{txt}
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
    str_replace_all(english_nums, english2int) %>%
    str_replace_all('([0-9]+) ?(-|to|or) ?([0-9]+)', '\\1 - \\3') %>%
    # Remove spaces from Latin expressions like "q1h"
    str_replace_all('(\\bq) ([1-8]) ([dh])', '\\1\\2\\3') %>%
    # Add spaces to really terse Latin expressions like '100mgbd'
    str_replace_all('(\\w+)(bd|[qt]ds)\\b', '\\1 \\2') %>%
    # Remove nuisance phrases like "weekly dispensing"
    str_remove_all('weekly (?:script|disp(?:en[sc](?:ed?|ing))?)') %>%
    str_remove_all('(?:script|disp(?:en[sc](?:ed?|ing))?) weekly') %>%
    # Trim whitespace
    str_replace_all('\\s+', ' ') %>% trimws
}

# Rather than try to match every possible variant, the first pass should
# simplify the text, then the rules for pattern matching can be much simpler.
# For example, rather than try to match mg, mgs, milligram, milligrams etc.
# we will first standardise the text so it ALWAYS says mg if any thing is found
# Even better: we could replace all units with "unit" so following regexes
# can treat all units the same (once we've extracted unit).

# uber_number <- '\\d{1,5}\\.?\\d{0,5}(?: or| to| [/-])?(?: \\d{1,5}\\.?\\d{0,5})?'
#latin_nums <- paste(names(latin2intdaily(T)), collapse = '|')

#' @import magrittr
#' @import stringr
extract_frequency_text <- function(txt) {
  original <- txt
  clean_prescription_text(txt) %>%
    str_replace_all('times(/| a| per) ?', '/ ') %>%
    # Invert hourly intervals to daily rates.
    str_replace_all('every \\d+ h(?:ou)?r?s?',
                    function(x) {
                      n <- as.numeric(str_extract(x, '\\d+'))
                      if (n >= 24) return(x)
                      paste(24 / n, '/ day') }) %>%
    # Convert "x daily" to standardised format.
    str_replace_all('([0-9]+) ((times )?daily|a day)', '\\1 / day') %>%
    # Just "daily" = 1 / day.
    str_replace_all('daily|every (day|morning|night)', '1 / day') %>%
    # Convert Latin phrases, e.g. q4h.
    str_replace_all(
      paste0('\\b(', paste(names(latin2intdaily(T)), collapse = '|'), ')\\b'),
      function(x) paste(latin2intdaily(x), '/ day')) -> txt
  freq <- str_extract(txt, '(?:\\d+\\.?\\d* ?[-] ?)?\\d+\\.?\\d*(?= / day)')
  #txt <- str_remove_all(txt, '\\d+\\.?\\d* / day ?') %>% trimws
  data.frame(freq_extracted = txt, freq)
}

extract_frequency_text(examples)

# Input is the output of extract_frequency_text
#' @import stringr
extract_interval_text <- function(txt) {
  txt %>%
    str_replace_all('(every|per) week|weekly', 'every 7 days') %>%
    str_replace_all(paste('(every )?(on )?alt(ernate)? (day|night|morning)s?',
                          'every (other|second) day', sep = '|'),
                    'every 2 days') %>%
    str_replace_all('every third (day|night|morning)', 'every 3 days') %>%
    str_replace_all('every \\d+ h(?:ou)?r?s?',
                    function(x) {
                      n <- as.numeric(str_extract(x, '\\d+'))
                      paste('every', n / 24, 'days')
                    }) %>%
    str_replace_all('(?:\\d+\\.?\\d* ?[-] ?)?\\d+\\.?\\d* / day ?', 'every 1 days') -> txt
  interval <- as.numeric(str_extract(txt, '(?<=every )\\d(?= days)'))
  txt <- str_remove_all(txt, 'every \\d+ days ?') %>% trimws
  data.frame(int_extracted = txt, interval)
}

examples %>%
  extract_frequency_text %>%
  cbind(extract_interval_text(.$freq_extracted))

extract_unit_text <- function(txt) {
  unit_names <- list(
    # based on add_dose_unit.py
    # maybe automatically drop trailing 's' and strip spaces.
    mg = c('mg', 'mgs', 'milligram', 'milligrams', 'milli gram', 'milligrams',
           'milli grams'),
    g = c('grams', 'grammes', 'gram', 'gramme', 'g', 'gm', 'gms'),
    tab = c('tabs', 'tab', 'tablet', 'tablets'),
    cap = c('cap', 'caps', 'capsule', 'capsules'),
    ml = c('millilitres', 'millilitre', 'mls', 'ml', 'milli litre',
           'milli litres', 'msl', 'milliliters', 'milliliter', 'milli liter',
           'milli liters'),
    iu = 'iu',
    sachet = c('sachet', 'sachets'),
    pastille = c('pastille', 'pastilles'),
    pill = c('pill', 'pills'),
    drop = c('drop', 'drops', 'dr', 'drp', 'drps'),
    puff = c('puff', 'puffs', 'puf', 'pufs'),
    blister = c('blister', 'blisters'),
    amp = c('amp', 'amps', 'ampoule', 'ampoules'),
    spray = c('spray', 'sprays', 'spr'),
    mcg = c('micg', 'micro g', 'microgram', 'micrograms', 'mcgs', 'micro gram',
            'micro grams', 'mcg', 'ug'),
    ng = c('ng', 'nang', 'nanogram', 'nano gram', 'nanograms', 'nano grams'),
    suppos = c('suppositor', 'suppository'),
    vial = c('vial', 'vials'),
    patch = c('patch', 'patches'),
    ounce = c('ounce', 'ounces', 'oz'),
    unit = c('unit', 'units'),
    lozenge = c('lozenge', 'lozenges', 'losenge', 'losenges'),
    pack = c('packs', 'packets', 'pack', 'packet'),
    capful = c('capfuls', 'capfulls', 'capful', 'capfull')
  )
  unit_lookup <- setNames(rep(names(unit_names), lengths(unit_names)),
                          unlist(unit_names, FALSE, FALSE))
  unit_pattern <- paste0(
    '\\b(',
    paste(sep = '|', 'capsules?', 'caps?', 'sachets?', 'dro?ps?', 'dr', 'ampoules?',
          'amps?', 'suppository?', 'pills?', 'blisters?', 'sprays?',
          '(?<=[0-9] )spr', '(?<=[0-9] )suppos',
          'tab(?:let)?s?', 'puff?s?', 'mls?', 'msl',
          'millil(?:it(?:er|re)s?)?',
          'grams?', 'gms?', 'g', 'mcgs?', 'micro ?grams?', 'milli ?grams?',
          'mgs?', 'inh', 'capfull?s?', 'vials?', 'patch(?:es)?',
          'bolus(?:es)?', 'lo[sz]enges?', 'ounces?', 'pack(?:et)?s?',
          'units?', 'pastilles?', 'ounces?'),
    ')\\b')

  txt %>%
    str_replace_all(unit_pattern, function(x) unit_lookup[x]) %>%
    str_extract(unit_pattern)
}

examples %>%
  extract_frequency_text %>%
  cbind(extract_interval_text(.$freq_extracted)) %>%
  transform(unit = extract_unit_text(int_extracted))

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

examples %>%
  extract_frequency_text %>%
  cbind(extract_interval_text(.$freq_extracted)) %>%
  transform(unit = extract_unit_text(int_extracted),
            number = extract_dose_number(int_extracted))

sample(common_dosages$PRESCRIPTION, 1000) %>%
  extract_frequency_text %>%
  cbind(extract_interval_text(.$freq_extracted)) %>%
  dplyr::mutate(unit = extract_unit_text(int_extracted),
                number = extract_dose_number(int_extracted)) %>%
  View
