#' Guess dosage from prescription free text
#'
#' @examples
#' View(guess_prescription(common_dosages[1:1000, 'PRESCRIPTION']))
#' @export
guess_prescription <- function(text) {
  data.frame(raw = text,
             clean = sanitize_prescription(text),
             number = guess_number(text),
             unit = guess_dose_unit(text),
             freq = guess_frequency(text),
             interval = guess_interval(text),
             row.names = NULL,
             stringsAsFactors = FALSE)
}

#' Extract drug information
#'
#' Based on \code{drug_information_extraction.py}.
#'
#' The aim of this function is to replicate the abovementioned Python script.
#' For now it will perform the following tasks:
#' \itemize{
#' \item Coerce to lower case
#' \item Trim leading and trailing whitespace
#' \item Replace pipes \code{|} with spaces (line 30)
#' \item Replace a sequence \code{numberxword} with \code{number x word}
#' \item Replace a sequence \code{numberword} with \code{number word}
#' \item Replace a sequence \code{wordnumberword} with \code{word number word}
#' \item Replace a sequence \code{numberwordnumber} with \code{number word number}
#' \item Split up sequences with units (see line 40 of \code{.py} script)
#' }
#' We have also added some stuff that may not have been in the original script:
#' replacing double spaces, tabs or newline characters with single spaces.
#'
#' @examples
#' sanitize_prescription(c('2mg per day', '1xdaily', 'a2b', '  double  spaced  ',
#'                      'newline\ntab\treturn\r', '2by14', 'take q4d'))
#'
#' @importFrom magrittr %>%
#' @importFrom stringr str_replace_all
#' @importFrom glue glue
#' @export
sanitize_prescription <- function(x) {
  unit_pattern <-
    glue::glue('(\\d+)(\\s+)({dose_dict("unit")})(sid|bd)')
  tolower(x) %>%
    stringr::str_replace_all('\\|', ' ') %>%
    trimws() %>%
    stringr::str_replace_all('([0-9]+)(x|/)([a-z]+)', '\\1 \\2 \\3') %>%
    stringr::str_replace_all('([0-9]+)([a-z]+)', '\\1 \\2') %>%
    stringr::str_replace_all('([a-z]+)([0-9]+)', '\\1 \\2') %>%
    stringr::str_replace_all('([a-z]+)([0-9]+)([a-z]+)', '\\1 \\2 \\3') %>%
    stringr::str_replace_all('(\\bq) ([1-8]) ([dh])', '\\1\\2\\3') %>% # preserve 'q1h'
    stringr::str_replace_all(unit_pattern, '\\1 \\2 \\3 \\4') %>%
    stringr::str_replace_all('\\s+', ' ')
}

#' Guess dosage number (rewriting from scratch)
#'
#' Seeing if we can do better than original without copying over code.
#'
#' @examples
#' x <- data.frame(text = common_dosages[1:1000, 'PRESCRIPTION'],
#'            old_min = common_dosages[1:1000, 'DN.MIN'],
#'            old_max = common_dosages[1:1000, 'DN.MAX'],
#'            new = unname(guess_number(common_dosages[1:1000, 'PRESCRIPTION'])))
#' @importFrom stringr str_replace_all str_extract_all
#' @importFrom glue glue
#' @export
guess_number <- function(text) {
  # Might rename this to 'guess dosage' so it sounds less ambiguous
  std_text <- sanitize_prescription(text)
  std_text <- str_replace_all(std_text, '(apply|to be applied)( to the affected part)?', 'one')
  std_text <- str_replace_all(std_text, 'to be taken ', '')
  std_text <- str_replace_all(std_text, '\\(s\\)', '')
  nums <- dose_dict('numbers')
  latin <- dose_dict('latin')
  freq <- regex_or('(?:every|each|at|in the) (?:day|night|morning|evening)',
                   'daily', 'bd', 'nocte', 'mane', '[qt]\\.?d\\.?s\\.?',
                   '(?:once|twice|(?:up to )?{nums*}(?:-{nums*})? times) ?(?:daily|every day|(?:a|per|/) ?day)',
                   'p\\.?r\\.?n\\.?', '\\dhrly', 'every (?:{nums*}|\\d-\\d+) h(?:ou)?rs?(?: when required)?',
                   '(?:take )?(?:when|as) (?:directed|required|needed)',
                   .sep = '|')
  patterns <- glue::glue('{nums*}(?:(?: or |-){nums*})?(?= (?!hours?)(?:\\w+ )?{freq*})',
                         '{nums*} \\d-?\\d? (?:mls?|msl)(?= spoon)',
                         '{nums*}(?= a day)',
                         '{nums*} x \\d+\\.?\\d* (?:mls?|msl)',
                         '^\\d(?:or|[.-])?\\d? {latin*}',
                         .sep = '|')
  number_matches <- stringr::str_extract_all(std_text, patterns, simplify = TRUE)
  longest_number <- apply(number_matches, 1, function(x) x[which.max(nchar(x))])
  output <- word2num(longest_number)
  setNames(output, text)
}

#' Guess frequency of dose
#'
#' @examples
#' guess_frequency(
#' c('5 daily', 'four monthly', '5 q4h', 'two x3hly', 'take 1 weekly', # NB initial verb
#'   '3 / day', '2/week', '3x per day', 'five q2h', 'three am', 'one daily'))
#'
#' x <- data.frame(text = common_dosages[1:1000, 'PRESCRIPTION'],
#'            old_min = common_dosages[1:1000, 'DF.MIN'],
#'            old_max = common_dosages[1:1000, 'DF.MAX'],
#'            new = unname(guess_frequency(common_dosages[1:1000, 'PRESCRIPTION'])))
#' View(x)
#' @importFrom stringr str_extract_all
#' @export
guess_frequency <- function(text) {
  std_text <- sanitize_prescription(text)
  df_unit <- dose_dict('unit')
  df_meal <- dose_dict('meal')
  df_meal_how <- dose_dict('meal_how')
  df_time_unit <- dose_dict('time_unit')
  df_every <- dose_dict('every')
  df_when <- dose_dict('when')
  df_timely <- dose_dict('timely')
  df_period <- dose_dict('period')
  nums <- dose_dict('numbers')

  df_times <- regex_or('once', 'twice', 'thrice', '(?:up ?)?(?:to )?{nums*} times?', .sep = '|')
  df_per_time_unit <- regex_or(
    '{df_every*} {nums*}(?: .)? {df_time_unit*}',
    '(?:{df_every}|{df_when})((?: .)? )?{df_period*}', ##
    '{df_when*} \\d{{1,4}}',
    '{df_times*}(?:(?: {df_every*})? {df_time_unit*})?',
    .sep = '|'
  )
  df_uber_number <- regex_or('{nums*}(?: (?:or|-|to|/))?(?: {nums*})?')

  df_latin <- dose_dict('latin')
  df_everyxunit <- regex_or('{df_every*} {df_period*} {df_per_time_unit*}')

  patterns <- regex_or(
    # are these ^ anchors necessary?
    '(?<=^{df_uber_number} (?:x ?)?)(?:(?:\\d )?)(?:{df_timely}|{df_latin*}|{df_everyxunit})',
    '^{df_uber_number}(?= (?:x )?{df_every*} {df_time_unit*})', # doesn't seem right - l167 of frequency_phase.mixup - doesn't capture time unit.
    '^{df_uber_number} ms?ls? [ob]d',
    '^{df_uber_number} {df_per_time_unit*} {df_uber_number*} {df_per_time_unit*}',
    '{df_per_time_unit*}',
    .sep = '|'
  )
  #message(patterns) # debug
  matches <- stringr::str_extract_all(std_text, patterns, simplify = TRUE)
  longest <- apply(matches, 1, function(x) x[which.max(nchar(x))])
  setNames(longest, text)
}

#' Convert extracted into numeric daily frequency of dose
#'
#' Based on the script \code{freq_word_con.py} in original algorithm.
#'
#' @param string Text extracted by \code{\link{guess_frequency}} (probably should rename this \code{extract_} or \code{match_})
#'
#' @return Estimated (minimum and maximum) times per day dose is administered, as numeric value.
#' @export
parse_frequency <- function(string) {
  # 3 times per day
  # 3 times daily
  # 3 every day
  # interval is converted later, in conv_dose_interval.py and add_dose_interval.py
}

#' Estimate the time between doses for a prescription.
#'
#' @export
guess_interval <- function(text) {
  std_text <- sanitize_prescription(text)
  std_text
}
