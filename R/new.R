# make this a function so we don't have to worry about code order
dose_unit <- paste('capsules?', 'caps?', 'sachets?', 'dro?ps?', 'dr', 'ampoules?',
               'amps?', 'suppository?', 'pills?', 'blisters?', 'sprays?',
               '(?<=[0-9] )spr', '(?<=[0-9] )suppos',
               'tab(?:let)?s?', 'puff?s?', 'mls?', 'msl',
               'millil(?:it(?:er|re)s?)?',
               'grams?', 'gms?', 'g', 'mcg', 'micro ?grams?', 'milli ?grams?',
               'mgs?', 'inh', 'capfull?s?', 'vials?', 'patch(?:es)?',
               'bolus(?:es)?', 'lo[sz]enges?', 'ounces?', 'pack(?:et)?s?',
               'units?', 'pastilles?', 'ounces?', sep = '|')

#' Guess dosage from prescription free text
#'
#' @examples
#' View(guess_prescription(common_dosages[1:1000, 'PRESCRIPTION']))
#' @export
guess_prescription <- function(text) {
  data.frame(raw = text,
             clean = extract_drug_info(text),
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
#' extract_drug_info(c('2mg per day', '1xdaily', 'a2b', '  double  spaced  ',
#'                     'newline\ntab\treturn\r', '2by14', 'take q4d'))
#'
#' @include keywords.R
#' @importFrom magrittr %>%
#' @importFrom stringr str_replace_all
#' @importFrom glue glue
#' @export
extract_drug_info <- function(x) {
  unit_pattern <-
    glue::glue('(\\d+)(\\s+)({dose_unit})(sid|bd)')
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
  std_text <- extract_drug_info(text)
  std_text <- str_replace_all(std_text, '(apply|to be applied)( to the affected part)?', 'one')
  std_text <- str_replace_all(std_text, 'to be taken ', '')
  std_text <- str_replace_all(std_text, '\\(s\\)', '')
  nums <- paste('one', 'two', 'three', 'four(?:teen)?', 'five',
                'six(?:teen)?', 'seven(?:teen)?', 'eight(?:een)?',
                'nine(?:teen)', 'ten', 'eleven', 'twelve', 'thirteen',
                'fifteen', 'twenty', '\\d+\\.?\\d*', sep = '|')
  freq <- glue::glue('(?:every|each|at|in the) (?:day|night|morning|evening)',
                     'daily', 'bd', 'nocte', 'mane', '[qt]\\.?d\\.?s\\.?',
                     '(?:once|twice|(?:up to )?(?:{nums})(?:-(?:{nums}))? times) ?(?:daily|every day|(?:a|per|/) ?day)',
                     'p\\.?r\\.?n\\.?', '\\dhrly', 'every (?:{nums}|\\d-\\d+) h(?:ou)?rs?(?: when required)?',
                     '(?:take )?(?:when|as) (?:directed|required|needed)',
                     .sep = '|')
  patterns <- glue::glue('(?:{nums})(?:(?: or |-)(?:{nums}))?(?= (?!hours?)(?:\\w+ )?(?:{freq}))',
                         '(?:{nums})(?= a day)', .sep = '|')
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
#' @importFrom glue glue
#' @importFrom stringr str_extract_all
#' @export
guess_frequency <- function(text) {
  std_text <- extract_drug_info(text)
  df_unit <- dose_unit
  df_meal <- c('meals?', 'food', 'breakfast', 'lunch', 'dinner', 'supper',
               '(?:main|evening) meal')
  df_meal_how <- c('before', 'after', 'with', 'at', 'qac', 'between')
  df_time_unit <- c('min(?:ute)?s?', 'h(?:ou)?rs?', 'd(?:ays?)?', 'w(?:ee)?ks?',
                    'months?', 'y(?:ea)?rs?', 'midday', 'fortnight')
  df_every <- c('an?', 'each', 'eve?ry', 'per', '/')
  df_when <- c('at', 'in', 'before', 'after', 'during')
  OR <- function(x) sprintf('(?:%s)', paste(x, collapse = '|'))
  df_timely <- paste0(OR(c(
    'd(?:ai)?', 'h(?:ou)?r?', '(?:bi)?w(?:ee)?k', '(?:bi)?mo?n?th',
    '(?:fort)?night', 'y(?:ea)?r')), 'ly')
  df_time_unit <- c('min(?:ute)?s?', 'h(?:ou)?rs?', 'd(?:ays?)?', 'w(?:ee)?ks?',
                    'months?', 'y(?:ea)?rs?', 'midday', 'fortnight')
  df_period <- c('mor(?:ne|ning)?', 'wk', 'eve(?:ning)?', 'd(?:ay)?',
                 '(?:after)?noon', 'tea time', 'bed(?:time)?', '[ap]m',
                 'midday', '(?:mid)?night', 'nocte?', 'noc', 'mane',
                 'dinner ?time', 'lunch ?time')
  df_times <- glue::glue('once', 'twice', 'thrice', '(?:up ?)?(?:to )?{OR(nums)} times?', .sep = '|')
  nums <- paste('one', 'two', 'three', 'four(?:teen)?', 'five',
                'six(?:teen)?', 'seven(?:teen)?', 'eight(?:een)?',
                'nine(?:teen)', 'ten', 'eleven', 'twelve', 'thirteen',
                'fifteen', 'twenty', '\\d{1,5}\\.?\\d{0,5}',
                sep = '|')

  df_per_time_unit <- glue::glue(
    '{OR(df_every)} {OR(nums)}(?: .)? {OR(df_time_unit)}',
    '({OR(c(df_every, df_when))}(?: .)? )?{OR(df_period)}',
    '{OR(df_when)} \\d{{1,4}}',
    '{OR(df_times)}(?:(?: {OR(df_every)})? {OR(df_time_unit)})?',
    .sep = '|'
  )
  df_uber_number <- glue::glue('{OR(nums)}(?: (?:or|-|to|/))?(?: {OR(nums)})?')

  df_latin <- c('nocte', 'dieb alt', 'alt h', 'mane', 'q[.]?[1-8]?[.]?[dh][.]?',
                add_initialism_dots(c(
                  'am', 'bd', 'bds', 'bh', 'bid', 'bis', 'biw', 'bt', 'eod',
                  'hs', 'od', 'om', 'on', 'op', 'opd', 'pm', 'qac', 'qad',
                  'qam', 'qd', 'qds', 'qhs', 'qid', 'qod', 'qpm', 'qqh', 'qwk',
                  'sid', 'td', 'tds', 'tid', 'tiw'))
  )
  df_everyxunit <- glue::glue(
    '{OR(df_every)} {OR(df_period)} {OR(df_per_time_unit)}'
  )

  patterns <- glue::glue(
    # are these ^ anchors necessary?
    '(?<=^{df_uber_number} (?:x ?)?)(?:(?:\\d )?)(?:{df_timely}|{OR(df_latin)}|{df_everyxunit})',
    '^{df_uber_number}(?= (?:x )?{OR(df_every)} {OR(df_time_unit)})', # doesn't seem right - l167 of frequency_phase.mixup - doesn't capture time unit.
    '^{df_uber_number} ms?ls? [ob]d',
    '^{df_uber_number} {OR(df_per_time_unit)} {OR(df_uber_number)} {OR(df_per_time_unit)}',
    .sep = '|'
  )
  #message(patterns) # debug
  matches <- stringr::str_extract_all(std_text, patterns, simplify = TRUE)
  longest <- apply(matches, 1, function(x) x[which.max(nchar(x))])
  setNames(longest, text)
}

#' Estimate the time between doses for a prescription.
#'
#' @export
guess_interval <- function(text) {
  std_text <- extract_drug_info(text)
  std_text
}
