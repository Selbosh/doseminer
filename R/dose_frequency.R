#' Convert daily frequency text into numeric form
#'
#' @param string character vector output from \code{extract_number}
#'
#' This is equivalent in some ways to \code{freq_word_con.py}.
#' The \code{frequency_dict} in the original Python code is implemented here via
#' the \code{word2num} utility function.
#'
#' @examples
#' convert_freq_text(c('daily', 'twice a day', '6 hrly', NA, 'weekly',
#' '1 - 2 a week', 'two to three wkly', 'every 2 weeks', '4 times weekly',
#' '3 - 4 times monthly', '5 - 6 times a month'))
#'
#' library(tibble)
#' common_dosages <- read.csv('tests/common_doses_textmining.csv')
#' n <- 5000
#' x <- tibble(
#'   text = common_dosages[1:n, 'PRESCRIPTION'],
#'   old_min = common_dosages[1:n, 'DF.MIN'],
#'   old_max = common_dosages[1:n, 'DF.MAX'],
#'   dftxt = guess_frequency(text),
#'   dose_freq = convert_freq_text(dftxt)
#' )
#'
#' @importFrom stringr str_extract str_detect str_match
#' @importFrom dplyr coalesce %>%
#'
#' @export
convert_freq_text <- function(x) {
  # The purpose of this function is to convert the extracted text from
  # guess_frequency into a numeric format
  dbl_dose <- function(d) {
    d_num <- as.numeric(word2num(d[, 2]))
    avg <- mean(d_num)
    avg[is.nan(avg)] <- NA
    as.character(avg)
  }
  # replace the following with dose_dict
  # and/or use a utility function for equivalent stuff in convert_number_text
  int <- paste('one', 'two', 'three', 'four(?:teen)?', 'five',
               'six(?:teen)?', 'seven(?:teen)?', 'eight(?:een)?',
               'nine(?:teen)', 'ten', 'eleven', 'twelve', 'thirteen',
               'fifteen', 'twenty', '\\d', sep = '|')
  nums <- paste('\\d+\\.?\\d*', int, sep = '|')
  at <- paste('each', 'every', 'an?', 'in', 'at', 'per', 'during', 'after',
              'before', 'with', sep = '|')
  mealtime <- paste('mor(?:n|ning|e)?', 'eve(?:ning)?', '(?:after)?noon',
                    'tea ?time', 'bedtime', 'dinner ?time', 'mid(?:night|day)',
                    'night', 'dusk', 'dawn', 'day', 'mane', 'nocte?',
                    'lunch ?time', sep = '|')
  meal <- paste('(?:main |evening )?meal', 'dinner', 'breakfast', 'lunch',
                'supper', 'food', sep = '|')
  n_at_meal <-
    regex_or('(\\w+|\\d+.\\d+) (?:{int*} times )?{at*} {mealtime*}')

  units <- paste('capsules?', 'caps?', 'sachets?', 'dro?ps?', 'dr', 'ampoules?',
                 'amps?', 'suppository?', 'pills?', 'blisters?', 'sprays?',
                 '(?<=[0-9] )spr', '(?<=[0-9] )suppos',
                 'tab(?:let)?s?', 'puff?s?', 'mls?', 'msl',
                 'millil(?:it(?:er|re)s?)?',
                 'grams?', 'gms?', 'g', 'mcg', 'micro ?grams?', 'milli ?grams?',
                 'mgs?', 'inh', 'capfull?s?', 'vials?', 'patch(?:es)?',
                 'bolus(?:es)?', 'lo[sz]enges?', 'ounces?', 'pack(?:et)?s?',
                 'units?', 'pastilles?', 'ounces?', sep = '|')

  dose_unit <- regex_or('{nums*} ?(-|to|or) ?{nums*} ?{units*}')
  dose_unit_sng <- regex_or('{nums*} ?(-)? ?{units*}')
  dose_unit_dbl <- regex_or('{dose_unit_sng*} ?(?:or|-|to) ?{dose_unit_sng*}')

  # development note:
  # name of column is the corresponding regex in freq_word_con.py
  matches <- data.frame(
    na_week_month = ifelse(x %in% c('weekly', 'monthly', 'wkly', 'wk', 'in the week',
                                    'in a week', 'a week', 'wk', 'a month'),
                            '?', NA), # not sure if this adds value
    # per_week = ifelse(stringr::str_detect(x, 'w(?:ee)?k'),
    #                   x, NA),

    freq =
      stringr::str_match_all(x, '(\\w+) (?:-|or|to) (\\w+) (?:a|the|times)? ?(?:w(?:ee)?k|wly)') %>%
      vapply(function(d) {
        if (!length(d))
          return(NA_character_)
        d_num <- as.numeric(word2num(d[, -1]))
        paste(d_num, collapse = '-')
      }, character(1)),

    every_week =
      ifelse(stringr::str_detect(x, '(?:every|each) (\\w+) (?:w(?:ee)k)'),
             '1', NA), # inconsistent with 'matches' rule above?

    times_a_week =
      stringr::str_match(x, '(\\w+) times(?: a)? (?:w(?:ee)?k|wly)') %>%
      apply(MARGIN = 1, function(d) word2num(d[2])), # line 271

    twice_a_week = #
      stringr::str_match(x, '(once|twice|thrice)(?: a)? (?:w(?:ee)?k|wly)') %>%
      apply(MARGIN = 1, function(d) word2num(d[2])),

    freq_monthly =
      stringr::str_match(x, '(\\w+) (?:-|or|to) (\\w+)(?: (?:a|the|times(?: a)?))? (?:m(?:on)?th)') %>%
      apply(MARGIN = 1, function(d) {
        if (all(is.na(d[-1])))
          return(NA_character_)
        paste(as.numeric(word2num(d[-1])), collapse = '-')
      }),

    a_month = # no need for these to be separate rules to weeks
      # unless we want to divide by 30 (7) each time, which original code
      # doesn't do (the /7 and /30 parts are commented out)
      stringr::str_match(x, '(\\w+) times(?: a)? (?:m(?:on)?th)') %>%
      apply(MARGIN = 1, function(d) word2num(d[2]))

    # every_n_months # see regex_every_month ==> 1
    # every_n_weeks # see regex_every_week ==> 1

  )
  setNames(word2num(dplyr::coalesce(!!!matches)), x)
}
