#' Convert dose number text into numeric form
#'
#' @param string character vector output from \code{extract_number}
#'
#' This is equivalent in some ways to \code{word_digit_con.py}, \code{word_digit_con2.py}
#'
#' @examples
#' convert_number_text(c('6 once per day', 'take five', 'nothing here', NA,
#' '2 three times at lunchtime and 3 at teatime'))
#'
#' library(tibble)
#' common_dosages <- read.csv('tests/common_doses_textmining.csv')
#' x <- tibble(
#'   text = common_dosages[1:5000, 'PRESCRIPTION'],
#'   old_min = common_dosages[1:5000, 'DN.MIN'],
#'   old_max = common_dosages[1:5000, 'DN.MAX'],
#'   dntxt = guess_number(text),
#'   dose_num = convert_number_text(dntxt)
#' )
#'
#' @importFrom stringr str_extract str_detect str_match
#' @importFrom dplyr coalesce %>%
#'
#' @export
convert_number_text <- function(x) {
  # The purpose of this function is to convert the extracted text from
  # guess_dose_number into a numeric format
  dbl_dose <- function(d, doseaggfun = mean) {
    d_num <- as.numeric(word2num(d[, 2]))
    avg <- FUN(d_num)
    avg[is.nan(avg)] <- NA
    as.character(avg)
  }
  # replace the following with dose_dict
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
  # name of column is the corresponding regex in word_digit_con.py
  matches <- data.frame(
    regex4 =
      stringr::str_extract(x, '^[0-9]+(?= ?once)'),

    dbl_meal_cmp = #glue::glue('{n_at_meal} ?(?:{meal})?(?: and)? {n_at_meal}') %>%
      stringr::str_match_all(x, n_at_meal) %>%
      vapply(dbl_dose, character(1)),

    dose_unit_dbl =
      stringr::str_match_all(x, dose_unit_dbl) %>%
      vapply(dbl_dose, character(1)),

    dose_unit =
      stringr::str_extract(x, dose_unit)#,

    # num =
    #   stringr::str_match(x, nums) # temp; fix me
  )
  setNames(word2num(dplyr::coalesce(!!!matches)), x)
}
