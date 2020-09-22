#' Guess dosage number from prescription text
#'
#' @inheritParams guess_dose_unit
#'
#' @examples
#' presc <- c(
#'   '1-2 TABLET(S) UP TO FOUR TIMES A DAY WHEN REQUIRED',
#'   'THREE CAPSULES DAILY',
#'   'ONE EVERY DAY',
#'   'ONE TWICE A DAY',
#'   'TWICE A DAY',
#'   'TAKE ONE TWICE DAILY',
#'   '2 TABLETS FOUR TIMES A DAY',
#'   '5ML THREE TIMES DAILY WHEN REQUIRED',
#'   'TAKE 1 FOUR TIMES A DAY WHEN REQUIRED',
#'   '2 TABLETS FOUR TIMES A DAY WHEN REQUIRED',
#'   '1 CAPSULE TWICE A DAY',
#'   '5-10MLS FOUR TIMES A DAY WHEN REQUIRED',
#'   '2.5 OR 5ML FOUR TIMES A DAY WHEN REQUIRED',
#'   '10MG WHEN REQUIRED',
#'   '1 PATCH EVERY 72 HOURS'
#' )
#' stack(guess_dose_number(presc))
#'
#' @importFrom stringr str_extract_all
#' @export
# guess_dose_number <- function(text) {
#   std_text <- extract_drug_info(text)
#   number_matches <- stringr::str_extract_all(std_text, patterns$dose_number, simplify = TRUE)
#   longest_number <- apply(number_matches, 1, function(x) x[which.max(nchar(x))])
#   output <- word2num(longest_number)
#   # see default_dose_number.py
#   setNames(output, text)
# }

#' Extract text corresponding to dosage number
#'
#' Seeing if we can do better than original without copying over code.
#'
#' @param text Character vector of free text prescriptions
#'
#' @return Vector of substrings, to be passed to \code{guess_number}
#'
#' @examples
#' library(tibble)
#' common_dosages <- read.csv('tests/common_doses_textmining.csv')
#' x <- tibble(
#'   text = common_dosages[1:1000, 'PRESCRIPTION'],
#'   old_min = common_dosages[1:1000, 'DN.MIN'],
#'   old_max = common_dosages[1:1000, 'DN.MAX'],
#'   new = extract_dn_text(common_dosages[1:1000, 'PRESCRIPTION'])
#' )
#' @export
extract_dn_text <- function(text) {
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
  latin <- paste('nocte', 'dieb alt', 'alt h', 'mane', 'q\\.?[1-8]?\\.?[dh]\\.?',
                 # '[bqt]ds?', '[bt]iw', '[bstq]id', 'bt', '[eq]od', 'qa[cdm]'
                 paste(add_initialism_dots(c(
                   'am', 'bd', 'bds', 'bh', 'bid', 'bis', 'biw', 'bt', 'eod',
                   'hs', 'od', 'om', 'on', 'op', 'opd', 'pm', 'qac', 'qad',
                   'qam', 'qd', 'qds', 'qhs', 'qid', 'qod', 'qpm', 'qqh', 'qwk',
                   'sid', 'td', 'tds', 'tid', 'tiw')), collapse = '|'), sep = '|')
  patterns <- glue::glue('(?:{nums})(?:(?: or |-)(?:{nums}))?(?= (?!hours?)(?:\\w+ )?(?:{freq}))', #?
                         '(?:{nums}) \\d-?\\d? (?:mls?|msl)(?= spoon)',
                         '(?:{nums})(?= a day)',
                         '(?:{nums}) x \\d+\\.?\\d* (?:mls?|msl)',
                         '^\\d(?:or|[.-])?\\d? (?:{latin})',
                         .sep = '|')
  number_matches <- stringr::str_extract_all(std_text, patterns, simplify = TRUE)
  longest_number <- apply(number_matches, 1, function(x) x[which.max(nchar(x))])
  output <- word2num(longest_number)
  setNames(output, text)
}

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
#'   text = common_dosages[1:1000, 'PRESCRIPTION'],
#'   old_min = common_dosages[1:1000, 'DN.MIN'],
#'   old_max = common_dosages[1:1000, 'DN.MAX'],
#'   dntxt = extract_dn_text(common_dosages[1:1000, 'PRESCRIPTION']),
#'   dose_num = convert_number_text(dntxt)
#' )
#'
#' @importFrom stringr str_extract str_detect str_match
#' @importFrom dplyr coalesce %>%
#' @importFrom glue glue
#'
#' @export
convert_number_text <- function(x) {
  # The purpose of this function is to convert the extracted text from
  # guess_dose_number into a numeric format
  dbl_dose <- function(d) {
    d_num <- as.numeric(word2num(d[, 2]))
    avg <- mean(d_num)
    avg[is.nan(avg)] <- NA
    as.character(avg)
  }
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
    glue::glue('(\\w+|\\d+.\\d+) (?:(?:{int}) times )?(?:{at}) (?:{mealtime})')

  units <- paste('capsules?', 'caps?', 'sachets?', 'dro?ps?', 'dr', 'ampoules?',
      'amps?', 'suppository?', 'pills?', 'blisters?', 'sprays?',
      '(?<=[0-9] )spr', '(?<=[0-9] )suppos',
      'tab(?:let)?s?', 'puff?s?', 'mls?', 'msl',
      'millil(?:it(?:er|re)s?)?',
      'grams?', 'gms?', 'g', 'mcg', 'micro ?grams?', 'milli ?grams?',
      'mgs?', 'inh', 'capfull?s?', 'vials?', 'patch(?:es)?',
      'bolus(?:es)?', 'lo[sz]enges?', 'ounces?', 'pack(?:et)?s?',
      'units?', 'pastilles?', 'ounces?', sep = '|')

  dose_unit <- glue::glue('(?:{nums}) ?(-|to|or) ?(?:{nums}) ?(?:{units})')
  dose_unit_sng <- glue::glue('(?:{nums}) ?(-)? ?(?:{units})')
  dose_unit_dbl <- glue::glue('(?:{dose_unit_sng}) ?(?:or|-|to) ?(?:{dose_unit_sng})')

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
