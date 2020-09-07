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
guess_dose_number <- function(text) {
  std_text <- extract_drug_info(text)
  number_matches <- stringr::str_extract_all(std_text, patterns$dose_number, simplify = TRUE)
  longest_number <- apply(number_matches, 1, function(x) x[which.max(nchar(x))])
  output <- word2num(longest_number)
  # see default_dose_number.py
  setNames(output, text)
}

#' Guess dosage number (rewriting from scratch)
#'
#' Seeing if we can do better than original without copying over code.
#'
#' @examples
#' x <- cbind(text = common_dosages[1:1000, 'PRESCRIPTION'],
#'            old_min = common_dosages[1:1000, 'DN.MIN'],
#'            old_max = common_dosages[1:1000, 'DN.MAX'],
#'            new = unname(guess_number(common_dosages[1:1000, 'PRESCRIPTION'])))
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
