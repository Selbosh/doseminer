#' Regular expression to match numbers in English
#'
#' A regex pattern to identify natural language English number phrases, such as
#' "one hundred and fifty" or "thirty-seven". Used internally by
#' \code{\link{replace_numbers}} to identify substrings to replace with their
#' decimal representation.
#'
#' This is a PCRE (Perl type) regular expression, so it must be used with
#' \code{perl = TRUE} in base R regex functions. The packages \code{stringr}
#' and \code{stringi} are based on the alternative ICU regular expression
#' engine, so they cannot use this pattern.
#'
#' @note
#' There is no support for fractions or mixed fractions, yet.
#' The original pattern did not support expressions like "a thousand", but
#' it has been adapted to offer (experimental) support for this.
#' Phrases like "million" or "thousand" with no prefix will \emph{not} match.
#'
#' @source \url{https://www.rexegg.com/regex-trick-numbers-in-english.html}
regex_numbers <- "(?x)           # free-spacing mode
(?(DEFINE)
  # Within this DEFINE block, we'll define many subroutines
# They build on each other like lego until we can define
# a 'big number'

(?<one_to_9>
    # The basic regex:
    # one|two|three|four|five|six|seven|eight|nine
    # We'll use an optimized version:
    # Option 1: four|eight|(?:fiv|(?:ni|o)n)e|t(?:wo|hree)|
    #                                          s(?:ix|even)
    # Option 2:
    (?:f(?:ive|our)|s(?:even|ix)|t(?:hree|wo)|(?:ni|o)ne|eight)
) # end one_to_9 definition

(?<ten_to_19>
    # The basic regex:
    # ten|eleven|twelve|thirteen|fourteen|fifteen|sixteen|seventeen|
    #                                              eighteen|nineteen
    # We'll use an optimized version:
    # Option 1: twelve|(?:(?:elev|t)e|(?:fif|eigh|nine|(?:thi|fou)r|
    #                                             s(?:ix|even))tee)n
    # Option 2:
    (?:(?:(?:s(?:even|ix)|f(?:our|if)|nine)te|e(?:ighte|lev))en|
       t(?:(?:hirte)?en|welve))
) # end ten_to_19 definition

(?<two_digit_prefix>
    # The basic regex:
    # twenty|thirty|forty|fifty|sixty|seventy|eighty|ninety
    # We'll use an optimized version:
    # Option 1: (?:fif|six|eigh|nine|(?:tw|sev)en|(?:thi|fo)r)ty
    # Option 2:
    (?:s(?:even|ix)|t(?:hir|wen)|f(?:if|or)|eigh|nine)ty
) # end two_digit_prefix definition

(?<one_to_99>
    (?&two_digit_prefix)(?:[- ](?&one_to_9))?|(?&ten_to_19)|
    (?&one_to_9)
) # end one_to_99 definition

(?<one_to_999>
    (?:(?&one_to_9)|a)[ ]hundred(?:[ ](?:and[ ])?(?&one_to_99))?|
    (?&one_to_99)
) # end one_to_999 definition

(?<one_to_999_999>
    (?:(?&one_to_999)|a)[ ]thousand(?:[ ](?&one_to_999))?|
    (?&one_to_999)
) # end one_to_999_999 definition

(?<one_to_999_999_999>
    (?:(?&one_to_999)|a)[ ]million(?:[ ](?&one_to_999_999))?|
    (?&one_to_999_999)
) # end one_to_999_999_999 definition

(?<one_to_999_999_999_999>
    (?:(?&one_to_999)|a)[ ]billion(?:[ ](?&one_to_999_999_999))?|
    (?&one_to_999_999_999)
) # end one_to_999_999_999_999 definition

(?<one_to_999_999_999_999_999>
    (?:(?&one_to_999)|a)[ ]trillion(?:[ ](?&one_to_999_999_999_999))?|
    (?&one_to_999_999_999_999)
) # end one_to_999_999_999_999_999 definition

(?<bignumber>
    zero|(?&one_to_999_999_999_999_999)
) # end bignumber definition

(?<zero_to_9>
    (?&one_to_9)|zero
) # end zero to 9 definition

(?<decimals>
    point(?:[ ](?&zero_to_9))+
) # end decimals definition

(?<fractions>
  (?:a|(?&one_to_9))?(?:[ ]hal(?:f|ves)|thirds?|quarters?|fifths?)
)

(?<mixed_fractions>
  and(?:[ ](?&fractions))
) # end mixed fraction definition

) # End DEFINE


####### The Regex Matching Starts Here ########
(?&bignumber)(?:[ ](?&decimals))?

  ### Other examples of groups we could match ###
  #(?&bignumber)
  # (?&one_to_99)
  # (?&one_to_999)
  # (?&one_to_999_999)
  # (?&one_to_999_999_999)
  # (?&one_to_999_999_999_999)
  # (?&one_to_999_999_999_999_999)"

#' Replace English number phrases with their decimal representations
#'
#' Uses \code{\link{numb_replacements}} to match parts of a string corresponding
#' to numbers, then invokes \code{\link{words2number}} to convert these
#' substrings to numeric. The rest of the string (the non-number words) is
#' left intact.
#'
#' Works on non-negative integer numbers under one billion
#' (one thousand million). Does not support fractions or decimals (yet).
#'
#' @param string A character vector. Can contain numbers and other text
#'
#' @return A character vector the same length as \code{string}, with words
#' replaced by their decimal representations.
#'
#' @examples
#' replace_numbers('Two plus two equals four')
#' replace_numbers('one hundred thousand dollars!')
#' replace_numbers(c('A vector', 'containing numbers', 'like thirty seven'))
#'
#' @seealso
#' \code{\link{words2number}}, for use on cleaned text that does not contain
#' any non-number words
#'
#' @export
replace_numbers <- function(string) {
  string <- str_remove_all(string, '(?:\\band|&)[ ]?')
  matches <- gregexpr(regex_numbers, string, perl = TRUE, ignore.case = TRUE)
  regmatches(string, matches) <- lapply(regmatches(string, matches), words2number)
  string
}

#' Dictionary of English names of numbers
#'
#' For internal use in \code{\link{words2number}}. When passed as a replacement
#' to a function like
#' \code{\link[stringr:str_replace]{str_replace_all}}, it turns the
#' string into an arithmetic expression that can be evaluated to give an integer
#' representation of the named number.
#'
#' Lifted from Ben Marwick's \code{words2number} package and converted into
#' a named vector (previously a chain of \code{\link{gsub}} calls).
#'
#' @examples
#' stringr::str_replace_all('one hundred and forty-two', numb_replacements)
#'
#' @note
#' Does not yet support decimals, fractions or mixed fractions.
#'
#' @source \url{https://github.com/benmarwick/words2number}
numb_replacements <-
  c('-' = ' ',
    'eleven(?:th)?' = '+11',
    'twel(?:ve|fth)' = '+12',
    'thirteen(?:th)?' = '+13',
    'fourteen(?:th)?' = '+14',
    'fifteen(?:th)?' = '+15',
    'sixteen(?:th)?' = '+16',
    'seventeen(?:th)?' = '+17',
    'eighteen(?:th)?' = '+18',
    'nineteen(?:th)?' = '+19',
    'twent(?:y|ieth)' = '+20',
    'thirt(?:y|ieth)' = '+30',
    'fort(?:y|ieth)' = '+40',
    'fift(?:y|ieth)' = '+50',
    'sixt(?:y|ieth)' = '+60',
    'sevent(?:y|ieth)' = '+70',
    'eight(?:y|ieth)' = '+80',
    'ninet(?:y|ieth)' = '+90',

    '(?:a|one) hundred(?:th)?' = '+100',
    'two hundred(?:th)?' = '+200',
    'three hundred(?:th)?' = '+300',
    'four hundred(?:th)?' = '+400',
    'five hundred(?:th)?' = '+500',
    'six hundred(?:th)?' = '+600',
    'seven hundred(?:th)?' = '+700',
    'eight hundred(?:th)?' = '+800',
    'nine hundred(?:th)?' = '+900',

    'one|first|\\ba\\b' = '+1',
    'second|two' = '+2',
    'th(?:ree|ird)' = '+3',
    'four(?:th)?' = '+4',
    'fi(?:ve|fth)' = '+5',
    'six(?:th)?' = '+6',
    'seven(?:th)?' = '+7',
    'eighth?' = '+8',
    'nin(?:e|th)' = '+9',

    'millions?' = ')*(1000000)+(0',
    'thousand(?:th)?' = ')*(1000)+(0',
    'hundred(?:th)?' = '+100',
    'ten(?:th)?' = '+10',
    'and|&' = '',
    ' ' = '',
    '^' = '(0',
    '$' = ')',
    '\\(0\\(' = '',
    '\\+\\+' = '\\+\\(',
    '\\)\\+\\)' = '\\)',
    # Finally remove any residual non-number words.
    # Otherwise the generated arithmetic expression will not evaluate.
    '[[:alpha:]]+' = ''
  )

#' Convert English names of numbers to their numerical values
#'
#' @source Originally adapted from the
#' \href{https://github.com/benmarwick/words2number}{\code{words2number}} package by
#' Ben Marwick.
#'
#' @param phrase A character vector containing names of numbers (only).
#'
#' @return A named numeric vector of the same length as \code{phrase}.
#'
#' @examples
#' words2number('seven')
#' words2number('forty-two')
#' words2number(c('three', 'one', 'twenty two thousand'))
#'
#' @importFrom stringr str_replace_all
#'
#' @export
words2number <- function(txt) {
  if (length(txt) < 1)
    return(txt)
  if (any(lengths(txt) > 1))
    stop('words2number does not work on nested lists')
  if (!is.character(txt[[1]]))
    stop('words2number should only be passed character-vector inputs')
  expression <- stringr::str_replace_all(tolower(txt), numb_replacements)
  result <- vapply(expression, function(e) eval(parse(text = e)), FUN.VALUE = 0)
  setNames(result, txt)
}

#' List of Latin medical and pharmaceutical abbreviations
#'
#' A named character vector. Names represent Latin terms and values the English
#' translations. Used for converting terms like "q4h" into "every 4 hours",
#' which can then be parsed into a dosage frequency/interval.
#'
#' Use with a function like \code{\link[stringr:str_replace]{str_replace_all}}
#' to translate a prescription from Latin to English (thence to numbers).
#'
#' @source
#' \url{https://en.wikipedia.org/wiki/List_of_abbreviations_used_in_medical_prescriptions}
#'
#' @examples
#' stringr::str_replace_all('Take two tablets q4h', latin_medical_terms)
#'
#' @export
latin_medical_terms <- c(
  'dieb alt' = 'every 2 days',
  'alt hs' = 'every 2 hours', ## ?
  'noc(?:te?)?' = 'at night',
  'mane' = 'in morning',
  q8h = 'every 8 hours',
  q7h = 'every 7 hours',
  q6h = 'every 6 hours',
  q5h = 'every 5 hours',
  q4h = 'every 4 hours',
  q3h = 'every 3 hours',
  q2h = 'every 2 hours',
  q1h = 'every hour',
  qhs = 'at bedtime',
  qh = 'every hour',
  hs = 'bedtime',
  bt = 'bedtime',
  qam = 'every morning',
  qpm = 'every afternoon',
  'qds?' = 'daily',
  q1d = 'daily',
  qid = '4 / day',
  qqh = 'every 4 hours',
  qwk = 'every week',
  'bds?' = 'twice daily',
  bid = 'twice daily',
  bis = 'twice',
  biw = 'twice weekly',
  'tds?' = 'thrice daily',
  tiw = 'thrice weekly',
  tid = 'thrice daily',
  'alt h(?:or)?' = 'every 2 hours',
  'alt d(?:\\b|ieb)' = 'every 2 days',
  eod = 'every 2 days',
  qad = 'every 2 days',
  qod = 'every 2 days',
  opd = '1 / day',
  sid = '1 / day',
  am = 'morning',
  pm = 'afternoon',
  '\\bom' = 'every morning',
  '\\bon\\b' = 'every night',
  '\\bod' = 'every day',
  'sos|(?:si opus )?sit|siop' = 'as needed',
  qs = 'as needed',
  'prn|pro re nata' = 'as needed'
)

