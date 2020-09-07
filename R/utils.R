#' Build regular expressions from vectors of alternatives
#'
#' A light wrapper around \code{\link[glue]{glue}}.
#'
#' Any expressions of the form \code{\{foo*\}} will be interpreted like
#' \code{\{paste(foo, collapse = '|')\}} when the string is interpolated.
#'
#' @param ... Arguments passed to \code{\link[glue]{glue}}
#'
#' @examples
#' ntimes <- c('once', 'twice', 'thrice')
#' regex_or('Take ({ntimes*}) per day')
#'
#' words <- list(n = ntimes)
#' regex_data_or(words, 'Take ({n*}) daily')
#'
#' # Easier to read but maybe less efficient:
#' library(glue)
#' glue_collapse(glue('Take {ntimes} per day'), sep = '|')
#'
#' @importFrom glue glue
#' @export
regex_or <- function(..., .envir = parent.frame()) {
  glue::glue(...,
             .transformer = collapse_transformer(sep = '|'),
             .envir = .envir)
}

#' @rdname regex_or
#' @importFrom glue glue_data
#' @export
regex_data_or <- function(.x, ..., .envir = parent.frame()) {
  glue::glue_data(.x, ..., .transformer = collapse_transformer(sep = '|'),
                  .envir = .envir)
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
#' @export
extract_drug_info <- function(x) {
  clean_text <- tolower(x)
  replace <- trimws(gsub('\\|', ' ', clean_text))
  replace_x <- gsub('([0-9]+)(x)([a-z]+)', '\\1 \\2 \\3', replace, perl = TRUE)
  single_spaces <- gsub('([0-9]+)([a-z]+)', '\\1 \\2', replace_x, perl = TRUE)
  single_spaces2 <- gsub('([a-z]+)([0-9]+)', '\\1 \\2', single_spaces, perl = TRUE)
  spaces <- gsub('([a-z]+)([0-9]+)([a-z]+)', '\\1 \\2 \\3', single_spaces2, perl = TRUE)
  spaces2 <- gsub('([0-9]+)([a-z]+)([0-9]+)', '\\1 \\2 \\3', spaces, perl = TRUE)
  spaces3 <- gsub(regex_or('(\\d+)(\\s+)({dose_dict$DoseUnit*})(sid|bd)'),
                  '\\1 \\2 \\3 \\4', spaces2, perl = TRUE)
  gsub('\\s+', ' ', spaces3)
}

#' @importFrom glue identity_transformer glue_collapse
collapse_transformer <- function(regex = "[*]$", ...) {
  function(text, envir) {
    collapse <- grepl(regex, text)
    if (collapse) {
      text <- sub(regex, "", text)
    }
    res <- glue::identity_transformer(text, envir)
    if (collapse) {
      glue::glue_collapse(res, ...)
    } else {
      res
    }
  }
}

#' Add optional dots to initialisms
#'
#' Convenience function for building regular expressions.
#' For example, \code{USA} can become \code{U.S.A}, \code{U.S.A.} and so on
add_initialism_dots <- function(x) {
  paste0(
    vapply(strsplit(x, ''), paste, collapse = '\\.?', FUN.VALUE = character(1)),
    '\\.?'
  )
}

#' Coerce English number words to digits
#'
#' If a number is given in word form, for example 'three', it is converted to
#' digits, i.e. '3' corresponding to a daily dosage.
#'
#' Current implementation covers numbers one to twenty four, plus common Latin
#' abbreviations (e.g. q.q.h. means 'every four hours', i.e. 6 per day).
#'
#' @param x a character vector of words
#'
#' @return a vector the same length as \code{x}, with any named numbers
#' converted to the respective digits.
#'
#' @examples
#' word2num(c('three', 'six', 'hello', 'q4h', 'bedtime'))
#'
#' @importFrom english words
#' @importFrom stringr str_replace_all
word2num <- function(x) {
  digits <- c(once = 1, twice = 2, thrice = 3, a = 1,
              setNames(1:24, gsub('-', '', english::words(1:24))),
              od = 1,
              bd = 2, 'b. d' = 2, b.d. = 2, 'b. d.' = 2,
              t.d.s. = 3, t.d.s = 3, 't. d. s.' = 3, tds = 3,
              t.i.d. = 3, t.i.d = 3, 't. i. d.' = 3, tid = 3,
              t.i.w. = 3, t.i.w = 3, 't. i. w.' = 3, tiw = 3,
              s.i.d. = 1, s.i.d = 1, 's. i. d.' = 1, sid = 1,
              q.w.k. = 1, q.w.k = 1, 'q. w. k.' = 1, qwk = 1,
              q.q.h. = 6, q.q.h = 6, 'q. q. h.' = 6, qqh = 6,
              q.o.d. = 1, q.o.d = 1, 'q. o. d.' = 1, qod = 1,
              q.i.d. = 4, q.i.d = 4, 'q. i. d.' = 4, qid = 4,
              q.d. = 1, q.d = 1, 'q. d.' = 1, qd = 1,
              q.1.d. = 1, q.1.d = 1, 'q. 1. d.' = 1, q1d = 1,
              a.m. = 1, a.m = 1, 'a. m.' = 1, am = 1,
              b.i.s. = 2, b.i.s = 2, 'b. i. s.' = 2, bis = 2,
              b.i.d. = 2, b.i.d = 2, 'b. i. d.' = 2, bid = 2,
              b.t. = 1, b.t = 1, 'b. t.' = 1, bt = 1,
              h.s. = 1, h.s = 1, 'h. s.' = 1, hs = 1,
              'dieb alt' = 1, alt = 1,
              e.o.d. = 1, e.o.d = 1, 'e. o. d.' = 1, eod = 1,
              mane = 1,
              o.n. = 1, o.n = 1, 'o. n.' = 1, on = 1,
              o.m. = 1, o.m = 1, 'o. m.' = 1, om = 1,
              o.p.d. = 1, o.p.d = 1, 'o. p. d.' = 1, opd = 1,
              p.m. = 1, p.m = 1, 'p. m.' = 1, pm = 1,
              q.a.d. = 1, q.a.d = 1, 'q. a. d.' = 1, qad = 1,
              q.a.m. = 1, q.a.m = 1, 'q. a. m.' = 1, qam = 1,
              q.d.s. = 4, q.d.s = 4, 'q. d. s.' = 4, qds = 4,
              q.p.m. = 1, q.p.m = 1, 'q. p. m.' = 1, qpm = 1,
              q.h. = 24, q.h = 24, 'q. h.' = 24, qh = 24,
              q.h.s. = 1, q.h.s = 1, 'q. h. s.' = 1, qhs = 1,
              q.1.h. = 24, q.1.h = 24, 'q. 1. h.' = 24, q1h = 24,
              q.2.h. = 12, q.2.h = 12, 'q. 2. h.' = 12, q2h = 12,
              noc = 1, nocte = 1, noct = 1,
              b.d.s. = 2, b.d.s = 2, 'b. d. s.' = 2, bds = 2,
              t.d. = 3, t.d = 3, 't. d.' = 3, td = 3,
              'alt sh' = 12,
              q.3.h. = 8, q.3.h = 8, 'q. 3. h.' = 8, q3h = 8,
              q.4.h. = 6, q.4.h = 6, 'q. 4. h.' = 6, q4h = 6,
              q.5.h. = 4.8, q.5.h = 4.8, 'q. 5. h.' = 4.8, q5h = 4.8,
              q.6.h. = 4, q.6.h = 4, 'q. 6. h.' = 4, q6h = 4,
              q.7.h. = 3.4, q.7.h = 3.4, 'q. 7. h.' = 3.4, q7h = 3.4,
              q.8.h. = 3, q.8.h = 3, 'q. 8. h.' = 3, q8h = 3,
              eve = 1, morning = 1, night = 1, d = 1, day = 1, afternoon = 1,
              evening = 1, midday = 1, midnight = 1, teatime = 1, dusk = 1,
              bedtime = 1, mor = 1, dawn = 1, each = 1, every = 1, daily = 1,
              'a month' = 1, 'tea time' = 1, noon = 1, nightly = 1,
              lunchtime = 1, 'lunch time' = 1, 'bed time' = 1, dinnertime = 1,
              'dinner time' = 1, other = 2, 'every day' = 1, 'every night' = 1)
  digit_words <- sprintf('(?<=\\b)(%s)(?=\\b)',
                         paste(names(digits), collapse = '|'))
  stringr::str_replace_all(x, digit_words, replacement = function(d) digits[d])
}

#' Convert half-number strings to integer
#'
#' Based on \code{dose_extraction.py}
#'
#' @importFrom stringr str_extract_all str_match_all
half2num <- function(x) {
  nums <- paste(
    '\\d+', 'one', 'two', 'three', 'four(?:teen)?', 'five|six(?:teen)?',
    'seven(?:teen)?', 'eight(?:een)?', 'nine(?:teen)?', 'ten', 'eleven',
    'twelve', 'thirteen', 'fifteen', 'twenty', sep = '|'
  )
  regex_half_less <- sprintf('(half) (?:or|to) (%s)', nums)
  regex_half_more <- sprintf('(half) (?:and) (%s)', nums)
  regex_half_extra_more <- sprintf('(%s) (?:and) (half)', nums)
  regex_quarter <- 'quarter'
  stringr::str_match_all(x, regex_half_less)
  stop('finish this function later')
}
