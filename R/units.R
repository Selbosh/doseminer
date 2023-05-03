#' Medication dosage units
#'
#' A named character vector. Names represent patterns to match dose units
#' and values represent standardised names for those units.
#'
#' Use with a function like \code{\link[stringr:str_replace]{str_replace_all}}
#' to standardise a freetext prescription. Used internally in
#' \code{\link{extract_from_prescription}}.
drug_units <- c(
  `applications?` = 'application',
  `mls? spoon(?:s?ful)?s?` = 'ml spoonful',
  `\\bamp(?:oule)?s?` = "ampoule",
  `bolus(?:es)?` = "bolus",
  `mgs?|milli ?grams?` = "milligram",
  `\bgram(?:me)s?|gms?|\bg\b` = "gram",
  `tab(?:let)?s?` = "tab",
  `cap(?:sule)?s?` = "cap",
  `milli ?lit(?:re|er)s?|mls?|msl` = "ml",
  `\\biu\\b` = "iu",
  `sachets?` = "sachet",
  `pastilles?` = "pastille",
  `pills?` = "pill",
  `\\bdr(?:o?ps?)?` = "drop",
  `puff?s?` = "puff",
  `blisters?` = "blister",
  `spr(?:ays?)?\\b` = "spray",
  `mic(?:ro)? ?g(?:rams?)?|mcgs?|ug` = "microgram",
  `\\bn(?:an)?g|nano ?grams?` = "nanogram",
  `suppository?|(?<=[0-9] )suppos` = "suppository",
  `vials?` = "vial", `patch(?:es)?` = "patch",
  `ounces?|oz` = "ounce",
  `units?` = "unit", `lo[sz]enges?` = "lozenge",
  `pack(?:et)?s?` = "pack",
  `caps? ?full?s?` = "capful",
  `doses?` = 'dose')

#' Extract units of dose from freetext prescriptions.
#'
#' A function used internally in \code{\link{extract_from_prescription}} to
#' parse the dosage units, such as millilitres, tablets, grams and so on.
#' If there are multiple units mentioned in a string, only the first is returned.
#'
#' @param txt a character vector
#'
#' @return A character vector the same length as \code{txt}, containing
#' standardised units, or \code{NA} if no units were found in the prescription.
#'
#' A simple wrapper around \code{\link[stringr:str_replace]{str_replace_all}} and
#' \code{\link[stringr]{str_extract}}.
#' Based on \code{add_dose_unit.py} from original Python/Java algorithm.
#'
#' @seealso \code{\link{extract_from_prescription}}
#'
#' @importFrom stringr str_replace_all str_extract
extract_dose_unit <- function(txt) {
  standardised <- stringr::str_replace_all(txt, drug_units)
  stringr::str_extract(standardised,
                       paste0('\\b(?:', paste(drug_units, collapse = '|'), ')\\b'))
}

#' Evaluate a multiplicative plaintext expression
#'
#' Replaces written phrases like "2 x 5" with their arithmetic result (i.e. 10)
#'
#' @param axb An string expression of the form 'A x B' where A, B are numeric
#'
#' @seealso
#' Used internally within \code{\link{extract_from_prescription}}
#'
#' @return An equivalent string giving the product of \code{A} and \code{B}.
#' If \code{A} is a range of values, a range of values is returned.
#'
#' @importFrom stringr str_replace
multiply_dose <- function(axb) {
  expr <- str_replace(axb, '((?:\\d+[.]?\\d* - )?\\d+[.]?\\d*) x (\\d+[.]?\\d*)', 'c(\\1) * \\2') %>%
    str_replace('-', ',')
  values <- eval(parse(text = expr))
  paste(values, collapse = ' - ')
}
