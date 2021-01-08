#' Medication dosage units
#'
#' A named character vector. Names represent patterns to match dose units
#' and values represent standardised names for those units.
#'
#' Use with a function like \code{\link[stringr:str_replace]{str_replace_all}}
#' to standardise a freetext prescription.
#'
#' @examples
#' stringr::str_replace_all('Three milli litres', drug_units)
drug_units <- c(
  `applications?` = 'application',
  `mls? spoon(?:s?ful)?s?` = 'ml spoonful',
  `amp(?:oule)?s?` = "ampoule",
  `bolus(?:es)?` = "bolus",
  `mgs?|milli ?grams?` = "milligram",
  `\bgram(?:me)s?|gms?|\bg\b` = "gram",
  `tab(?:let)?s?` = "tab",
  `cap(?:sule)?s?` = "cap",
  `milli ?lit(?:re|er)s?|mls?|msl` = "ml",
  iu = "iu",
  `sachets?` = "sachet",
  `pastilles?` = "pastille",
  `pills?` = "pill",
  `dr(?:o?ps?)?` = "drop",
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
#' If there are multiple units in a string, only the first is returned.
#'
#' @param txt a character vector
#'
#' @return A character vector the same length as \code{txt}, containing
#' standardised units, or \code{NA} if no units were found in the prescription.
#'
#' @examples
#' extract_dose_unit(example_prescriptions)
#'
#' Based on \code{add_dose_unit.py} from original Python/Java algorithm.
#'
#' @importFrom stringr str_replace_all str_extract
extract_dose_unit <- function(txt) {
  standardised <- stringr::str_replace_all(txt, drug_units)
  stringr::str_extract(standardised, paste(drug_units, collapse = '|'))
}

#' Evaluate a multiplicative plaintext expression
#'
#' Replaces written phrases like "2 x 5" with their arithmetic result (i.e. 10)
#'
#' @seealso
#' Used internally within \code{\link{extract_from_prescription}}
#'
#' @examples
#' multiply_dose('2 x 5')
#'
#' @importFrom stringr str_replace
multiply_dose <- function(axb) {
  expr <- str_replace(axb, '(\\d+[.]?\\d*) x (\\d+[.]?\\d*)', '\\1 * \\2')
  eval(parse(text = expr))
}
