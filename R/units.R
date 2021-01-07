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
#'
#' Regular
drug_units <- c(
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
  `n(?:an)?g|nano ?grams?` = "nanogram",
  `suppository?|(?<=[0-9] )suppos` = "suppository",
  `vials?` = "vial", `patch(?:es)?` = "patch",
  `ounces?|oz` = "ounce",
  `units?` = "unit", `lo[sz]enges?` = "lozenge",
  `pack(?:et)?s?` = "pack",
  `caps? ?full?s?` = "capful")
