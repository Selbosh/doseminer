#' Guess dosage unit from prescription text
#'
#' Currently works on raw text, but later we might want to demerge this step.
#' Similarly, the \code{unit_dict} and its reverse-lookup table could be defined
#' outside this function, both for readability and to avoid redundant computation.
#'
#' @param text character vector of raw prescription text
#'
#' @return a vector of units, the same length as \code{text}. Names are equal to
#' \code{text}, but I might remove the names later if we decide to use a data
#' frame-orientated workflow rather than a vector-based one.
#'
#' @examples
#' presc <- c(
#'   '1-2 TABLET(S) UP TO FOUR TIMES A DAY WHEN REQUIRED',
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
#' stack(guess_dose_unit(presc))
#'
#' @importFrom stringr str_extract_all
#'
#' @export
guess_dose_unit <- function(text) {
  unit_patterns <- sprintf('\\b(?:%s)\\b', dose_dict('unit'))
  unit_names <- list(
    # based on add_dose_unit.py
    # maybe automatically drop trailing 's' and strip spaces.
    mg = c('mg', 'mgs', 'milligram', 'milligrams', 'milli gram', 'milligrams',
           'milli grams'),
    g = c('grams', 'grammes', 'gram', 'gramme', 'g', 'gm', 'gms'),
    tab = c('tabs', 'tab', 'tablet', 'tablets'),
    cap = c('cap', 'caps', 'capsule', 'capsules'),
    ml = c('millilitres', 'millilitre', 'mls', 'ml', 'milli litre',
           'milli litres', 'msl', 'milliliters', 'milliliter', 'milli liter',
           'milli liters'),
    iu = 'iu',
    sachet = c('sachet', 'sachets'),
    pastille = c('pastille', 'pastilles'),
    pill = c('pill', 'pills'),
    drop = c('drop', 'drops', 'dr', 'drp', 'drps'),
    puff = c('puff', 'puffs', 'puf', 'pufs'),
    blister = c('blister', 'blisters'),
    amp = c('amp', 'amps', 'ampoule', 'ampoules'),
    spray = c('spray', 'sprays', 'spr'),
    mcg = c('micg', 'micro g', 'microgram', 'micrograms', 'mcgs', 'micro gram',
            'micro grams', 'mcg', 'ug'),
    ng = c('ng', 'nang', 'nanogram', 'nano gram', 'nanograms', 'nano grams'),
    suppos = c('suppositor', 'suppository'),
    vial = c('vial', 'vials'),
    patch = c('patch', 'patches'),
    ounce = c('ounce', 'ounces', 'oz'),
    unit = c('unit', 'units'),
    lozenge = c('lozenge', 'lozenges', 'losenge', 'losenges'),
    pack = c('packs', 'packets', 'pack', 'packet'),
    capful = c('capfuls', 'capfulls', 'capful', 'capfull')
  )
  unit_lookup <- setNames(rep(names(unit_names), lengths(unit_names)),
                          unlist(unit_names, FALSE, FALSE))
  std_text <- sanitize_prescription(text)
  unit_matches <- stringr::str_extract_all(std_text, unit_patterns, simplify = TRUE)
  unit_text <- apply(unit_matches, 1, function(x) x[which.max(nchar(x))])
  setNames(unit_lookup[unit_text], text)
}
