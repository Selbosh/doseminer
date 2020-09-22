#' Dictionary of useful patterns for text mining prescriptions
#'
#' @param x Name of category of keywords. If missing, returns whole dictionary
#'
#' The possible categories of keywords include the following. If in doubt,
#' call \code{dose_dict()} with no argument to return the whole dictionary.
#' \itemize{
#'   \item unit
#'   \item numbers
#'   \item latin
#'   \item meal
#'   \item meal_how
#'   \item time_unit
#'   \item timely
#'   \item period
#'   \item ordinal
#'   \item as_needed
#'   \item verb
#' }
#'
#' @examples
#' dose_dict('numbers')
dose_dict <- function(x) {
    OR <- function(x) sprintf('(?:%s)', paste(x, collapse = '|'))
    dictionary <- list(
        numbers = c('one', 'two', 'three', 'four(?:teen)?', 'five',
                    'six(?:teen)?', 'seven(?:teen)?', 'eight(?:een)?',
                    'nine(?:teen)', 'ten', 'eleven', 'twelve', 'thirteen',
                    'fifteen', 'twenty', '\\d{1,5}\\.?\\d{0,5}'),
        unit = c('capsules?', 'caps?', 'sachets?', 'dro?ps?', 'dr', 'ampoules?',
                 'amps?', 'suppository?', 'pills?', 'blisters?', 'sprays?',
                 '(?<=[0-9] )spr', '(?<=[0-9] )suppos',
                 'tab(?:let)?s?', 'puff?s?', 'mls?', 'msl',
                 'millil(?:it(?:er|re)s?)?',
                 'grams?', 'gms?', 'g', 'mcg', 'micro ?grams?', 'milli ?grams?',
                 'mgs?', 'inh', 'capfull?s?', 'vials?', 'patch(?:es)?',
                 'bolus(?:es)?', 'lo[sz]enges?', 'ounces?', 'pack(?:et)?s?',
                 'units?', 'pastilles?', 'ounces?'),
        latin = c('nocte', 'dieb alt', 'alt h', 'mane', 'q\\.?[1-8]?\\.?[dh]\\.?',
                  add_initialism_dots(c(
                      'am', 'bd', 'bds', 'bh', 'bid', 'bis', 'biw', 'bt', 'eod',
                      'hs', 'od', 'om', 'on', 'op', 'opd', 'pm', 'qac', 'qad',
                      'qam', 'qd', 'qds', 'qhs', 'qid', 'qod', 'qpm', 'qqh',
                      'qwk', 'sid', 'td', 'tds', 'tid', 'tiw')
                      )
            ),
        meal = c('meals?', 'food', 'breakfast', 'lunch', 'dinner', 'supper',
                 '(?:main|evening) meal'),
        meal_how = c('before', 'after', 'with', 'at', 'qac', 'between'),
        time_unit = c('min(?:ute)?s?', 'h(?:ou)?rs?(?=\\b)',
                      '(?<=\\b)d(?:ays?)?(?=\\b)', 'w(?:ee)?ks?',
                      'months?', 'y(?:ea)?rs?', 'midday', 'fortnight'),
        every = c('an?', 'each', 'eve?ry', 'per', '/'),
        when = c('at', 'in', 'before', 'after', 'during'),
        timely = paste0(OR(c(
            'd(?:ai)?', 'h(?:ou)?r?', '(?:bi)?w(?:ee)?k', '(?:bi)?mo?n?th',
            '(?:fort)?night', 'y(?:ea)?r')), 'ly'),
        period = c('mor(?:ne|ning)?', 'wk', 'eve(?:ning)?',
                   '(?<=\\b)d(?:ay)?(?=\\b)',
                   '(?:after)?noon', 'tea time', 'bed(?:time)?', '[ap]m',
                   'midday', '(?:mid)?night', 'nocte?', 'noc', 'mane',
                   'dinner ?time', 'lunch ?time'),
        ordinal = c('first', 'second', 'third', 'fourth', 'fifth', 'sixth', 'seventh',
                    'eighth', 'ninth', 'tenth'),
        as_needed = c('p\\.?r\\.?n\\.?', 'pro re nata', 'sos', '(?:si opus )?sit',
                     'siop', '(?:as|if|when) (?:needed|required|reqd?|necessary)'),
        verb = c('put', 'take', 'insert', 'suck', 'inhale', 'use', 'instil', 'inject',
                 'add', 'chew')
    )

    if (missing(x))
      return(dictionary)

    paste(dictionary[[x]], collapse = '|')
}
