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
                   'dinner ?time', 'lunch ?time')
    )

    if (missing(x))
      return(dictionary)

    paste(dictionary[[x]], collapse = '|')
}


# Make these available as internal data:
# https://r-pkgs.org/data.html#data-sysdata
#' @importFrom english words ordinal
#' @include utils.R
# .dose_dict <- list(
#     #number = c(english::words(1:20), '\\d{1,3}', 1000),
#     number = c('one', 'tw(?:o|enty)', 'three', 'four(?:teen)?', 'five', 'six(?:teen)?',
#                'seven(?:teen)?', 'eight(?:een)?', 'nine(?:teen)', 'ten',
#                'eleven', 'twelve', 'thirteen', 'fifteen', '\\d{1,4}'),
#     DoseUnit = c('capsules?', 'caps?', 'sachets?', 'dro?ps?', 'dr', 'ampoules?',
#                  'amps?', 'suppository?', 'pills?', 'blisters?', 'sprays?',
#                  '(?<=[0-9] )spr', '(?<=[0-9] )suppos',
#                  'tab(?:let)?s?', 'puff?s?', 'mls?', 'msl',
#                  'millil(?:it(?:er|re)s?)?',
#                  'grams?', 'gms?', 'g', 'mcg', 'micro ?grams?', 'milli ?grams?',
#                  'mgs?', 'inh', 'capfull?s?', 'vials?', 'patch(?:es)?',
#                  'bolus(?:es)?', 'lo[sz]enges?', 'ounces?', 'pack(?:et)?s?',
#                  'units?', 'pastilles?', 'ounces?'),
#     roman = tolower(as.roman(8:1)),
#     mini = c('or', '-', 'to', '/'),
#     meal = c('meals?', 'food', 'breakfast', 'lunch', 'dinner', 'supper',
#              'main meal', 'evening meal'),
#     spoon = c('spoons?', 'spoons?fuls?'),
#     past_verbs = c('applied', 'sprayed', 'inserted', 'sucked', 'inhaled', 'used',
#                    'instilled', 'taken', 'chewed', 'injected', 'put'),
#     verb = c('put', 'take', 'insert', 'suck', 'inhale', 'use', 'instil', 'inject',
#              'add', 'chew'), # no 'spray' here since it is also a dose unit; could lead to false positives
#     period = c('morning', 'evening', 'afternoon', 'dawn', 'dusk', 'night',
#                'noon', 'eve', 'morn?', 'tea ?time', 'day', 'bed ?time',
#                'am', 'pm', 'midday', 'midnight', 'morne', 'nocte?', 'mane',
#                'dinner ?time', 'lunch ?time', 'bed'),
#     every = c('an?', 'each', 'every', 'per'),
#     time = 'times?', # pointless keeping this as a variable
#     when = c('at', 'in', 'before', 'after', 'during'),
#     botheverywhen = c('at', 'in', 'before', 'after', 'during', 'an?', 'each',
#                       'every', 'per'),
#     small = c('mane', 'when', 'as', 'if', 'into', 'to'),
#     timeUnit = c('minutes?', 'mins?', 'hours?', 'hrs?', 'days?', 'weeks?', 'wks?',
#                  'months?', 'years?', 'midday', 'yrs', 'fortnight'),
#     timeUnitLy = c('hourly', 'daily', 'nightly', 'weekle?y', 'wk?ly', 'biweekly',
#                    'monthly', 'bimonthly', 'hr?ly', 'dly', 'hy', 'mn?thly',
#                    'yearly', 'yrly', 'fortnightly'),
#     ordNumber = as.character(english::ordinal(1:10)),
#     grammaria = c('mg', 'mgs?', 'gra?ms?', 'mcgs?', 'micrograms?',
#                   'milligrams?', 'gms?', 'g'),
#     ml = c('mls?', 'msl'),
#     dose = 'doses?',
#     latin = c('nocte', 'dieb alt', 'alt h', 'mane', 'q\\.?[1-8]?\\.?h\\.?',
#               add_initialism_dots(c(
#                 'qwk', 'qqh', 'sid', 'bis', 'bd', 'qad', 'eod', 'td', 'q1d',
#                 'qac',
#                 'bid', 'od','tid', 'tds', 'qhs', 'qam', 'qpm', 'biw', 'tiw',
#                 'qd', 'qid', 'opd', 'op', 'om', 'on', 'qds', 'bh', 'qod', 'hs',
#                 'pm', 'bt', 'am', 'bds'))
#     ),
#     hour = 'h(?:ours?)?',
#     asNeeded = c('p\\.?r\\.?n\\.?', 'pro re nata', 'sos', '(?:si opus )?sit',
#     'siop', '(?:as|if|when) (?:needed|required|reqd?|necessary)'),
#     timeAbbrev = c('d', 'hs?', 'od', 'day', 'week', 'pm', 'am', 'ac', 'hrs')
# )
