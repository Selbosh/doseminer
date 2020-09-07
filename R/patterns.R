#' Regular expression patterns for prescription text mining
#'
#' Mainly for internal use. You can list all available patterns with
#' \code{names(doseminer:::patterns)}
#'
#' @include utils.R
#' @include keywords.R
patterns <- within(list(), {
  dose_unit <- regex_data_or(dose_dict, '\\b(?:{DoseUnit*})')

  uber_number <- regex_data_or(dose_dict, paste(
    # why is this called uber_number anyway?
    '(?:{number*}) ?(?:{mini*}) ?(?:{number*})',              # 5 - 6
    #'\\d\\.\\d ?(?:{mini*})? ?\\d?\\.?\\d? ?(?:{number*})', # 5.5 - 6.5 ten (?)
    '\\d\\.\\d ?(?:{mini*})? ?\\d?\\.?\\d?',
    '{number*}',                                        # nineteen
    sep = '|'))

  numberOrInterval <- regex_data_or(dose_dict, paste(
    '{number*}',        # redundant as this is part of uber_number anyway?
    '{ordNumber*}',     # first, second (but not 1st, 2nd, ...)
    '\\b[0-9]{{1,3}}\\b', # any standalone number between 1 and 3 digits
    '{uber_number*}',
    sep = '|'))

  times <- regex_data_or(dose_dict, paste(
    'once|twice|thrice',
    '(?:up)? ?(?:to)? ?(?:{numberOrInterval*}) times?',
    sep = '|'))

  perTimeUnit <- regex_data_or(dose_dict, paste(
    '(?:{every*}) ?(?:{numberOrInterval})? ?\\w? ?(?:{timeUnit*})', # every (1) week
    '{period*}',                                              # dinner time
    '(?:{every*}|{when*}) ?\\w? ?(?:{period*})',                  # every x week
    'times?',                                                 # times
    'times? ?(?:{every*})? ?(?:{timeUnit*})',                     # times per week
    '(?:times? )?(?:{timeUnitLy*})',                              # times weekly
    sep = '|'))

  asNeeded <- regex_data_or(dose_dict, '{asNeeded*}')

  half <- '(?:and|&) (?:a )?half'

  extra <- '(?:to|or) (?:one|1)'

  freqAbbrevHour <- regex_data_or(dose_dict, paste(
    'q ?\\.? ?[0-9]{{1,3}} ?(?:{hour*})(?: ?\\.)?',                  # q.3h.
    'q ?\\.? ?[0-9]{{1,3}} ?- ?[0-9]{{1,3}} ?(?:{hour*})(?: ?\\.)?', # q.1-3 h.
    'q ?\\.? ?(?:{numberOrInterval}) (?:{hour*})(?: ?\\.)?',         #
    sep = '|'))

  allAbbrev <- regex_data_or(dose_dict, paste(
    '{latin*}',
    'q ?[.-]? ?(?:{timeAbbrev*})',
    'q ?\\.? ?(?:{numberOrInterval})(?: (?:{timeUnit*}))?',
    'every (?:{numberOrInterval}) (?:{timeUnit*})',
    '(?:{numberOrInterval}) per (?:{timeUnit*})',
    '{freqAbbrevHour}',
    '\\b[btq]\\.?d\\.?',
    sep = '|'))

  dns <- '\\(s\\)'

  # EXTRACTION -----------------------------------------------------------------
  dose_number <- regex_data_or(dose_dict, paste(sep = '|',
    # Here we are *extracting*, not just matching whole patterns.
    # '(?<=(?:{number*}) ?x ?)(?:{number*}) ?x ?(?:{number*}) ?(?:{ml*})', # redundant?
    '(?:{number*}) ?x ?(?:{number*}) ?(?:{ml*})',
    '(?:{number*})(?= ?x ?[0-9] hly)',
    '(?:{number*})\\.(?:{number*})(?= ?x ?[0-9] hly)',
    '((?:{uber_number}) ?)?[0-9] ?-? ?\\.?[0-9]? ?(?:{ml*})(?= ?(?:{spoon*}))',
    '(?<=(?:{verb*}) )(?:{uber_number})(?=\\.? ?doses?)',
    #'[0-9]\\.?[0-9]? ?x ?[0-9]\\.?[0-9]? ?(?:{ml*})',
    # '(?<=[0-9]\\.?[0-9]? ?x ?)[0-9]\\.?[0-9]? ?x ?[0-9]\\.?[0-9]? ?(?:{ml*})', # redundant
    '(?<=^mdu ?mitte ?)(?:{uber_number})(?= ?(?:{latin*}))',
    '(?<=^(?:{verb*}) ?(?:{perTimeUnit}) ?)[0-9][.-]?[0-9]? ?(?:{ml*})',
    '(?<=^.?)[0-9][.-]?[0-9]? ?(?:{ml*}) ?(?:{mini*}) ?[0-9][.-]?[0-9]? ?(?:{ml*})', # check! is the .? appropriate?
    '(?<=^.?)[0-9] ?(?:[.-]|or)? ?[0-9]? (?:{DoseUnit*})', # check!
    '(?<=^.?)[0-9] ?(?:[.-]|or)? ?[0-9]?(?: [pd])?(?= ?(?:{latin*}))', # check!
    '(?<=^(?:{verb*}) )(?:{uber_number})(?= (?:{when*}|{small*}|{perTimeUnit}))',
    '(?<=^.{{0,3}})(?:{uber_number})(?= {dose_unit})', # risk of causing '10 mg' to read as '10', for example
    # '(?<=^to be )applied',
    '(?<=^(?:to be )?appl(?:y|ied) )(?:{times})',
    '(?<=^(?:to be )?)appl(?:y|ied)(?! (?:{times}))', # NB: extract iff no number is present
    '(?<=/? ?/? ?)(?:{uber_number})(?= (?:{when*}|{every*}) (?:{period*}))', # conflicts with line 98
    '(?:(?:{uber_number}) ?)?(?:x ?)?(?:{uber_number}) (?:{ml*})',
    '(?:{uber_number}) (?:or|to) (?:{uber_number})(?= (?:{dose_unit}))',
    '(?<=^x ?)(?:{uber_number})',
    sprintf('(?<=^(?:(?:{verb*}) )?)(?:%s)', # patterns starting w/ optional verb
            paste('(?:{dose_unit})(?=(?:{dns})?(?: (?:{times})))',
                  '(?:{times}|{uber_number})(?= (?:{timeUnitLy*}))',
                  '(?:{roman*})(?= (?:immediately|{dose_unit}))',
                  '[0-9][.-]?[0-9]?(?= (?:(?:{every*}) )?(?:{timeUnit*}))',
                  'up to (?:{uber_number})(?= (?:{dose_unit}|{allAbbrev}|{perTimeUnit}|{timeUnit*}|{small*}|{when*}))',
                  '(?:{uber_number}) (?:. )?then (?:{uber_number})',
                  '(?:{uber_number}) ?. ?(?:{uber_number})(?= after)',
                  '(?:{uber_number}) ?. ?and (?:{uber_number})(?= ?. ?(?:{perTimeUnit}|(?:{uber_number}) (?:{timeUnit*})))', # ?
                  '(?:{uber_number}) (?:{when*}|{every*}) (?:{period*})(?: (?:{uber_number}) (?:{when*}|{every*}) (?:{period*}))?',
                  sep = '|')
    )
  ))
})
