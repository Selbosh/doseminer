#' Sample electronic prescribing dataset
#'
#' A dataset containing product codes, patient identifiers, quantities, dates
#' and free-text dose instructions, similar to data provided by the Clinical
#' Practice Research Datalink (CPRD).
#'
#' Variables in the data include
#' \describe{
#' \item{id}{record identifier}
#' \item{patid}{patient identifier}
#' \item{date}{date of start of prescription}
#' \item{prodcode}{product code; identifier for the prescribed medication}
#' \item{qty}{total quantity of medication prescribed}
#' \item{text}{free text prescribing instructions}
#' }
#'
'cprd'

#' Example freetext prescriptions
#'
#' Various examples of how prescription data may be represented in free text.
#'
#' @seealso \code{\link{example_cprd}}
#'
#' @export
example_prescriptions <- c(
  '1 tablet to be taken daily',
  '2.5ml four times a day when required',
  '1.25mls three times a day',
  'take 10mls q.d.s. p.r.n.',
  'take 1 or 2 4 times/day',
  '2x5ml spoon 4 times/day',
  'take 2 tablets every six hours max eight in twenty four hours',
  '1 tab nocte twenty eight tablets',
  '1-2 four times a day when required',
  'take one twice daily', # WARNING: converts to 'take 1 2 daily' - ambiguous
  '1 q4h prn',
  'take two every three days',
  'five every week',
  'every 72 hours',
  '1 x 5 ml spoon 4 / day for 10 days',
  'two to three times a day',
  'three times a week',
  'three 5ml spoonsful to be taken four times a day after food',
  'take one or two every 4-6 hrs',
  '5ml 3 hrly when required',
  'one every morning to reduce bp',
  'take 1 or 2 6hrly when required',
  'take 1 or 2 four times a day as required for pain',
  'take 1 or 2 4 times/day if needed for pain',
  '1-2 tablets up to four times daily',
  'take one or two tablets 6-8 hrly every 2-3 days',
  'one and a half tablets every three hours')

#' Example freetext prescriptions
#'
#' Adapted from CPRD common dosages
#'
#' @seealso \code{\link{example_prescriptions}}
#'
#' @export
example_cprd <-
  c(
    'TAKE 1 OR 2 3 TIMES/DAY',
    'TAKE 1 OR 2 FOUR TIMES A DAY WHEN REQUIRED',
    'TAKE 1 OR 2 AS DIRECTED',
    'TAKE 1 OR 2 4 TIMES/DAY AS REQUIRED',
    'TAKE ONE TWICE DAILY',
    'TAKE 1 OR 2 EVERY 6HRLY WHEN REQUIRED',
    'TAKE 1 OR 2 4 TIMES/DAY',
    'TAKE 1 OR 2 4 TIMES/DAY IF NEEDED FOR PAIN',
    'TAKE 1 OR 2 4 TIMES/DAY WHEN REQUIRED',
    '1-2 FOUR TIMES A DAY WHEN REQUIRED',
    '1 THREE TIMES A DAY WHEN REQUIRED',
    'TAKE 1 OR 2 FOUR TIMES A DAY AS REQUIRED FOR FOR FOR PAIN',
    '1-2 TABLETS UP TO FOUR TIMES DAILY',
    'TAKE ONE 2 TIMES/DAY',
    'TAKE ONE 2 TIMES A DAY',
    '1-2   FOUR TIMES A DAY',
    '1-2 FOUR TIMES A DAY',
    '1 OR 2 TABLETS FOUR TIMES A DAY',
    'ONE OR TWO FOUR TIMES A DAY WHEN REQUIRED',
    'TAKE 1-2 UP TO FOUR TIMES A DAY WHEN REQUIRED',
    'TAKE 1 OR 2 FOUR TIMES A DAY',
    'TAKE 1-2 THREE TIMES A DAY',
    'TAKE AS DIRECTED',
    'APPLY ONE WEEKLY',
    'ONE THREE TIMES A DAY AS REQUIRED',
    '1 OR 2 FOUR TIMES A DAY FOR FOR FOR FOR PAIN WHEN REQUIRED',
    '2 FOUR TIMES A DAY',
    '1 TWICE A DAY'
  )
