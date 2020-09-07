context('Pattern matching')

library(stringr)
dose_rx <- function(x, p) {
  if (any(y <- grepl('\\w(ml|msl)', x))) # could make this ([a-z]\\d|\\d[a-z])
    stop('Words and numbers must be separated in inputs: ', x[y])
  str_extract_all(x, doseminer:::patterns[[p]], simplify = TRUE)
}

test_pattern <- function(p, x, y = NULL) {
  if (is.null(y)) {
    expect_equivalent(dose_rx(names(x), p), x)
  } else
    expect_equivalent(dose_rx(x, p), y)
}

test_that('Match longer numbers greedily (fourteen, not four; 19 not 1)', {
  # If only fragments are being captured, e.g. '15' only returns '1', then
  # it is a problem with the precedence of 'or' operators in the regex.
  # Can fix by reordering the entries, or by writing more sophisticated
  # patterns, e.g. 'six(ty|teen)?' rather than 'six|sixty|sixteen'
  test_pattern('uber_number',
               'fourteen nineteen eighteen 20 15 1000 99',
               c('fourteen', 'nineteen', 'eighteen', 20, 15, 1000, 99))
  test_pattern('uber_number',
               '5.5 - 6.5', '5.5 - 6.5')
  test_pattern('uber_number',
               'a period of 3-5 years', '3-5')
  test_pattern('uber_number',
               'there are no numbers in this string!', character(0))
})

test_that('Match misspelt dose units', {
  # Make sure that the start of the word is captured for misspellings,
  # but equally, if the whole word is spelt correctly, capture the whole word
  # and not just the start of it.
  test_pattern('dose_unit',
               c('5 millilitres, every day', '5 millilrtrs', '5 msl'),
               c('millilitres', 'millil', 'msl'))
  test_pattern('dose_unit',
               c('10 lozenges', '9 losenges', '5 sprys', '6 sprays',
                 '4 suppostry per day', 'suppository'),
               c('lozenges', 'losenges', 'spr', 'sprays', 'suppos',
                 'suppository'))
})

test_that('Match hourly frequencies', {
  test_pattern('freqAbbrevHour',
               c('take q.3h.', 'q3h', 'q 3 h', 'q.1-3 h.', 'q 1-3 h',
                 'q 10-21 hour', 'q three hour'),
               c('q.3h.', 'q3h', 'q 3 h', 'q.1-3 h.', 'q 1-3 h', 'q 10-21 hour',
                 'q three hour'))
})

context('Pattern extraction')

test_that('Extract dose number', {
  numbers <- c(
    # Text input  = Expected output
    '2x3x4 ml'    = '3x4 ml',
    '5x10 ml'     = '5x10 ml',
    'two x 6 hly' = 'two',
    '3.1 x 4 hly' = '3.1',
    '1-2 2.5 ml spoonsful' = '1-2 2.5 ml',
    'two 5-6 ml spoonfuls' = 'two 5-6 ml',
    'inhale 2-5 doses' = '2-5',
    'take three to five doses' = 'three to five',
    'use one dose' = 'one',
    '0.9 x 9.9 mls' = '0.9 x 9.9 mls',
    '1.2 x 3.4 x 5.6 ml' = '3.4 x 5.6 ml',
    '1.2 x 1 x 2 mls' = '1 x 2 mls',
    'mdu mitte two b.d.' = 'two',
    'take every 2 weeks 5-6 mls' = '5-6 mls',
    '5.5 ml to 6.6 ml every day' = '5.5 ml to 6.6 ml',
    '1-2 ml or 3-4 ml weekly' = '1-2 ml or 3-4 ml',
    '1 ml / 9 ml lorem ipsum' = '1 ml / 9 ml',
    ' 10 mg dolor sit amet' = '10 mg',
    '5-9 gm foo bar' = '5-9 gm',
    '\t3.3 mgs of stuff' = '3.3 mgs',
    '#91 micrograms daily' = '91 micrograms',
    '6.7 p mane' = '6.7 p',
    '7-8 q.w.k' = '7-8',
    '7 or 8 qqh' = '7 or 8'
  )
  test_pattern('dose_number', numbers)

  with_verbs <- c(
    'take 5-6 at dinner' = '5-6',
    'inject eight when needed' = 'eight',
    'use 1 or 2 during meals' = '1 or 2',
    'inhale 9 every 3 to 4 hours' = '9',
    '- two to three pills' = 'two to three',
    'two to three pills' = 'two to three',
    # ^ this highlights a problem with the initial `any` patterns in the original code
    #'to be applied' = 'applied',
    'to be applied thrice' = 'thrice',
    'to be applied 3-4 times' = '3-4 times',
    'to be applied up to three times' = 'up to three times'
  )
  test_pattern('dose_number', with_verbs)

  ranges_of_values <- c(
    '// 2-3 every morning' = '2-3',
    '/two to three at bed time' = 'two to three',
    '5.5 each evening' = '5.5',
    '2.5 - 3.5 every teatime' = '2.5 - 3.5',
    'applied' = 'applied', 'apply' = 'apply',
    'apply up to 3 times' = 'up to 3 times',
    '2-3 x 1.5 mls daily' = '2-3 x 1.5 mls',
    'two or three x 10 msl today' = 'two or three x 10 msl',
    'two to three x 6 ml foo' = 'two to three x 6 ml',
    '2-3x6 mls' = '2-3x6 mls', # need space before {ml}. added by preprocessing
    '3.4 to 5.6 mls' = '3.4 to 5.6',
    '1-2 or 3-5 mg' = '1-2 or 3-5',
    'x eight' = 'eight',
    'x 1-2' = '1-2',
    'once daily' = 'once',
    '3 times weekly' = '3 times',
    'iv immediately' = 'iv',
    'iii capsules' = 'iii',
    'use tablet(s) once' = 'tablet',
    '2-3 every hour' = '2-3',
    '1.5 each week' = '1.5',
    'use up to 5 capsules' = 'up to 5',
    'up to 2 every 3 days' = 'up to 2',
    'take up to ten every week' = 'up to ten',
    'up to four q.4.h' = 'up to four',
    'up to 3 week' = 'up to 3',
    'inhale up to ten if necessary' = 'up to ten',
    'instil up to two before breakfast' = 'up to two',
    'take 1-2 - then 2-3' = '1-2 - then 2-3',
    'one - 2-3 after meals' = 'one - 2-3',
    'three - and four every 2 weeks' = 'three - and four',
    'four - and 3 4 hrs' = 'four - and 3' # ?
    #'use two each morning 5 every evening' = 'two each morning 5 every evening' #?
  )
  test_pattern('dose_number', ranges_of_values)
})
