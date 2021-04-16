context('Text mining utility functions')

test_that('Text cleaning coerces to lower case', {
  expect_equal(clean_prescription_text('FOO BAR'), 'foo bar')
})

test_that('Text cleaning trims whitespace', {
  expect_equal(clean_prescription_text(' hello,  world \t '), 'hello, world')
})

test_that('Text cleaning converts English number words to integers', {
  expect_equal(clean_prescription_text('one two three'), '1 2 3')
  expect_equal(clean_prescription_text('fourteen'), '14')
  expect_equal(clean_prescription_text('sixteen'), '16')
  expect_equal(clean_prescription_text('twenty-four'), '24')
  expect_equal(clean_prescription_text('twenty one'), '21')
})

test_that('Text cleaning standardises numerical ranges', {
  expect_equal(clean_prescription_text('10 - 20'), '10 - 20')
  expect_equal(clean_prescription_text('1 or 2'), '1 - 2')
  expect_equal(clean_prescription_text('5-6'), '5 - 6')
  expect_equal(clean_prescription_text('three to four'), '3 - 4')
  expect_equal(clean_prescription_text('twenty-six to twenty eight'), '26 - 28')
  expect_equal(clean_prescription_text('2- six'), '2 - 6')
  expect_equal(clean_prescription_text('1.5 to 30.0'), '1.5 - 30.0')
})

test_that('Text cleaning preserves alphanumeric Latin expressions', {
  expect_equal(clean_prescription_text('q4h prn'), 'q4h prn')
  expect_equal(clean_prescription_text('q.8.h.'), 'q8h')
  expect_equal(clean_prescription_text('q 8 h'), 'q8h')
})

test_that('Convert Latin abbreviations to daily integer frequencies', {
  expect_equivalent(extract_from_prescription('q4h')$freq, '6')
  expect_equivalent(extract_from_prescription('qqh')$freq, '6')
  expect_equivalent(extract_from_prescription('bds')$freq, '2')
  expect_equivalent(extract_from_prescription('q8h')$freq, '3')
  expect_equivalent(extract_from_prescription('qh')$freq, '24')
})

test_that('Convert ranges of spoon measurements', {
  expect_equivalent(extract_from_prescription('Four 5ml spoonfuls up to 4 times a day')$dose, '20')
  expect_equivalent(extract_from_prescription('Two to four 5ml spoonfuls up to 4 times a day')$dose, '10-20')
})

