context('Text mining utility functions')

test_that('Convert English number words to integers', {
  expect_equivalent(english2int(c('one', 'two', 'three')), c('1', '2', '3'))
  expect_equivalent(english2int('twenty four'), '24')
  expect_equivalent(english2int(c('once', 'twice')), c('1', '2'))
})

test_that('Convert English half measures to decimals', {
  expect_equivalent(english2int(' and a half'), '.5')
  expect_equivalent(english2int('a half'), '0.5')
  expect_equivalent(english2int('half'), '0.5')
  expect_equivalent(english2int('one half'), '0.5')
})

test_that('Convert Latin abbreviations to daily integers', {
  expect_equivalent(latin2intdaily('alt sh'), 12)
  expect_equivalent(latin2intdaily('q4h'), 6)
  expect_equivalent(latin2intdaily('qqh'), 6)
  expect_equivalent(latin2intdaily('bds'), 2)
  expect_equivalent(latin2intdaily('q8h'), 3)
})

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

test_that('Text cleaning converts English half measures to decimals', {
  expect_equal(clean_prescription_text('a half'), '0.5')
  expect_equal(clean_prescription_text('one and a half'), '1.5')
  expect_equal(clean_prescription_text('two & one half'), '2.5')
  expect_equal(clean_prescription_text('26 and a half'), '26.5')
  expect_equal(clean_prescription_text('one half'), '0.5')
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

