context('Parsing English numbers')

test_that('Convert isolated English integer words', {
  expect_equivalent(words2number(c('one', 'two', 'three')), c(1, 2, 3))
  expect_equivalent(words2number(c('thirty seven', 'forty two')), c(37, 42))
  expect_equivalent(words2number('two hundred and fifty two'), 252)
  expect_equivalent(words2number('a thousand'), 1000)
})

test_that('Replace English integers in context', {
  expect_equivalent(replace_numbers(c('one', 'two', 'three')), c('1', '2', '3'))
  expect_equivalent(replace_numbers(c('twenty four hours', 'answer is forty two')),
                                    c('24 hours', 'answer is 42'))
  expect_equivalent(replace_numbers('three hundred and sixty five days'), '365 days')
  expect_equivalent(replace_numbers('take fifteen tablets'), 'take 15 tablets')
})

test_that('Convert English fractions to decimals', {
  expect_equivalent(words2number('half'), 0.5)
  expect_equivalent(words2number('a half'), 0.5)
  expect_equivalent(words2number('one and a half'), 1.5)
  expect_equivalent(words2number('three and a half'), 3.5)
})

test_that('Replace English fractions in context', {
  expect_equivalent(replace_numbers('half spoonful'), '0.5 spoonful')
  expect_equivalent(replace_numbers('take a half'), 'take 0.5')
  expect_equivalent(replace_numbers('no half measures!'), 'no 0.5 measures!')
  expect_equivalent(replace_numbers('one and a half tablets'), '1.5 tablets')
  expect_equivalent(replace_numbers('pi is three and a half'), 'pi is 3.5')
})

test_that('Text cleaning converts English half measures to decimals', {
  expect_equal(clean_prescription_text('a half'), '0.5')
  expect_equal(clean_prescription_text('one and a half'), '1.5')
  expect_equal(clean_prescription_text('two & one half'), '2.5')
  expect_equal(clean_prescription_text('26 and a half'), '26.5')
  expect_equal(clean_prescription_text('one half'), '0.5')
})
