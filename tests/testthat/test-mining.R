context('Text mining example prescriptions')

# NB: don't make these test too strict. They are to verify correctness of the
# algorithm as it is written; not necessarily real-world performance.

examples <- c(
  '1 tablet to be taken daily',
  '2.5ml four times a day when required',
  '1.25mls three times a day',
  'take 10mls q.d.s. p.r.n.',
  'take 1 or 2 4 times/day',
  '2x5ml spoon 4 times/day',
  'take 2 tablets every six hours max eight in twenty four hours',
  '1 tab nocte twenty eight tablets',
  '1-2 four times a day when required',
  'take one twice daily',
  '1 q4h prn',
  'take two every three days',
  'five every week',
  'every 72 hours',
  '1 x 5 ml spoon 4 / day for 10 days')

test_that('2 + 2 = 4', {
  expect_equal(2 + 2, 4)
})

test_that('"every other day" => daily interval of 2' {
  expect_equivalent(daily_interval('every other day'), 2)
  expect_equivalent(daily_interval('every 3 days'), 3)
  expect_equivalent(daily_interval('twice a week'), 3.5) # later convert fractional intervals into c(floor, ceiling)
  expect_equivalent(daily_interval('three times a week'), 2.33)
  expect_equivalent(daily_interval('every week'), 7)
  expect_equivalent(daily_interval('every other week'), 14)
  expect_equivalent(daily_interval('dieb alt wk'), 14)
})
