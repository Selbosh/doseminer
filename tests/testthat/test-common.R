context('Common dosages')

# Using example prescriptions from 'common dosages', verify outputs.
common_dosages <- read.csv('tests/common_doses_textmining.csv')

test_that('Units match those extracted by old algorithm', {
  original_units <- common_dosages$DOSE.UNIT # = according to old algorithm
  original_units[original_units == '-'] <- NA
  unit_guesses <- guess_dose_unit(common_dosages$PRESCRIPTION)
  # Retrieval of the new algorithm is actually better, so we ignore cases where
  # the old algorithm did not guess a dose unit but the new method offered one.
  expect_equivalent(original_units[!is.na(original_units)],
                    unit_guesses[!is.na(original_units)])
})

test_that('Dose numbers match those extracted by old algorithm', {
  original_min <- common_dosages$DN.MIN
  original_max <- common_dosages$DN.MAX
  number_guesses <- guess_dose_number(common_dosages$PRESCRIPTION)
  # x <- cbind(text = common_dosages$PRESCRIPTION,
             # old_min = original_min, old_max = original_max,
             # new = number_guesses)
  expect_equivalent(original_min[original_min == original_max],
                    number_guesses[original_min == original_max])
})
