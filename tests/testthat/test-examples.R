context('Text mining miscellaneous highlighted examples')

example1 <- 'Diclofenac sodium 50mg gastro-resistant tablets'
example2 <- 'Bendroflumethiazide 2.5mg tablets'
example3 <- 'Co-amoxiclav 250mg/125mg tablets'
example4 <- 'Zyban 150mg modified-release tablets (GlaxoSmithKline UK Ltd)'

test_that('Do not parse unit abbreviations in middle of words', {
    out1 <- extract_from_prescription(example1)
    out2 <- extract_from_prescription(example2)
    expect_equal(out1$unit, 'milligram') # not 'iu'
    expect_equal(out2$unit, 'milligram') # not 'drop'
})

test_that('Handle multiple doses separated by slashes', {
    out3 <- extract_from_prescription(example3)
    expect_equal(out3$dose, '250-125')
})

test_that('Do not parse frequency abbreviations in middle of words', {
    out4 <- extract_from_prescription(example4)
    expect_equal(out4$freq, NA_character_) # not '3' (= t.d.)
})

test_that('Latin "qds" means four times a day, not daily', {
    qds <- extract_from_prescription(c('take 10mls q.d.s. p.r.n.',
                                       'take 10mls qds prn',
                                       'take 2 qds',
                                       '10mgqds'))
    expect_equal(qds$freq, rep('4', 4))
    expect_equal(qds$optional, c(1, 1, 0, 0))
    # "qd" (quaque die) is once a day
    qd <- extract_from_prescription(c('1 tab qd', '1 tab q.d.'))
    expect_equal(qd$freq, c('1', '1'))
})
