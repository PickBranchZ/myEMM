library(myEMM)

test_that('nameHelper removes factor() and I()',{
  test_case1 <- c('Edu','factorEdu','f(Edu)')
  res1 <- c('Edu','factorEdu','f(Edu)')
  test_case2 <- c('factor', 'I')
  res2 <- c('factor', 'I')
  test_case3 <- c('factor(Edu)','I(Edu*Gender)','Edu*Gender')
  res3 <- c('Edu','Edu*Gender','Edu*Gender')
  expect_equal(nameHelper(test_case1), res1)
  expect_equal(nameHelper(test_case2), res2)
  expect_equal(nameHelper(test_case3), res3)
})
