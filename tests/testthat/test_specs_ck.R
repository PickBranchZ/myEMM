library(myEMM)

test_that('specs_ck check whether given specs exits in mynames',{
  test_specs1 <- c('Edu','Gender')
  test_mynames1 <- c('BMI', 'Income', 'Gender', 'IncomeGender', 'Work','Edu')
  res1 <- c(FALSE, FALSE, TRUE, FALSE, FALSE, TRUE)
  test_specs2 <- c('e')
  test_mynames2 <- c('a', 'b', 'c', 'd', 'e', 'f')
  res2 <- c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE)
  test_specs3 <- c('e', 'g')
  test_mynames3 <- c('a', 'b', 'c', 'd', 'e', 'f')
  # res3 <- c(FALSE, FALSE, FALSE, FALSE, TRUE, FALSE)
  expect_equal(specs_ck(test_specs1, test_mynames1), res1)
  expect_equal(specs_ck(test_specs2, test_mynames2), res2)
  expect_error(specs_ck(test_specs3, test_mynames3))
})
