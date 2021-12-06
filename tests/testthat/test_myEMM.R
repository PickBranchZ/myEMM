library(myEMM)
load('data/EgData.RData')

test_that('myEMM return a list of expected emmeans',{
  #expect_equal(myEmm(model1, specs1), emm_res1)
  #expect_equal(myEmm(model1, specs2), emm_res2)
  expect_equal(myEmm(model3, specs3), res3)
})
