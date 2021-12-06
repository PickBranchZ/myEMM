library(myEMM)
data("mtcars")

test_that('myEMM return a list of expected emmeans',{
  mod1 <- lm(mpg ~ disp + wt + factor(cyl), data = mtcars)
  mymat <- matrix(c(23.80, 1.43, 27, 20.87, 26.73,
                    19.49, 1.13, 27, 17.17, 21.82,
                    17.48, 1.38, 27, 14.65, 20.30), byrow = TRUE, ncol = 5)
  colnames(mymat) <- c('emm', 'se', 'df', 'lower.CL', 'upper.CL')
  rownames(mymat) <- c('4', '6', '8')
  res1 <- list('cyl'=mymat)
  expect_equal(myEMM(mod1, 'cyl'), res1)

  mymat <- matrix(c(20.26, 0.88, 27, 18.44, 22.07), byrow = TRUE, ncol = 5)
  colnames(mymat) <- c('emm', 'se', 'df', 'lower.CL', 'upper.CL')
  res2 <- list('disp'=mymat)
  expect_equal(myEMM(mod1, 'disp'), res2)
})
