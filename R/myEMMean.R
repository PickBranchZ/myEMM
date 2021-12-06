#' This is some description of this function.
#' @title my estimated marginal mean calculator
#'
#' @description Obtain estimated marginal mean and its confidence interval based on given linear models.
#'
#' @param object object is a linear regression model
#' @param specs a variable or a vector of variables to calculate EMM with
#'
#' @return a list of EMMs for each spec
#' @import stats
#' @examples
#' myEMM(lm(mpg ~ disp + wt + factor(cyl), data = mtcars), c('wt', 'cyl'))
#' @export
#'
myEMM <- function(object, specs){
  ## obtain basic information from input model
  data <- object$model[1:length(object$model)]
  classes <- attr(object$terms, 'dataClasses')

  ## remove factor() and I() in names
  mynames <- nameHelper(names(data))
  ## check specs to calculate included or not. If not, warning
  ck <- specs_ck(specs, mynames[-1])
  ## remove every symbols in the names
  mynames <- gsub("[[:punct:]|[:blank:]]", "", mynames)
  names(classes) <- mynames
  names(data) <- mynames

  ## build reference grid and generate new X according to each grid
  ref_grid <- myRefGrid(data, classes)
  newdata <- xGen(ref_grid, data, classes)
  names(newdata) <- mynames

  ## calculate EMM and related values
  results <- emmHelper(data, newdata, ck)
  return(results)
}






