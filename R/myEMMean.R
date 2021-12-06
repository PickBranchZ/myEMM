#' This is some description of this function.
#' @title my estimated marginal mean calculator
#'
#' @description the function is written for biostat625 HW4
#'
#' @details you can use this function to calculate estimated marginal mean and its confidence interval.
#'
#' @param object object is a lm model
#' @param specs a vector or variable to calculate EMM with
#'
#' @return a list of EMMs for each specs
#' @import stats
#' @export
#' @examples myEMM(model3, specs3)
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






