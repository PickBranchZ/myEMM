#' @title nameHelper
#' @description name function to remove factor() and I()
#' @param names names is the original vector of names
#' @return a vector of EMMs for each specs
#'
nameHelper <- function(names){
  for(i in 1:length(names)){
    if(grepl("factor\\(|I\\(",names[i])==TRUE){
      names[i] <- gsub("factor\\(|I\\(", '', names[i])
      names[i] <- gsub("\\)$", '', names[i])
    }
  }
  return(names)
}

#' @title specs_ck
#' @description function to check specs to calculate included or not
#' @param specs specs is a vector of expected specs
#' @param mynames mynames is a vector of names
#' @return a boll vector of expected specs in mynames or warning when there is specs not in mynames
#'
specs_ck <- function(specs, mynames)
{
  ck <- specs %in% mynames
  if (sum(1-ck)!=0){
    warning(paste0('Variables ', specs[ck==FALSE], ' do not exit in the model.'))
    ck <- ck[which(ck==TRUE)]
  }
  ck <- mynames %in% specs
  return(ck)
}


#' @title myRefGrid
#' @description function to build reference grid of each variable
#' @param data data is a data.frame of model data
#' @param classes classes is a vector of classses of variables in the data
#' @return a list of reference grid of each variable
#'
myRefGrid <- function(data, classes){
  tmp <- list(NULL)
  length(tmp)<-ncol(data)
  for(i in 1:ncol(data)){
    if(classes[i]=='numeric'){
      tmp[[i]] <- mean(data[, i])
    }else{
      tmp[[i]] <- levels(data[,i])
    }
  }
  return(tmp)
}

#' @title xGen
#' @description function to generate new X according to each grid
#' @param ref_grid ref_grid is a list of reference grid of each variable
#' @param data data is a data.frame of model data
#' @param classes classes is a vector of classses of variables in the data
#' @return a data.frame of newly generated data
#'
xGen <- function(ref_grid, data, classes){
  ## calculate needed # of rows of newdata for every category
  n=1
  len=c()
  nx <- length(ref_grid)
  for(i in 1:nx){
    len=c(len, length(ref_grid[[i]]))
    n=n*length(ref_grid[[i]])
  }

  ## generate newdata according to reference grid
  newdata <- as.data.frame(matrix(0, ncol = nx, nrow = n))
  newdata[ ,1] <- rep(ref_grid[[1]], n/len[1])
  if(nx>1){
    tmp=1
    for(i in 2:nx){
      tmp=tmp*len[i-1]
      newdata[ ,i] <- rep(rep(ref_grid[[i]], each=tmp), n/(len[i]*tmp))
      if(classes[i]=='factor'){
        newdata[ ,i] <- as.factor(newdata[ ,i])
        levels(newdata[ ,i]) <- levels(data[ ,i])
      }
    }
  }
  return(newdata)
}

#' @title emmHelper
#' @description function to calculate emm, se and confidence intervals
#' @param data data is a data.frame of model data
#' @param newdata newdata is a data.frame of newly generated data
#' @param ck ck is a boll vector of expected specs in mynames
#' @return a list of emmeams, se and confidence interval of each expected spec
#'
emmHelper <- function(data, newdata, ck){
  names <- names(data)

  ## fit a same model with my names and calculate emm for each detailed category
  myFormula <- as.formula(paste0(names[1], '~', paste(names[2:length(names)], collapse = ' + ')))
  m1 <- lm(myFormula, data)
  pred <- predict(m1, newdata, se.fit = TRUE)
  newdata$EMM <- pred$fit
  newdata$se <- pred$se.fit
  df <- pred$df

  res <- list(NULL)
  length(res) <- sum(ck)
  for(i in 1:length(res)){
    x = which(ck==TRUE)[i]+1
    names(res)[i] = names(data)[x]

    ##generate covariance matrix
    X <- model.matrix(myFormula, data=newdata)
    V <- vcov(m1)
    corr_mat <- X %*% V %*% t(X)

    ##calculate emm and se
    if(class(data[,x])!='factor'){              ## for not factor x
      emm <- mean(newdata$EMM)
      se <- sqrt(sum(corr_mat)/nrow(corr_mat))
    }else{                                      ## for factor x
      emm <- c()
      se <- c()
      for (j in levels(newdata[, x])){
        ##calculate emm
        emm <- c(emm, mean(newdata$EMM[newdata[,x]==j]))

        ##generate corresponding covariance matrix for each x and calculate se
        x_index <- as.numeric(newdata[,x]==j)
        x_mat <- matrix(0, ncol = length(x_index), nrow = length(x_index))
        diag(x_mat) <- x_index
        corr_tmp <- x_mat %*% corr_mat %*% x_mat
        se <- c(se, sqrt(sum(corr_tmp))/sum(x_index))
      }
    }

    ## calculate confidence intervals with emm and se
    lower.CL <- emm - qt(0.975, df)*se
    upper.CL <- emm + qt(0.975, df)*se

    ## combine all results
    res[[i]] <- round(cbind(emm, se, df, lower.CL, upper.CL), 2)
    row.names(res[[i]]) <- levels(newdata[, x])
  }

  return(res)
}
