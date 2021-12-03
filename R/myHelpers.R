#' @noRd

## name function to remove factor() and I()
nameHelper <- function(names){
  for(i in 1:length(names)){
    if(grepl("factor\\(|I\\(",names[i])==TRUE){
      names[i] <- gsub("factor\\(|I\\(", '', names[i])
      names[i] <- gsub("\\)$", '', names[i])
    }
  }
  return(names)
}


## function to check specs to calculate included or not
specs_ck <- function(specs, mynames)
{
  ck <- specs %in% mynames
  if (sum(1-ck)!=0){
    stop(paste0('Variables ', specs[ck==FALSE], ' do not exit in the model.'))
  }
  ck <- mynames %in% specs
  return(ck)
}


## function to build reference grid
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


## function to generate new X according to each grid
xGen <- function(ref_grid, classes){
  n=1
  len=c()
  nx <- length(ref_grid)
  for(i in 1:nx){
    len=c(len, length(ref_grid[[i]]))
    n=n*length(ref_grid[[i]])
  }

  newdata <- as.data.frame(matrix(0, ncol = nx, nrow = n))

  newdata[ ,1] <- rep(ref_grid[[1]], n/len[1])
  if(nx>1){
    tmp=1
    for(i in 2:nx){
      tmp=tmp*len[i-1]
      newdata[ ,i] <- rep(rep(ref_grid[[i]], each=tmp), n/(len[i]*tmp))
      if(classes[i]=='factor'){
        newdata[ ,i] <- as.factor(newdata[ ,i])
      }
    }
  }
  return(newdata)
}


## function to calculate emm, se and confidence intervals
emmHelper <- function(data, newdata, ck){
  names <- names(data)
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
    emm <- as.vector(tapply(newdata$EMM, newdata[x], mean))

    X <- model.matrix(myFormula, data=newdata)
    V <- vcov(m1)
    corr_mat <- X %*% V %*% t(X)
    if(class(data[,x])!='factor'){
      se <- sqrt(sum(corr_mat)/nrow(corr_mat))
    }else{
      se <- c()
      for (j in levels(data[,x])){
        x_index <- newdata[,x]==j
        x_mat <- matrix(0, ncol = length(x_index), nrow = length(x_index))
        diag(x_mat) <- x_index
        corr_tmp <- x_mat %*% corr_mat %*% x_mat
        se <- c(se, sqrt(sum(corr_tmp))/sum(x_index))
      }
    }
    lower.CL <- emm - qt(0.975, df)*se
    upper.CL <- emm + qt(0.975, df)*se
    res[[i]] <- round(cbind(emm, se, lower.CL, upper.CL), 2)
  }

  return(res)
}
