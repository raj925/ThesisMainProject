requiredPackages <- c("tibble", "psych", "ltm", "stringr", "ggsci", "ggplot2", "rjson", 
                      "reticulate", "ggpubr", "lme4", "lmerTest",
                      "pracma", "lattice", "MASS", "apcluster", "blme", 
                      "smacof","cluster","factoextra", "pwr", 
                      "magrittr", "tidyr", "dplyr", "boot", "rstatix", 
                      "devtools",  "fossil", "tidyverse", "proxy",
                      "plotly","viridis","padr", "wordcloud", "RColorBrewer", "tm", "logisticPCA", "rARPACK", "FactoMineR")

new.packages <- requiredPackages[!(requiredPackages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "http://cran.us.r-project.org")
lapply(requiredPackages, require, character.only = TRUE)
install_github("vqv/ggbiplot")
library(ggbiplot)

meanFun <- function(data, i){
  d <- data[i, ]
  return(mean(d))
  bo <- boot(data[, "xs", drop = FALSE], statistic=meanfun, R=5000)
  boot.ci(bo, conf=0.95, type="bca")
}

subsample <- function(x,n,r) {
  returnDf <- data.frame(matrix(ncol=n,nrow=r))
  for (iter in 1:r)
  {
    sampleArr <- x
    returnArr <- sample(sampleArr,size = n,replace = FALSE)
    returnDf[iter,] <- returnArr
  }
  return(returnDf)
}

jaccard <- function(a, b) {
  intersection = length(intersect(a, b))
  union = length(a) + length(b) - intersection
  return (intersection/union)
}

jaccardMat <- function(m) {
  retMat <- data.frame(matrix(ncol = ncol(m), nrow = nrow(m) ))
  for (x in 1:nrow(m))
  {
    for (y in 1:nrow(m))
    {
      if (x == y)
      {
        retMat[x,y] <- 0
      }
      else
      {
        retMat[x,y] <- jaccard(m[x,],m[y,])
      }
    }
  }
  return(retMat)
}

data_summary <- function(x) {
  m <- mean(x)
  ymin <- m-sd(x)
  ymax <- m+sd(x)
  return(c(y=m,ymin=ymin,ymax=ymax))
}


inversionNumber <- function(x){
    mergeSort <- function(x){
        if(length(x) == 1){
            inv <- 0
            #printind(' base case')
        } else {
            n <- length(x)
            n1 <- ceiling(n/2)
            n2 <- n-n1
            y1 <- mergeSort(x[1:n1])
            y2 <- mergeSort(x[n1+1:n2])
            inv <- y1$inversions + y2$inversions
            x1 <- y1$sortedVector
            x2 <- y2$sortedVector
            i1 <- 1
            i2 <- 1
            while(i1+i2 <= n1+n2+1){
                if(i2 > n2 || (i1 <= n1 && x1[i1] <= x2[i2])){ # ***
                    x[i1+i2-1] <- x1[i1]
                    i1 <- i1 + 1
                } else {
                    inv <- inv + n1 + 1 - i1
                    x[i1+i2-1] <- x2[i2]
                    i2 <- i2 + 1
                }
            }
        }
        return (list(inversions=inv,sortedVector=x))
    }
    r <- mergeSort(x)
    return (r$inversions)
}

kendallTauDistance <- function(x,y){
    n <- length(x)
    ub <- (0.5*n)*(n-1)
    return(1-(inversionNumber(order(x)[rank(y)])/ub))
}

requiredPackages <- c("rpart", "caret", "tidyverse", "data.table", "verification", "glmnet",
                      "GGally", "corrplot", "verification", "ROCR", "maptree",
                      "glmnet", "gridExtra", "randomForest", "mgcv", "nnet", "pROC",
                      "gbm", "e1071", "xgboost", "DT", "NeuralNetTools", "rpart.plot")
new.packages <- requiredPackages[!(requiredPackages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, repos = "http://cran.us.r-project.org")
lapply(requiredPackages, require, character.only = TRUE)

