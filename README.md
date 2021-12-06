# myEMM
<!-- badges: start -->
[![R-CMD-check](https://github.com/PickBranchZ/myEMM/workflows/R-CMD-check/badge.svg)](https://github.com/PickBranchZ/myEMM/actions)

[![codecov](https://codecov.io/gh/PickBranchZ/myEMM/branch/main/graph/badge.svg?token=8OOI3T11X4)](https://codecov.io/gh/PickBranchZ/myEMM)
<!-- badges: end -->

## Overview
Estimated marginal means (EMMs), a.k.a. least-squares means, are predictions on a reference grid of predictor settings, or marginal averages thereof. Although package 'emmeans' is enormously useful to do post hoc comparisons among groups after fitting a model, it will try to get access to original data which means there may be something wrong when the original data is changed or deleted. So, I write my own EMM calculator, 'myEMM', to calculate EMMs as well as its standard errors and confidence intervals based on the given model only. The function will generate data similar to the original one simplely from the given model to avoid the issue for 'emmeans'. The function needs input of a linear model as well as expected specs to calculate EMM with and will return a list of EMMs as well as its standard errors and confidence intervals.


## Installation
The package can be downloaded from github site [https://github.com/PickBranchZ/myEMM](url) or through 'install_github' function of the package 'devtools'.
```
# install.packages("myEMM")
devtools::install_github("PickBranchZ/myEMM")
```

## Usage
```
library(myEMM)

data(mtcars)
m1 = lm(mpg ~ disp + wt + factor(cyl), data = mtcars)
myEMM(m1, 'cyl')
#> $cyl
#>     emm   se df lower.CL upper.CL
#> 4 23.80 1.43 27    20.87    26.73
#> 6 19.49 1.13 27    17.17    21.82
#> 8 17.48 1.38 27    14.65    20.30
```

## Getting help
If you encounter a bug, or have questions and other discussion please contact through [longfeiz@umich.edu](url).
