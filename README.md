This package constitutes an interactive R problem set based on the RTutor package (https://github.com/skranz/RTutor). 

Welcome! This is a RTutor Problem Set, containing the main results from the paper
“Moral Hazard, Incentive Contracts and Risk: Evidence from Procurement” from Patrick Bajari (University of Washington and NBER)
and Gregory Lewis (Harvard University and NBER) published 2013. They analyzed contracts of road construction projects and compared
different contract strategies using alternative time incentives.

The paper can be downloaded here: https://academic.oup.com/restud/article-abstract/81/3/1201/1602080?redirectedFrom=fulltext


## 1. Installation

RTutor and this package is hosted on Github. To install everything, run the following code in your R console.
```s
if (!require(devtools))
  install.packages("devtools")
source_gist("gist.github.com/skranz/fad6062e5462c9d0efe4")
install.rtutor(update.github=TRUE)

devtools::install_github("ClaMaSch/RTutorIncentiveContracts", upgrade_dependencies=FALSE)
```

## 2. Show and work on the problem set
To start the problem set first create a working directory in which files like the data sets and your solution will be stored. Then adapt and run the following code.
```s
library(RTutorIncentiveContracts)

# Adapt your working directory to an existing folder
setwd("C:/problemsets/RTutorIncentiveContracts")
# Adapt your user name
run.ps(user.name="Jon Doe", package="RTutorIncentiveContracts",
       load.sav=TRUE, sample.solution=FALSE)
```
If everything works fine, a browser window should open, in which you can start exploring the problem set.
