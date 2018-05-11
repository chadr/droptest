\name{fit}
\alias{fit}
\docType{data}
\title{
%%   ~~ data name/kind ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("fit")}
\format{
  The format is:
List of 30
 $ coefficients     : Named num [1:6] 2.674 2.73 0.734 1.593 1.253 ...
  ..- attr(*, "names")= chr [1:6] "sprayA" "sprayB" "sprayC" "sprayD" ...
 $ residuals        : Named num [1:72] -0.3103 -0.5172 0.3793 -0.0345 -0.0345 ...
  ..- attr(*, "names")= chr [1:72] "1" "2" "3" "4" ...
 $ fitted.values    : Named num [1:72] 14.5 14.5 14.5 14.5 14.5 14.5 14.5 14.5 14.5 14.5 ...
  ..- attr(*, "names")= chr [1:72] "1" "2" "3" "4" ...
 $ effects          : Named num [1:72] -35.27 -37.03 -3.67 -12.23 -8.12 ...
  ..- attr(*, "names")= chr [1:72] "sprayA" "sprayB" "sprayC" "sprayD" ...
 $ R                : num [1:6, 1:6] -13.2 0 0 0 0 ...
  ..- attr(*, "dimnames")=List of 2
  .. ..$ : chr [1:6] "sprayA" "sprayB" "sprayC" "sprayD" ...
  .. ..$ : chr [1:6] "sprayA" "sprayB" "sprayC" "sprayD" ...
 $ rank             : int 6
 $ qr               :List of 5
  ..$ qr   : num [1:72, 1:6] -13.191 0.289 0.289 0.289 0.289 ...
  .. ..- attr(*, "dimnames")=List of 2
  .. .. ..$ : chr [1:72] "1" "2" "3" "4" ...
  .. .. ..$ : chr [1:6] "sprayA" "sprayB" "sprayC" "sprayD" ...
  ..$ rank : int 6
  ..$ qraux: num [1:6] 1.29 1 1 1 1 ...
  ..$ pivot: int [1:6] 1 2 3 4 5 6
  ..$ tol  : num 1e-11
  ..- attr(*, "class")= chr "qr"
 $ family           :List of 12
  ..$ family    : chr "poisson"
  ..$ link      : chr "log"
  ..$ linkfun   :function (mu)  
  ..$ linkinv   :function (eta)  
  ..$ variance  :function (mu)  
  ..$ dev.resids:function (y, mu, wt)  
  ..$ aic       :function (y, n, mu, wt, dev)  
  ..$ mu.eta    :function (eta)  
  ..$ initialize:  expression({  if (any(y < 0))  stop("negative values not allowed for the 'Poisson' family")  n <- rep.int(1, nobs| __truncated__
  ..$ validmu   :function (mu)  
  ..$ valideta  :function (eta)  
  ..$ simulate  :function (object, nsim)  
  ..- attr(*, "class")= chr "family"
 $ linear.predictors: Named num [1:72] 2.67 2.67 2.67 2.67 2.67 ...
  ..- attr(*, "names")= chr [1:72] "1" "2" "3" "4" ...
 $ deviance         : num 98.3
 $ aic              : num 377
 $ null.deviance    : num 2265
 $ iter             : int 5
 $ weights          : Named num [1:72] 14.5 14.5 14.5 14.5 14.5 ...
  ..- attr(*, "names")= chr [1:72] "1" "2" "3" "4" ...
 $ prior.weights    : Named num [1:72] 1 1 1 1 1 1 1 1 1 1 ...
  ..- attr(*, "names")= chr [1:72] "1" "2" "3" "4" ...
 $ df.residual      : int 66
 $ df.null          : int 72
 $ y                : Named num [1:72] 10 7 20 14 14 12 10 23 17 20 ...
  ..- attr(*, "names")= chr [1:72] "1" "2" "3" "4" ...
 $ converged        : logi TRUE
 $ boundary         : logi FALSE
 $ model            :'data.frame':	72 obs. of  2 variables:
  ..$ count: num [1:72] 10 7 20 14 14 12 10 23 17 20 ...
  ..$ spray: Factor w/ 6 levels "A","B","C","D",..: 1 1 1 1 1 1 1 1 1 1 ...
  ..- attr(*, "terms")=Classes 'terms', 'formula'  language count ~ spray - 1
  .. .. ..- attr(*, "variables")= language list(count, spray)
  .. .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. ..$ : chr [1:2] "count" "spray"
  .. .. .. .. ..$ : chr "spray"
  .. .. ..- attr(*, "term.labels")= chr "spray"
  .. .. ..- attr(*, "order")= int 1
  .. .. ..- attr(*, "intercept")= int 0
  .. .. ..- attr(*, "response")= int 1
  .. .. ..- attr(*, ".Environment")=<environment: R_GlobalEnv> 
  .. .. ..- attr(*, "predvars")= language list(count, spray)
  .. .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "factor"
  .. .. .. ..- attr(*, "names")= chr [1:2] "count" "spray"
 $ call             : language glm(formula = count ~ spray - 1, family = poisson, data = InsectSprays)
 $ formula          :Class 'formula'  language count ~ spray - 1
  .. ..- attr(*, ".Environment")=<environment: R_GlobalEnv> 
 $ terms            :Classes 'terms', 'formula'  language count ~ spray - 1
  .. ..- attr(*, "variables")= language list(count, spray)
  .. ..- attr(*, "factors")= int [1:2, 1] 0 1
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:2] "count" "spray"
  .. .. .. ..$ : chr "spray"
  .. ..- attr(*, "term.labels")= chr "spray"
  .. ..- attr(*, "order")= int 1
  .. ..- attr(*, "intercept")= int 0
  .. ..- attr(*, "response")= int 1
  .. ..- attr(*, ".Environment")=<environment: R_GlobalEnv> 
  .. ..- attr(*, "predvars")= language list(count, spray)
  .. ..- attr(*, "dataClasses")= Named chr [1:2] "numeric" "factor"
  .. .. ..- attr(*, "names")= chr [1:2] "count" "spray"
 $ data             :'data.frame':	72 obs. of  2 variables:
  ..$ count: num [1:72] 10 7 20 14 14 12 10 23 17 20 ...
  ..$ spray: Factor w/ 6 levels "A","B","C","D",..: 1 1 1 1 1 1 1 1 1 1 ...
 $ offset           : NULL
 $ control          :List of 3
  ..$ epsilon: num 1e-08
  ..$ maxit  : num 25
  ..$ trace  : logi FALSE
 $ method           : chr "glm.fit"
 $ contrasts        :List of 1
  ..$ spray: chr "contr.treatment"
 $ xlevels          :List of 1
  ..$ spray: chr [1:6] "A" "B" "C" "D" ...
 - attr(*, "class")= chr [1:2] "glm" "lm"
}
\details{
%%  ~~ If necessary, more details than the __description__ above ~~
}
\source{
%%  ~~ reference to a publication or URL from which the data were obtained ~~
}
\references{
%%  ~~ possibly secondary sources and usages ~~
}
\examples{
data(fit)
## maybe str(fit) ; plot(fit) ...
}
\keyword{datasets}
