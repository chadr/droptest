\name{fit1}
\alias{fit1}
\docType{data}
\title{
%%   ~~ data name/kind ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("fit1")}
\format{
  The format is:
List of 30
 $ coefficients     : Named num [1:5] 3.64e-01 3.96e-01 -5.37e-16 -3.80e-01 -6.44e-02
  ..- attr(*, "names")= chr [1:5] "windhead" "windtail" "magnMedium" "magnOut" ...
 $ residuals        : Named num [1:256] 1.7 1.7 1.74 1.67 1.67 ...
  ..- attr(*, "names")= chr [1:256] "1" "2" "3" "4" ...
 $ fitted.values    : Named num [1:256] 0.59 0.59 0.574 0.598 0.598 ...
  ..- attr(*, "names")= chr [1:256] "1" "2" "3" "4" ...
 $ effects          : Named num [1:256] -1.401 -1.575 0.513 -1.128 0.179 ...
  ..- attr(*, "names")= chr [1:256] "windhead" "windtail" "magnMedium" "magnOut" ...
 $ R                : num [1:5, 1:5] -5.6 0 0 0 0 ...
  ..- attr(*, "dimnames")=List of 2
  .. ..$ : chr [1:5] "windhead" "windtail" "magnMedium" "magnOut" ...
  .. ..$ : chr [1:5] "windhead" "windtail" "magnMedium" "magnOut" ...
 $ rank             : int 5
 $ qr               :List of 5
  ..$ qr   : num [1:256, 1:5] -5.5952 0.0879 0.0884 0 0 ...
  .. ..- attr(*, "dimnames")=List of 2
  .. .. ..$ : chr [1:256] "1" "2" "3" "4" ...
  .. .. ..$ : chr [1:5] "windhead" "windtail" "magnMedium" "magnOut" ...
  ..$ rank : int 5
  ..$ qraux: num [1:5] 1.09 1 1.03 1.05 1.01
  ..$ pivot: int [1:5] 1 2 3 4 5
  ..$ tol  : num 1e-11
  ..- attr(*, "class")= chr "qr"
 $ family           :List of 12
  ..$ family    : chr "binomial"
  ..$ link      : chr "logit"
  ..$ linkfun   :function (mu)  
  ..$ linkinv   :function (eta)  
  ..$ variance  :function (mu)  
  ..$ dev.resids:function (y, mu, wt)  
  ..$ aic       :function (y, n, mu, wt, dev)  
  ..$ mu.eta    :function (eta)  
  ..$ initialize:  expression({  if (NCOL(y) == 1) {  if (is.factor(y))  y <- y != levels(y)[1L]  n <- rep.int(1, nobs)  y[weights =| __truncated__
  ..$ validmu   :function (mu)  
  ..$ valideta  :function (eta)  
  ..$ simulate  :function (object, nsim)  
  ..- attr(*, "class")= chr "family"
 $ linear.predictors: Named num [1:256] 0.364 0.364 0.299 0.396 0.396 ...
  ..- attr(*, "names")= chr [1:256] "1" "2" "3" "4" ...
 $ deviance         : num 349
 $ aic              : num 359
 $ null.deviance    : num 355
 $ iter             : int 4
 $ weights          : Named num [1:256] 0.242 0.242 0.244 0.24 0.24 ...
  ..- attr(*, "names")= chr [1:256] "1" "2" "3" "4" ...
 $ prior.weights    : Named num [1:256] 1 1 1 1 1 1 1 1 1 1 ...
  ..- attr(*, "names")= chr [1:256] "1" "2" "3" "4" ...
 $ df.residual      : int 251
 $ df.null          : int 256
 $ y                : Named num [1:256] 1 1 1 1 1 1 1 1 1 1 ...
  ..- attr(*, "names")= chr [1:256] "1" "2" "3" "4" ...
 $ converged        : logi TRUE
 $ boundary         : logi FALSE
 $ model            :'data.frame':	256 obs. of  3 variables:
  ..$ use.binary: int [1:256] 1 1 1 1 1 1 1 1 1 1 ...
  ..$ wind      : Factor w/ 2 levels "head","tail": 1 1 1 2 2 2 1 1 1 2 ...
  ..$ magn      : Factor w/ 4 levels "Light","Medium",..: 1 2 4 1 2 4 1 2 4 1 ...
  ..- attr(*, "terms")=Classes 'terms', 'formula'  language use.binary ~ wind + magn - 1
  .. .. ..- attr(*, "variables")= language list(use.binary, wind, magn)
  .. .. ..- attr(*, "factors")= int [1:3, 1:2] 0 1 0 0 0 1
  .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. ..$ : chr [1:3] "use.binary" "wind" "magn"
  .. .. .. .. ..$ : chr [1:2] "wind" "magn"
  .. .. ..- attr(*, "term.labels")= chr [1:2] "wind" "magn"
  .. .. ..- attr(*, "order")= int [1:2] 1 1
  .. .. ..- attr(*, "intercept")= int 0
  .. .. ..- attr(*, "response")= int 1
  .. .. ..- attr(*, ".Environment")=<environment: R_GlobalEnv> 
  .. .. ..- attr(*, "predvars")= language list(use.binary, wind, magn)
  .. .. ..- attr(*, "dataClasses")= Named chr [1:3] "numeric" "factor" "factor"
  .. .. .. ..- attr(*, "names")= chr [1:3] "use.binary" "wind" "magn"
 $ call             : language glm(formula = use.binary ~ wind + magn - 1, family = binomial, data = shuttle)
 $ formula          :Class 'formula'  language use.binary ~ wind + magn - 1
  .. ..- attr(*, ".Environment")=<environment: R_GlobalEnv> 
 $ terms            :Classes 'terms', 'formula'  language use.binary ~ wind + magn - 1
  .. ..- attr(*, "variables")= language list(use.binary, wind, magn)
  .. ..- attr(*, "factors")= int [1:3, 1:2] 0 1 0 0 0 1
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:3] "use.binary" "wind" "magn"
  .. .. .. ..$ : chr [1:2] "wind" "magn"
  .. ..- attr(*, "term.labels")= chr [1:2] "wind" "magn"
  .. ..- attr(*, "order")= int [1:2] 1 1
  .. ..- attr(*, "intercept")= int 0
  .. ..- attr(*, "response")= int 1
  .. ..- attr(*, ".Environment")=<environment: R_GlobalEnv> 
  .. ..- attr(*, "predvars")= language list(use.binary, wind, magn)
  .. ..- attr(*, "dataClasses")= Named chr [1:3] "numeric" "factor" "factor"
  .. .. ..- attr(*, "names")= chr [1:3] "use.binary" "wind" "magn"
 $ data             :'data.frame':	256 obs. of  8 variables:
  ..$ stability : Factor w/ 2 levels "stab","xstab": 2 2 2 2 2 2 2 2 2 2 ...
  ..$ error     : Factor w/ 4 levels "LX","MM","SS",..: 1 1 1 1 1 1 1 1 1 1 ...
  ..$ sign      : Factor w/ 2 levels "nn","pp": 2 2 2 2 2 2 1 1 1 1 ...
  ..$ wind      : Factor w/ 2 levels "head","tail": 1 1 1 2 2 2 1 1 1 2 ...
  ..$ magn      : Factor w/ 4 levels "Light","Medium",..: 1 2 4 1 2 4 1 2 4 1 ...
  ..$ vis       : Factor w/ 2 levels "no","yes": 1 1 1 1 1 1 1 1 1 1 ...
  ..$ use       : Factor w/ 2 levels "auto","noauto": 1 1 1 1 1 1 1 1 1 1 ...
  ..$ use.binary: int [1:256] 1 1 1 1 1 1 1 1 1 1 ...
 $ offset           : NULL
 $ control          :List of 3
  ..$ epsilon: num 1e-08
  ..$ maxit  : num 25
  ..$ trace  : logi FALSE
 $ method           : chr "glm.fit"
 $ contrasts        :List of 2
  ..$ wind: chr "contr.treatment"
  ..$ magn: chr "contr.treatment"
 $ xlevels          :List of 2
  ..$ wind: chr [1:2] "head" "tail"
  ..$ magn: chr [1:4] "Light" "Medium" "Out" "Strong"
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
data(fit1)
## maybe str(fit1) ; plot(fit1) ...
}
\keyword{datasets}
