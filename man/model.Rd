\name{model}
\alias{model}
\docType{data}
\title{
%%   ~~ data name/kind ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("model")}
\format{
  The format is:
List of 12
 $ coefficients : Named num [1:4] 9.62 -3.92 1.23 2.94
  ..- attr(*, "names")= chr [1:4] "(Intercept)" "wt" "qsec" "am"
 $ residuals    : Named num [1:32] -1.47 -1.158 -3.481 0.543 1.69 ...
  ..- attr(*, "names")= chr [1:32] "Mazda RX4" "Mazda RX4 Wag" "Datsun 710" "Hornet 4 Drive" ...
 $ effects      : Named num [1:32] -113.65 -29.12 -9.1 5.12 1.44 ...
  ..- attr(*, "names")= chr [1:32] "(Intercept)" "wt" "qsec" "am" ...
 $ rank         : int 4
 $ fitted.values: Named num [1:32] 22.5 22.2 26.3 20.9 17 ...
  ..- attr(*, "names")= chr [1:32] "Mazda RX4" "Mazda RX4 Wag" "Datsun 710" "Hornet 4 Drive" ...
 $ assign       : int [1:4] 0 1 2 3
 $ qr           :List of 5
  ..$ qr   : num [1:32, 1:4] -5.657 0.177 0.177 0.177 0.177 ...
  .. ..- attr(*, "dimnames")=List of 2
  .. .. ..$ : chr [1:32] "Mazda RX4" "Mazda RX4 Wag" "Datsun 710" "Hornet 4 Drive" ...
  .. .. ..$ : chr [1:4] "(Intercept)" "wt" "qsec" "am"
  .. ..- attr(*, "assign")= int [1:4] 0 1 2 3
  ..$ qraux: num [1:4] 1.18 1.05 1.08 1.18
  ..$ pivot: int [1:4] 1 2 3 4
  ..$ tol  : num 1e-07
  ..$ rank : int 4
  ..- attr(*, "class")= chr "qr"
 $ df.residual  : int 28
 $ xlevels      : Named list()
 $ call         : language lm(formula = mpg ~ wt + qsec + am, data = cars)
 $ terms        :Classes 'terms', 'formula'  language mpg ~ wt + qsec + am
  .. ..- attr(*, "variables")= language list(mpg, wt, qsec, am)
  .. ..- attr(*, "factors")= int [1:4, 1:3] 0 1 0 0 0 0 1 0 0 0 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:4] "mpg" "wt" "qsec" "am"
  .. .. .. ..$ : chr [1:3] "wt" "qsec" "am"
  .. ..- attr(*, "term.labels")= chr [1:3] "wt" "qsec" "am"
  .. ..- attr(*, "order")= int [1:3] 1 1 1
  .. ..- attr(*, "intercept")= int 1
  .. ..- attr(*, "response")= int 1
  .. ..- attr(*, ".Environment")=<environment: R_GlobalEnv> 
  .. ..- attr(*, "predvars")= language list(mpg, wt, qsec, am)
  .. ..- attr(*, "dataClasses")= Named chr [1:4] "numeric" "numeric" "numeric" "numeric"
  .. .. ..- attr(*, "names")= chr [1:4] "mpg" "wt" "qsec" "am"
 $ model        :'data.frame':	32 obs. of  4 variables:
  ..$ mpg : num [1:32] 21 21 22.8 21.4 18.7 18.1 14.3 24.4 22.8 19.2 ...
  ..$ wt  : num [1:32] 2.62 2.88 2.32 3.21 3.44 ...
  ..$ qsec: num [1:32] 16.5 17 18.6 19.4 17 ...
  ..$ am  : num [1:32] 1 1 1 0 0 0 0 0 0 0 ...
  ..- attr(*, "terms")=Classes 'terms', 'formula'  language mpg ~ wt + qsec + am
  .. .. ..- attr(*, "variables")= language list(mpg, wt, qsec, am)
  .. .. ..- attr(*, "factors")= int [1:4, 1:3] 0 1 0 0 0 0 1 0 0 0 ...
  .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. ..$ : chr [1:4] "mpg" "wt" "qsec" "am"
  .. .. .. .. ..$ : chr [1:3] "wt" "qsec" "am"
  .. .. ..- attr(*, "term.labels")= chr [1:3] "wt" "qsec" "am"
  .. .. ..- attr(*, "order")= int [1:3] 1 1 1
  .. .. ..- attr(*, "intercept")= int 1
  .. .. ..- attr(*, "response")= int 1
  .. .. ..- attr(*, ".Environment")=<environment: R_GlobalEnv> 
  .. .. ..- attr(*, "predvars")= language list(mpg, wt, qsec, am)
  .. .. ..- attr(*, "dataClasses")= Named chr [1:4] "numeric" "numeric" "numeric" "numeric"
  .. .. .. ..- attr(*, "names")= chr [1:4] "mpg" "wt" "qsec" "am"
 - attr(*, "class")= chr "lm"
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
data(model)
## maybe str(model) ; plot(model) ...
}
\keyword{datasets}
