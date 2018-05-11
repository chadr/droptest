\name{mdl2}
\alias{mdl2}
\docType{data}
\title{
%%   ~~ data name/kind ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("mdl2")}
\format{
  The format is:
List of 12
 $ coefficients : Named num [1:5] 62.101 -0.155 -0.98 0.125 1.078
  ..- attr(*, "names")= chr [1:5] "(Intercept)" "Agriculture" "Education" "Catholic" ...
 $ residuals    : Named num [1:47] 7.31 2.28 8.01 10.1 13.37 ...
  ..- attr(*, "names")= chr [1:47] "Courtelary" "Delemont" "Franches-Mnt" "Moutier" ...
 $ effects      : Named num [1:47] -480.9 29.9 -48.3 37.2 -20.2 ...
  ..- attr(*, "names")= chr [1:47] "(Intercept)" "Agriculture" "Education" "Catholic" ...
 $ rank         : int 5
 $ fitted.values: Named num [1:47] 72.9 80.8 84.5 75.7 63.5 ...
  ..- attr(*, "names")= chr [1:47] "Courtelary" "Delemont" "Franches-Mnt" "Moutier" ...
 $ assign       : int [1:5] 0 1 2 3 4
 $ qr           :List of 5
  ..$ qr   : num [1:47, 1:5] -6.856 0.146 0.146 0.146 0.146 ...
  .. ..- attr(*, "dimnames")=List of 2
  .. .. ..$ : chr [1:47] "Courtelary" "Delemont" "Franches-Mnt" "Moutier" ...
  .. .. ..$ : chr [1:5] "(Intercept)" "Agriculture" "Education" "Catholic" ...
  .. ..- attr(*, "assign")= int [1:5] 0 1 2 3 4
  ..$ qraux: num [1:5] 1.15 1.01 1.16 1.01 1.06
  ..$ pivot: int [1:5] 1 2 3 4 5
  ..$ tol  : num 1e-07
  ..$ rank : int 5
  ..- attr(*, "class")= chr "qr"
 $ df.residual  : int 42
 $ xlevels      : Named list()
 $ call         : language lm(formula = Fertility ~ Agriculture + Education + Catholic + Infant.Mortality, data = swiss)
 $ terms        :Classes 'terms', 'formula'  language Fertility ~ Agriculture + Education + Catholic + Infant.Mortality
  .. ..- attr(*, "variables")= language list(Fertility, Agriculture, Education, Catholic, Infant.Mortality)
  .. ..- attr(*, "factors")= int [1:5, 1:4] 0 1 0 0 0 0 0 1 0 0 ...
  .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. ..$ : chr [1:5] "Fertility" "Agriculture" "Education" "Catholic" ...
  .. .. .. ..$ : chr [1:4] "Agriculture" "Education" "Catholic" "Infant.Mortality"
  .. ..- attr(*, "term.labels")= chr [1:4] "Agriculture" "Education" "Catholic" "Infant.Mortality"
  .. ..- attr(*, "order")= int [1:4] 1 1 1 1
  .. ..- attr(*, "intercept")= int 1
  .. ..- attr(*, "response")= int 1
  .. ..- attr(*, ".Environment")=<environment: R_GlobalEnv> 
  .. ..- attr(*, "predvars")= language list(Fertility, Agriculture, Education, Catholic, Infant.Mortality)
  .. ..- attr(*, "dataClasses")= Named chr [1:5] "numeric" "numeric" "numeric" "numeric" ...
  .. .. ..- attr(*, "names")= chr [1:5] "Fertility" "Agriculture" "Education" "Catholic" ...
 $ model        :'data.frame':	47 obs. of  5 variables:
  ..$ Fertility       : num [1:47] 80.2 83.1 92.5 85.8 76.9 76.1 83.8 92.4 82.4 82.9 ...
  ..$ Agriculture     : num [1:47] 17 45.1 39.7 36.5 43.5 35.3 70.2 67.8 53.3 45.2 ...
  ..$ Education       : int [1:47] 12 9 5 7 15 7 7 8 7 13 ...
  ..$ Catholic        : num [1:47] 9.96 84.84 93.4 33.77 5.16 ...
  ..$ Infant.Mortality: num [1:47] 22.2 22.2 20.2 20.3 20.6 26.6 23.6 24.9 21 24.4 ...
  ..- attr(*, "terms")=Classes 'terms', 'formula'  language Fertility ~ Agriculture + Education + Catholic + Infant.Mortality
  .. .. ..- attr(*, "variables")= language list(Fertility, Agriculture, Education, Catholic, Infant.Mortality)
  .. .. ..- attr(*, "factors")= int [1:5, 1:4] 0 1 0 0 0 0 0 1 0 0 ...
  .. .. .. ..- attr(*, "dimnames")=List of 2
  .. .. .. .. ..$ : chr [1:5] "Fertility" "Agriculture" "Education" "Catholic" ...
  .. .. .. .. ..$ : chr [1:4] "Agriculture" "Education" "Catholic" "Infant.Mortality"
  .. .. ..- attr(*, "term.labels")= chr [1:4] "Agriculture" "Education" "Catholic" "Infant.Mortality"
  .. .. ..- attr(*, "order")= int [1:4] 1 1 1 1
  .. .. ..- attr(*, "intercept")= int 1
  .. .. ..- attr(*, "response")= int 1
  .. .. ..- attr(*, ".Environment")=<environment: R_GlobalEnv> 
  .. .. ..- attr(*, "predvars")= language list(Fertility, Agriculture, Education, Catholic, Infant.Mortality)
  .. .. ..- attr(*, "dataClasses")= Named chr [1:5] "numeric" "numeric" "numeric" "numeric" ...
  .. .. .. ..- attr(*, "names")= chr [1:5] "Fertility" "Agriculture" "Education" "Catholic" ...
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
data(mdl2)
## maybe str(mdl2) ; plot(mdl2) ...
}
\keyword{datasets}
