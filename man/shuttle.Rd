\name{shuttle}
\alias{shuttle}
\docType{data}
\title{
%%   ~~ data name/kind ... ~~
}
\description{
%%  ~~ A concise (1-5 lines) description of the dataset. ~~
}
\usage{data("shuttle")}
\format{
  A data frame with 256 observations on the following 8 variables.
  \describe{
    \item{\code{stability}}{a factor with levels \code{stab} \code{xstab}}
    \item{\code{error}}{a factor with levels \code{LX} \code{MM} \code{SS} \code{XL}}
    \item{\code{sign}}{a factor with levels \code{nn} \code{pp}}
    \item{\code{wind}}{a factor with levels \code{head} \code{tail}}
    \item{\code{magn}}{a factor with levels \code{Light} \code{Medium} \code{Out} \code{Strong}}
    \item{\code{vis}}{a factor with levels \code{no} \code{yes}}
    \item{\code{use}}{a factor with levels \code{auto} \code{noauto}}
    \item{\code{use.binary}}{a numeric vector}
  }
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
data(shuttle)
## maybe str(shuttle) ; plot(shuttle) ...
}
\keyword{datasets}
