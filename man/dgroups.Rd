% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/dgroups.R
\name{dgroups}
\alias{dgroups}
\title{Generates groups of simulated test series.}
\usage{
dgroups(num.groups = NULL, probs = NULL, ...)
}
\arguments{
\item{num.groups}{Integer. Specifies how many groups of drop tests to
simulate.}

\item{probs}{Vector. Specifies probabilities of q used for each group.
Vector length must equal value of num.groups.}

\item{...}{Passes values to \code{dseries}.}
}
\value{
Data table of groups where each group consists of multiple drop
  tests. Consisting of the following elements:

\itemize{  
 \item \strong{F_CRITERIA} The failure criteria specified.
 
 \item \strong{REACT} The total number of simulated reactions (failures).
 
 \item \strong{NON_REACT} The total number of simulated non-reactions
  (successes).
  
 \item \strong{TRIALS} The number of simulated trials performed until
 failure condition met.
  
 \item \strong{MAX_TRIALS} The maximum number of simulated trials to
  perform as specified.
  
 \item \strong{PCT_REACT} The percent of simulated trials that yielded a
  reaction (failure).
  
 \item \strong{Q} The probability of failure (reaction) as specified.
 
 \item \strong{P} The probability of success (non-reaction).
 
 \item \strong{RESULT} Whether the simulated test series as a whole failed
  or passed based on the failure criteria specified.
  
 \item \strong{GROUP} Denotes the group of simulated test series.
}
}
\description{
\code{dgroups} returns a collection test series organized into groups. Each
batch of test series are identified with a group number. Test parameters
will be uniform within each group.
}
\examples{
  dgroups(num.groups = 2, probs = c(0.01, 0.2), num.series = 5)
  dgroups(num.groups = 2, probs = c(0.01, 0.2), num.series = 5, max.trials = 60)
  dgroups(num.groups = 2, probs = c(0.01, 0.2), num.series = 5, fail.criteria = 2)
  dgroups(num.groups = 5, probs = seq(0.01, 0.05, by = 0.01), num.series = 2) 

}
\seealso{
\code{\link{dseries}}
  \code{\link{dtrials}}
  \code{\link{droptest}}
}
\author{
Chad Ross \email{chad.ross@gmail.com}
}
