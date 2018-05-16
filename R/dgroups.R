dgroups <- function(num.groups = NULL, probs = NULL, ...) {
  #' Generates groups of simulated test series.
  #'
  #' \code{dgroups} returns a collection of multiple simulated test series. Each
  #' batch of test series are identified with a group number. Within each group
  #' test parameters will be identical.
  #'  
  #' @param num.groups Integer. Specifies how many groups of drop tests to
  #'   produce.
  #' @param probs Vector. Specifies values of probabilities to be used when 
  #'   multi.p is TRUE. Note: Vector length must equal value of num.groups.
  #' @param ... Passes values to \code{dseries}.
  #' 
  #' @examples
  #'   dgroups(num.groups = 2, probs = c(0.01, 0.2), num.series = 5)
  #'   dgroups(num.groups = 2, probs = c(0.01, 0.2), num.series = 5, max.trials = 60)
  #'   dgroups(num.groups = 2, probs = c(0.01, 0.2), num.series = 5, fail.criteria = 2)
  #'   dgroups(num.groups = 5, probs = seq(0.01, 0.05, by = 0.01), num.series = 2) 
  #' 
  #' @return Data table of groups where each group consists of multiple drop
  #'   tests. \strong{Note:} Only data table is supported at this time.
  #'   
  #'  Where:
  #'  \strong{F_CRITERIA} is the failure criteria specified (default is 1).
  #'  \strong{REACT} is the total number of simulated reactions (failures).
  #'  \strong{NON_REACT} is the total number of simulated non-reactions
  #'   (successes).
  #'  \strong{TRIALS} is the number of simulated trials performed until failure 
  #'   condition met. If the failure condition was not met then this value
  #'   will always be equal to \strong{MAX_TRIALS}.
  #'  \strong{MAX_TRIALS} is the maximum number of simulated trials to perform
  #'   as specified (default is 20). \strong{TRIALS} will always be less than
  #'   or equal to \strong{MAX_TRIALS}.
  #'  \strong{PCT_REACT} is the percent of simulated trials that yielded a
  #'   reaction (failure).
  #'  \strong{Q} is the probability of failure (reaction) as specified.
  #'  \strong{P} is the probability of success (non-reaction).
  #'  \strong{RESULT} is whether the simulated test series as a whole failed
  #'   or passed based on the failure criteria specified.
  #'  \strong{GROUP} denotes the group of simulated test series.
  #'   
  #' @seealso 
  #'   \code{\link{dseries}}
  #'   \code{\link{dtrials}}

  if (length(probs) > 0) {
    num.groups <- length(probs)
  }

  if (length(probs) == 0 && is.null(num.groups)) {
    stop("If multiple values for Q aren't used then number of groups
         must be specified.")
  }
  
  if (is.null(probs)) {
    stop("Must specify probabilities for groups.")
  }

  groups <- data.table(NULL)

  for(i in 1:num.groups) {
    # fixed probability for each group
    if (length(probs) == 1) {
      # generate several groups of drop tests
      groups <- rbind(groups, dseries(tag.group = TRUE, group = i,
                      ...))
    }
    # varying probability by group
    if (length(probs) > 1) {
      # generate several groups of drop tests
      groups <- rbind(groups, dseries(tag.group = TRUE, q = probs[[i]],
                      group = i, ...))
      }
  }
    
  return(groups)
}