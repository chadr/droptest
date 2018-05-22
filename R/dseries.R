dseries <- function(num.series, tag.group = FALSE, group = 0, ...) {
  #' Completes multiple series of simulated drop tests.
  #' 
  #' \code{dseries} returns a series of simulated drop tests. Where each test
  #' consists of trials (drops).
  #'
  #' @param num.series Integer. Specifies how many series of drop tests to
  #'   simulate.
  #' @param tag.group Logical. If \code{TRUE}, group number is included in
  #'   output data table. Default is \code{FALSE}.
  #' @param group Integer. Assigns a group number to each simulated drop test
  #'   within the same series. Only if \code{tag.group} is \code{TRUE}. Default
  #'   is 0.
  #' @param ... Passes \code{q}, \code{max.trials}, \code{fail.criteria},
  #'   and \code{fail.criteria} to \code{dtrials}. All are optional except
  #'   \code{q}.
  #'
  #' @examples
  #'   dseries(num.series = 5, q = 0.05)
  #'   dseries(num.series = 5, q = 0.05, max.trials = 60)
  #'   dseries(num.series = 5, q = 0.05, max.trials = 60, fail.criteria = 2)
  #'   
  #' @author Chad Ross \email{chad.ross@gmail.com}
  #'   
  #' @return Data table of multiple simulated drop tests. 
  #'   Each row of the data.table represents one simulated drop test. 
  #'   Containing the following elements:
  #'   
  #' \itemize{  
  #'  \item \strong{F_CRITERIA} The failure criteria specified.
  #'  
  #'  \item \strong{REACT} The total number of simulated reactions (failures).
  #'  
  #'  \item \strong{NON_REACT} The total number of simulated non-reactions
  #'   (successes).
  #'   
  #'  \item \strong{TRIALS} The number of simulated trials performed until
  #'   failure condition met.
  #'   
  #'  \item \strong{MAX_TRIALS} The maximum number of simulated trials to
  #'   perform as specified.
  #'   
  #'  \item \strong{PCT_REACT} The the percent of simulated trials that yielded
  #'   a reaction (failure).
  #'   
  #'  \item \strong{Q} The probability of failure (reaction) as specified.
  #'  
  #'  \item \strong{P} The probability of success (non-reaction).
  #'  
  #'  \item \strong{RESULT} Whether the simulated test series as a whole
  #'   failed or passed based on the failure criteria specified.
  #' }
  #' 
  #' @seealso
  #'   \code{\link{dtrials}}
  #'   \code{\link{droptest}}

  # vector to hold series
  my.list <- vector('list', num.series)
  
  # generate a batch of drop tests
  for (i in 1:num.series) {
    my.list[[i]] <- dtrials(data.structure = "data.table", ...)
  }
  
  results <- do.call("rbind", my.list)
  
  if (tag.group == TRUE) {
    # assign group
    results$GROUP <- group
  }
  
  return(results)
  }