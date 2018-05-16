dseries <- function(num.series, tag.group = FALSE, group = 0, ...) {
  #' Completes multiple series of simulated drop tests.
  #' 
  #' \code{dseries} returns a series of simulated drops tests. Where each test
  #' consists of independent drops (trials).
  #'
  #' @param num.series Integer. Specifies how many series of drop tests to
  #'   simulate.
  #' @param tag.group Logical. If \code{TRUE}, group number is included in
  #' output data table. Default is \code{FALSE}.
  #' @param group: Integer. Assigns a group number to each simulated drop test
  #'   within the same series. Only if \code{tag.group} is \code{TRUE}. Default
  #'   is 0.
  #' @param ... Passes \code{q}, \code{max.trials}, \code{fail.criteria},
  #'   and \code{fail.criteria} to \code{dtrials}. All are optional except
  #'   \code{q}.
  #'
  #' @return Data table of multiple simulated drop tests. 
  #'   Where each row of the data.table represents one simulated drop test. 
  #'   \strong{Note:} Only data table is supported at this time.
  #'   
  #' @seealso \code{\link{dtrials}}

  # NULL object to prepare for rbinds
  tests <- NULL

  # generate data.table
  for (i in 1:num.series) {
    # generate a batch of drop tests
    tests <- rbind(tests, dtrials(data.structure = "data.table", ...))
  }

  if (tag.group == TRUE) {
    # assign group
    tests$GROUP <- group
  }

  return(tests)
}