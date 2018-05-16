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

    groups <- NULL

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