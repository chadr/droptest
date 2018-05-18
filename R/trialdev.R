trialdev <- function(sim.values) {
  #' Calculates trial deviation for simulated trials (drops).
  #'
  #' \code{trialdev} creates a data.table with the average distances from q for
  #' the total percent of reactions (failures).  
  #'   
  #' @param sim.values Data table. Data table produced by droptest::dtrials, 
  #'   droptest::dseries, or droptest::dgroups.
  #'
  #' @examples
  #'   trialdev(dtrials(q = 0.05, max.trials = 60, fail.criteria = 2))
  #' 
  #' @return Data table of q, p, trial deviation, and average total trials per
  #'   test. Aggregated by q.
  #'   
  #' \itemize{
  #'  \item \strong{Q} The probability of failure (reaction) as specified.
  #'  
  #'  \item \strong{P} The probability of success (non-reaction).
  #'  
  #'  \item \strong{TRIAL_DEV} The average distance from q for the total percent
  #'   of reactions (failures).
  #'   
  #'  \item \strong{AVG_TRIALS} The average number of simulated trials reached
  #'   for each q.
  #' }
  #' 
  #' @author Chad Ross \email{chad.ross@gmail.com}
  #'
  #' @seealso 
  #'  \code{\link{dtrials}}
  #'  \code{\link{dseries}}
  #'  \code{\link{dgroups}}
  #'  \code{\link{droptest}}

  # check input var(s)
  if (!is.data.table(sim.values)) {
    stop("Input should be a data table.")
  }
  
  # make sure data table is in correct format
  if (!all(c("PCT_REACT", "P") %in% names(sim.values))) {
    stop("Input data table is malformed. See droptest::trials,
         droptest::series, or droptest::groups")
  }

  td.output <- data.table(NULL)
  c.groups <- data.table(NULL)
  g.output <- data.table(NULL)
  
  if ("GROUP" %in% colnames(sim.values)) {
    # process each group
    for (group in unique(sim.values$GROUP)) {
      # current group
      c.group <- sim.values[sim.values$GROUP == group]
      # get squared distances
      c.group$TRIAL_DEV <- c.group$Q - c.group$PCT_REACT
      c.group$TRIAL_DEV <- c.group$TRIAL_DEV ^ 2
      # can't have a fractional trial
      c.group$AVG_TRIALS <- c.group$TRIALS     
      # aggregate by Q and get means
      g.output <- aggregate(cbind(P, TRIAL_DEV, AVG_TRIALS) ~ Q, FUN = mean,
                            data = c.group)
      g.output$AVG_TRIALS <- floor(g.output$AVG_TRIALS)
      # get "trial deviation"
      g.output$TRIAL_DEV <- sqrt(g.output$TRIAL_DEV)
      # build output
      td.output <- rbind(td.output, g.output)
    }
  } else {
    # only one probability level
    # get squared distances
    sim.values$TRIAL_DEV <- sim.values$Q - sim.values$PCT_REACT
    sim.values$TRIAL_DEV <- sim.values$TRIAL_DEV ^ 2
    sim.values$AVG_TRIALS <- sim.values$TRIALS     
    # aggregate by P and get means
    td.output <- aggregate(cbind(P, TRIAL_DEV, AVG_TRIALS) ~ Q, FUN = mean,
                           data = sim.values)
    # get "trial deviation"
    td.output$TRIAL_DEV <- sqrt(td.output$TRIAL_DEV)
    td.output$AVG_TRIALS <- floor(td.output$AVG_TRIALS)
  }
    
  return(td.output)
}