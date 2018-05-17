trialdev <- function(sim.values) {
  #' Calculates the "Trial Deviation".
  #'
  #' \code{trialdev} Creates data.table with the average distances from q for
  #' the total percent of reactions (failures).  
  #'   
  #' @param sim.values data.table. Data table produced by droptest::trials, 
  #'   droptest::series, or droptest::groups.
  #'
  #' @examples
  #'   trialdev(dtrials(q = 0.05, max.trials = 60, fail.criteria = 2))
  #' 
  #' @return Data table of q, p, trial deviation, and average total trials per
  #'   test (average distance from q). Aggregated by q levels.
  #'   
  #'  Where:
  #'  \strong{Q} is the probability of failure (reaction) as specified.
  #'  \strong{P} is the probability of success (non-reaction).
  #'  \strong{TRIAL_DEV} is the average distance from q for the total percent of
  #'   reactions (failures).
  #'  \strong{AVG_TRIALS} is the average number of simulated trials for each
  #'   level of q.
  #'
  #' @seealso 
  #'  \code{\link{dtrials}}
  #'  \code{\link{dseries}}
  #'  \code{\link{dgroups}}

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