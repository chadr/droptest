trialdev <- function(sim.values) {
    # Calculates average distance from P for observed reaction percent ("Trial 
    #   Deviation").
    #
    # Args:
    #   sim.values: data.frame. Data frame produced by droptest::trials, 
    #     droptest::series, or droptest::groups.
    #
    # Returns:
    #   Data frame of mean total trials per test, and trial deviation (average
    #     distance from P). Aggregated by P levels.

    # check input var(s)
    if (!is.data.frame(sim.values)) {
        stop("Input should be a data frame.")
    }
    # make sure data frame is in correct format
    if (!all(c("PCT_REACT", "P") %in% names(sim.values))) {
        stop("Input data frame is malformed. See droptest::trials,
             droptest::series, or droptest::groups")
    }

    if ("GROUP" %in% sim.values) {
        td.output <- NULL
        # process each group
        for (group in unique(sim.values$GROUP)) {
            # current group
            c.group <- sim.values[sim.values$GROUP == group]
            # get squared distances
            c.group$TD <- c.group$P - c.group$PCT_REACT
            c.group$TD <- c.group$TD ^ 2
            c.group$AVG_TRIALS <- c.group$TRIALS     
            # aggregate by P and get means
            g.output <- aggregate(cbind(TD, AVG_TRIALS) ~ P, FUN = mean,
                                  data = sim.values)
            # get "trial deviation"
            g.output$TD <- sqrt(td.output$TD)
            # build output
            td.output <- rowbind(td.output, g.output)
        }

    } else {
        # only one probability level

        # get squared distances
        sim.values$TD <- sim.values$P - sim.values$PCT_REACT
        sim.values$TD <- sim.values$TD ^ 2
        sim.values$AVG_TRIALS <- sim.values$TRIALS     
        # aggregate by P and get means
        td.output <- aggregate(cbind(TD, AVG_TRIALS) ~ P, FUN = mean,
                               data = sim.values)
        # get "trial deviation"
        td.output$TD <- sqrt(td.output$TD)
    } 

    return(td.output)
}