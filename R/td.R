td <- function(sim.values) {
    # Calculates average distance from P for observed reaction percent.
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
        # process each group


    } else {
        # only one probability level

        # get squared distances
        sim.values$TD <- sim.values$P - sim.values$PCT_REACT
        sim.values$TD <- sim.values$TD ^ 2

        sim.values$AVG_TRIALS <- sim.values$TRIALS
        
        # aggregate by P and get means
        td.output <- aggregate(cbind(AVG_TRIALS, TD) ~ P, FUN = mean,
                               data = sim.values)
        # variance to sd
        td.output$TD <- sqrt(td.output$TD)
    } 

    return(td.output)
}