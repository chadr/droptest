td <- function(sim.values) {
    # Generates groups of simulated test series.
    #
    # Args:
    #   num.groups: Integer. Specifies how many groups of drop tests to produce.
    #   use.range: If TRUE, a range of probablities will be used. One for each
    #     group.
    #   range: Vector. Specifies range of probabilities to be used when 
    #     use.range is TRUE. Note: Vector length must equal value of num.groups.
    #
    # Returns:
    #   Data frame of groups where each group consists of multiple drop tests.
    #   Note: Only data frame is supported at this time.

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

        # mean of observed reaction probability
        #avg.p = mean(simulated.values$PCT_REACTIONS)
        #sim.values$AVG_PCT <- mean(sim.values$PCT_REACTIONS)
        sim.values$TD <- sim.values$P - sim.values$PCT_REACT
        sim.values$TD <- sim.values$TD ^ 2
        sim.values$AVG_TRIALS <- sim.values$TRIALS
        
        td.output <- aggregate(cbind(AVG_TRIALS, TD) ~ P, FUN = mean,
                               data = sim.values)
        td.output$TD <- sqrt(td.output$TD)
    } 

    return(td.output)
}