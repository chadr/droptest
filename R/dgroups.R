dgroups <- function(num.groups = NULL, probs = NULL, ...) {
    # Generates groups of simulated test series.
    #
    # Args:
    #   num.groups: Integer. Specifies how many groups of drop tests to produce.
    #   probs: Vector. Specifies values of probabilities to be used when 
    #     multi.p is TRUE. Note: Vector length must equal value of num.groups.
    #
    # Returns:
    #   Data table of groups where each group consists of multiple drop tests.
    #   Note: Only data table is supported at this time.
    require("data.table")

    if (length(probs) > 0) {
        num.groups <- length(probs)
    }

    if (length(probs) == 0 && num.groups == NULL) {
        stop("If multiple values for P aren't used then number of groups
             must be specified.")
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
            groups <- rbind(groups, dseries(tag.group = TRUE, p = probs[[i]],
                            group = i, ...))
        }
    }
    
    return(groups)
}