groups <- function(num.groups = NULL, multi.p = FALSE, probs = NULL, ...) {
    # Generates groups of simulated test series.
    #
    # Args:
    #   num.groups: Integer. Specifies how many groups of drop tests to produce.
    #   multi.p: If TRUE, values of probablities will be used. One for each
    #     group.
    #   probs: Vector. Specifies values of probabilities to be used when 
    #     multi.p is TRUE. Note: Vector length must equal value of num.groups.
    #
    # Returns:
    #   Data frame of groups where each group consists of multiple drop tests.
    #   Note: Only data frame is supported at this time.
    require("dplyr")

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
      	if (multi.p == FALSE) {
            # generate several groups of drop tests
            groups <- bind_rows(groups, series(tag.group = TRUE, group = i,
                            ...))
        }
        # varying probability by group
        if (multi.p == TRUE) {
            # generate several groups of drop tests
            groups <- bind_rows(groups, series(tag.group = TRUE, p = probs[[i]],
                            group = i, ...))
        }
    }
    
    return(groups)
}