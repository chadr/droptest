dseries <- function(num.series, tag.group = FALSE, group = 0, ...) {
    # Completes multiple series of dsimulated rop tests. See testTrials.R
    # for more information.
    #
    # Args:
    #   num.series: Integer. Specifies how many drop tests to simulate.
    #   group: Integer. Assigns a group number to each simulated drop test.
    #     Default is 0.
    #   tag.group: If TRUE, group number is included in output data table.
    #     Default is FALSE.
    #
    # Returns:
    #   Data table of multiple simulated drop tests. Note: Only data frame is
    #     supported at this time.
    require(data.table)
    
    tests <- NULL

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