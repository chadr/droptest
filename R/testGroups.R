testGroups <- function(num.groups, use.range = FALSE, range = NULL, ...) {
  # Generates groups of drop tests.
  #
  # Args:
  #   num.groups: Integer. Specifies how many groups of drop tests to produce.
  #   use.range: If TRUE, a range of probablities will be used. One for each
  #     group.
  #   range: Vector. Specifies range of probabilities to be used when use.range
  #     is TRUE. Note: Vector length must equal value of num.groups.
  #
  # Returns:
  #   Data frame of groups where each group consists of multiple drop tests.
  #   Note: Only data frame is supported at this time.
  #
  # See https://google.github.io/styleguide/Rguide.xml for style info.
  
  groups <- NULL

  for(i in 1:num.groups) {
    # fixed probability for each group
  	if (use.range == FALSE) {
      # generate several groups of drop tests
      groups <- rbind(groups, testSeries(tag.group = TRUE, group = i, ...))
    }
    # varying probability by group
    if (use.range == TRUE) {
      # generate several groups of drop tests
      groups <- rbind(groups, testSeries(tag.group = TRUE, p = range[[i]],
                      group = i, ...))
    }
  }
  
  return(groups)
}