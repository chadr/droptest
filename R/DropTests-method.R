DropTests <- function(num.tests, tag.group = FALSE, group = 0, ...) {
  # Completes multiple simulated drop tests. See DropTest.r for more info.
  #
  # Args:
  #   num.tests: Integer. Specifies how many drop tests to simulate.
  #   group: Integer. Assigns a group number to each simulated drop test.
  #     Default is 0.
  #   tag.group: If TRUE, group number is included in output data frame.
  #     Default is FALSE.
  #
  # Returns:
  #   Data frame of multiple simulated drop tests. Note: Only data frame is
  #     supported at this time.
  #
  # See https://google.github.io/styleguide/Rguide.xml for style info.

  tests <- NULL

  for (i in 1:num.tests) {
    # generate a batch of drop tests
    tests <- rbind(tests, DropTest(data.structure = "data.frame", ...))
  }

  if (tag.group == TRUE) {
    # assign group
    tests$GROUP <- group
  }

  return(tests)
}