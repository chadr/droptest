DropTest <- function(p, observations = 20, fail.criteria = 1, raw.data = FALSE,
                     data.structure = "data.frame") {
  # Completes a simulated drop test.
  #
  # Drop testing -- performed by the military and NASA -- yields results
  #   that are difficult to analyze. Numerous tech briefs and standards have
  #   attempted to address the problem. While fundamentally a binomial 
  #   process, drop testing almost always produces truncated data. Testing
  #   stops as soon as a failure condition is reached. If the failure condition
  #   occurs on drop one or two -- depending on the failure criteria -- then
  #   the test returns only one or two result values. Alternatively, if the
  #   failure condition occurs on the last observation, then the test returns
  #   as many result values as observations.
  #
  #   Drop testing is used to evaluate if a material will interact with liquid
  #   (LOX) or gaseous oxygen (GOX). The material is exposed to the LOX or GOX
  #   and an impactor is dropped onto the sample. Each drop is a bernoulli
  #   trial where a reaction is a failure and a non-reaction is a success. The
  #   specified number of trials -- until failure -- produces one test run.
  #
  #   For more information on drop testing: 
  # 
  # Args:
  #   p: Integer. Specifies the probability that a reaction occurs.
  #     A reaction is interpreted as a failure.
  #   fail.criteria: Integer. Specifies number of reactions that can occur
  #     before entire test is considered a failure. Default is 1.
  #   raw.data: If TRUE, output includes non-truncated binomial sample; If
  #     FALSE, it does not. Default is FALSE.
  #   data.structure: Default is data.frame.
  #   
  # Returns:
  #   Result of simulation as a data frame or list, depending on value of 
  #     data.structure.
  #
  # See https://google.github.io/styleguide/Rguide.xml for style info.

  # constant
  k <- 1
  
  # 1 = reaction; 0 = no reaction
  results <- rbinom(observations, k, p)
  
  # position of fail condition
  index <- which(results == 1)[fail.criteria]
  
  if (is.na(index)) {
    # no reactions
    reactions <- 0
    non.reactions <- observations
  } else {
    # count number of reactions and non-reactions
    reactions <- sum(results[0:index])
    non.reactions <- index    
  }
  
  if (data.structure == "data.frame") {
    # build data frame
    test.return <- data.frame(REACTIONS = reactions,
                              NON_REACTIONS = non.reactions,
                              TRIALS = reactions + non.reactions,
                              RESULT = ifelse(reactions >= fail.criteria,
                                              "FAIL", "PASS"),
                              P = p,
                              stringsAsFactors = FALSE)
   
    if (raw.data == TRUE) {
      # provide raw data if requested (not truncated)
      test.return$RAW_DATA <- I(list(results))
    }
  } 

  if (data.structure == "list") {
    #build list
    test.return <- list(REACTIONS = reactions,
                        NON_REACTIONS = non.reactions,
                        RESULT = ifelse(reactions >= fail.criteria,
                                        "FAIL", "PASS"),
                        P = p)
    
    if (raw.data == TRUE) {
      # provide raw data if requested (not truncated)
      test.return[["RAW_DATA"]] <- results
    }
  }

  return(test.return)
}