testTrials <- function(p, observations = 20, fail.criteria = 1,
                       raw.data = FALSE, data.structure = "data.frame") {
    # Completes a simulated drop test.
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

        if (index == 20) {
            non.reactions = non.reactions - 1
        }
    }
    
    if (data.structure == "data.frame") {
        # build data frame
        test.return <- data.frame(REACTIONS = reactions,
                                  NON_REACTIONS = non.reactions,
                                  TRIALS = reactions + non.reactions,
                                  PCT_REACTIONS = reactions / (reactions + non.reactions),
                                  P = p,
                                  RESULT = ifelse(reactions >= fail.criteria,
                                                "FAIL", "PASS"))
     
        if (raw.data == TRUE) {
          # provide raw data if requested (not truncated)
          test.return$RAW_DATA <- I(list(results))
        }
    } 

    if (data.structure == "list") {
        #build list
        test.return <- list(REACTIONS = reactions,
                            NON_REACTIONS = non.reactions,
                            TRIALS = reactions + non.reactions,
                            PCT_REACTIONS = reactions / (reactions + non.reactions),
                            P = p,
                            RESULT = ifelse(reactions >= fail.criteria,
                                            "FAIL", "PASS"))
      
        if (raw.data == TRUE) {
          # provide raw data if requested (not truncated)
          test.return[["RAW_DATA"]] <- results
        }
    }

    return(test.return)
}