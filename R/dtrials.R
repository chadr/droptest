dtrials <- function(q, max.trials = 20, fail.criteria = 1,
                    data.structure = "data.table") {
  #' Completes a simulated drop test.
  #' 
  #' \code{dtrials} returns bernoulli trials that make up a drop test.
  #' 
  #' @param q Integer. Specifies the probability that a reaction occurs. 
  #'   A reaction is interpreted as a failure therfore q is the probability
  #'   of failure for the bernoulli trials. Probability of success would be
  #'   \eqn{p = 1 - q}. See \url{https://en.wikipedia.org/wiki/Bernoulli_trial}
  #' @param max.trials Integer. The maximum number of bernoulli trials to
  #'   perform. where each trial represents one drop of the impactor onto
  #'   a sample. The simulated test can only reach this number of trials if
  #'   no reactions (failures) occur. Default is 20.
  #' @param fail.criteria Integer. Specifies number of reactions (failures)
  #'   that can occur before an entire test is considered a failure. Default
  #'   is 1.
  #' @param data.structure Instructs function to return result as a data.table
  #'   or a list. Default is data.table. 
  #'   
  #'   \strong{Note:} Other functions in this package only work with
  #'   data.tables. List is an option strictly for future flexibility.
  #' @return Result of simulation as a data table or list -- depending on value
  #'   of \code{data.structure}. Where #@%#%#@^#@$^#@$^
  #' @examples 
  #'   dtrials(0.05)
  #'   dtrials(0.05, max.trials = 60)
  #'   dtrials(0.05, fail.criteria = 2)
  #'   dtrials(0.05, max.trials = 60, fail.criteria = 2)
  #'   dtrials(0.05, data.structure = "list")    
  #'   
  #' \dontrun{
  #' dtrials()
  #' }
  
  # validate function arguments
  if (!(data.structure == "data.table" || data.structure == "list")) {
    stop("only data.table or list are supported")
  }
  if (fail.criteria > max.trials) {
    stop("max.trials must be greater than or equal to fail.criteria")
  }
  
  # constant for binomial process
  k <- 1
    
  # 1 = reaction; 0 = no reaction
  results <- rbinom(max.trials, k, q)
    
  # position of fail condition
  index <- which(results == 1)[fail.criteria]
    
  if (is.na(index)) {
    # no reactions
    reactions <- 0
    non.reactions <- max.trials
  } else {
    # count number of reactions and non-reactions
    reactions <- sum(results[0:index])
    non.reactions <- index    

    if (index == 20) {
      non.reactions = non.reactions - 1
    }
  }
    
  if (data.structure == "data.table") {
    # build data frame
    test.return <- data.table(F_CRITERIA = fail.criteria,
                              REACT = reactions,
                              NON_REACT = non.reactions,
                              TRIALS = reactions + non.reactions,
                              MAX_TRIALS = max.trials,
                              PCT_REACT = reactions / (reactions + non.reactions),
                              Q = q,
                              P = 1 - q,
                              RESULT = ifelse(reactions >= fail.criteria,
                                              "FAIL", "PASS"),
                              stringsAsFactors = FALSE)
  } 

  if (data.structure == "list") {
    #build list
    test.return <- list(F_CRITERIA = fail.criteria,
                        REACT = reactions,
                        NON_REACT = non.reactions,
                        TRIALS = reactions + non.reactions,
                        MAX_TRIALS = max.trials,
                        PCT_REACT = reactions / (reactions + non.reactions),
                        Q = q,
                        P = 1 - q,
                        RESULT = ifelse(reactions >= fail.criteria,
                                       "FAIL", "PASS"))
  }

  return(test.return)
}