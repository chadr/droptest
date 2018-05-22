dtrials <- function(q, max.trials = 20, fail.criteria = 1,
                    data.structure = "data.table") {
  #' Completes a simulated drop test.
  #' 
  #' \code{dtrials} returns simulated bernoulli trials (drops) that compose one
  #'   drop test.
  #' 
  #' @param q Integer. Specifies the probability that a reaction occurs. 
  #'   A reaction is interpreted as a failure therfore q is the probability
  #'   of failure for the bernoulli trials. Probability of success would be
  #'   \eqn{p = 1 - q}. See \url{https://en.wikipedia.org/wiki/Bernoulli_trial}
  #' @param max.trials Integer. The maximum number of bernoulli trials to
  #'   perform. where each trial represents one drop of the impactor onto
  #'   a sample. Trials performed will always be less than or equal to
  #'   \code{max.trials}. Default is 20.
  #' @param fail.criteria Integer. Specifies number of reactions (failures)
  #'   that can occur before an entire test is considered a failure. Must be 
  #'   less than or equal to \code{max.trials}. Default is 1.
  #' @param data.structure Instructs function to return result as a data.table
  #'   or list. Default is data.table. 
  #'   
  #'   \strong{Note:} Other functions in this package only work with
  #'   data.tables. List is an option strictly for future flexibility.
  #' 
  #' @return A data table or list. Containing the following elements:
  #'   
  #'  \itemize{ 
  #'   \item \strong{F_CRITERIA} The failure criteria specified.
  #'    
  #'   \item \strong{REACT} The total number of simulated reactions
  #'    (failures).
  #'    
  #'   \item \strong{NON_REACT} The total number of simulated non-reactions
  #'     (successes).
  #'     
  #'   \item \strong{TRIALS} The number of simulated trials performed until
  #'     the failure condition was met.
  #'          
  #'    \item \strong{MAX_TRIALS} The maximum number of simulated trials
  #'     specified.
  #'     
  #'    \item \strong{PCT_REACT} The percent of simulated trials that yielded
  #'     a reaction (failure).
  #'     
  #'    \item \strong{Q} The probability of failure (reaction) as specified.
  #'    
  #'    \item \strong{P} The probability of success (non-reaction).
  #'    
  #'    \item \strong{RESULT} Whether the simulated test series as a whole
  #'    failed or passed based on the failure criteria specified.
  #'  }
  #'  
  #' @author Chad Ross \email{chad.ross@gmail.com}
  #'  
  #' @examples 
  #'   dtrials(0.05)
  #'   dtrials(0.05, max.trials = 60)
  #'   dtrials(0.05, fail.criteria = 2)
  #'   dtrials(0.05, max.trials = 60, fail.criteria = 2)
  #'   dtrials(0.05, data.structure = "list")
  #'   
  #' @seealso \code{\link{droptest}}
  
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
                                              "FAIL", "PASS"))
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