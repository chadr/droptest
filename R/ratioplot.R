ratioplot <- function(simq = seq(0.01, 0.20, by=0.01),
                      colors = c("RED", "GREEN"), ...) {
  #' Creates plot of pass/fail ratio for repeated test series.
  #' 
  #' \code{ratioplot} Creates plot of pass/fail ratio for repeated test
  #'  series. By simulated probability of reaction.
  #' 
  #' @param simq Vector. Probabilities of failure (q) to use for simulated
  #'   trials. Defaults to range of q = 0.01 to q = 0.5 in 0.05 steps.
  #' @param colors Vector. Passes color options to \code{barplot}. Defaults to
  #'   red and green.
  #' @param ... Passes values to \code{dgroups}.
  #'
  #' @return none   
  #'         
  #' @examples
  #'  # recommended to use num.series value greater than or equal to 1000.
  #'  ratioplot(simq = seq(0.01, 0.20, by = 0.01), num.series = 100)
  #'  ratioplot(simq = seq(0.01, 0.20, by = 0.01), num.series = 100,
  #'            colors = c("BLUE", "BLACK"))
  #'            
  #' @author Chad Ross \email{chad.ross@gmail.com}
  #'   
  #' @seealso 
  #'   \code{\link{dgroups}} 
  #'   \code{\link{dseries}}
  #'   \code{\link{dtrials}}
  #'   \code{\link{droptest}}
  #'   
  # get pass through args
  passed <- list(...)
  
  # generate simulated test data
  sim.data <- dgroups(probs = simq, ...)
  # convert p to percent
  sim.data$Q <- sim.data$Q * 100
    
  # collect frequencies into table
  sim.table <- table(sim.data$RESULT, sim.data$Q)
  
  # expand right side of clipping rect to make room for the legend
  par(xpd = TRUE, mar = par()$mar+c(0,0,0,4))
  
  # create plot
  barplot(sim.table, main = "RATIO OF PASS/FAIL FOR REPEATED TEST SERIES",
          xlab = "SIMULATED PROBABILITY OF REACTION, PERCENT", 
          ylab = "TOTAL NUMBER OF SIMULATED TEST SERIES",
          col = colors)
  
  # plot legend where you want
  legend(ncol(sim.table) + 5, passed$num.series, rownames(sim.table),
         col = colors, pch = 15, bty = "n")
  
  # restore default clipping rect
  par(mar = c(5, 4, 4, 2) + 0.1)
  
}