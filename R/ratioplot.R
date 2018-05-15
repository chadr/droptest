ratioplot <- function(simp = seq(0.01, 0.20, by=0.01), ...) {
    # Creates a plot of pass/fail ratio for repeated test series, by simulated
    # probability of reaction.
    #
    # Args:
    #   simp: Vector. Probabilities to use for simulated trials. Defaults to
    #   range of p = 0.01 to p = 0.2 in 0.01 steps.
    #
    # Returns:
    #   Data table of multiple simulated drop tests. Note: Only data frame is
    #     supported at this time.
    
    # generate simulated test data
    sim.data <- dgroups(probs = simp, ...)
    # convert p to percent
    sim.data$P <- sim.data$P * 100
    
    # collect frequencies into table
    sim.table <- table(sim.data$RESULT, sim.data$P)
  
    # expand right side of clipping rect to make room for the legend
    par(xpd = TRUE, mar = par()$mar+c(0,0,0,4))
  
    # create plot
    barplot(sim.table, main = "RATIO OF PASS/FAIL FOR REPEATED TEST SERIES",
            xlab = "SIMULATED PROBABILITY OF REACTION, PERCENT", 
            ylab = "TOTAL NUMBER OF SIMULATED TEST SERIES",
            col = c("RED", "GREEN"))
  
    # plot legend where you want
    legend(ncol(sim.table) + 5, 100, rownames(sim.table),
    	   col = c("RED", "GREEN"), pch = 15, bty = "n")
  
    # restore default clipping rect
    par(mar = c(5, 4, 4, 2) + 0.1)
  
}