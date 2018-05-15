gayleplot <- function(simp = seq(0.01, 0.5, by=0.05), ...) {
  # Creates plot of standard deviation (%), vs probability of reaction (%).
  # Similar to the one seen in NASA Technical Note NASA-TN D-7905. (1970)
  # Written by J. B. Gayle. The namesake of this function.
  # https://ntrs.nasa.gov/archive/nasa/casi.ntrs.nasa.gov/19750014413.pdf
  #
  # Plots historical non-truncated data standard deviations against binomial
  # process. Also plots standard deviations of truncated data (from
  # simulation).
  #
  # Args:
  #   simp: Vector. Probabilities to use for simulated values. Defaults to
  #     range of p = 0.01 to p = 0.05 in 0.05 steps.
  # Returns:
  #   Data table of multiple simulated drop tests. Note: Only data frame is
  #     supported at this time.
  require("data.table")
  
  arguments <- list(...)
  
  # generate groups of simulated data
  obs.group <- groups(probs = simp, ...)
  
  #aggregate by P and get std deviation of reactions
  obs.group$PCT_REACT_SD <- obs.group$PCT_REACT
  obs.agg <- aggregate(PCT_REACT_SD ~ P, FUN = sd, data = obs.group)
  
  # generate binomial distribution data
  simb = seq(0.01, 0.5, by=0.01)
  q <- 1 - simb
  n <- arguments$max.trials
  binomial.group <- data.frame(BIN_P = simb, 
                               BIN_SD = sqrt((simb * q) / n))
  
  # read historical data from rds file
  old <- readRDS("../data/D7905.rds")
  
  # smooth curve for simulated std deviation points
  sm.sim.sd = smooth.spline(obs.agg$P, obs.agg$PCT_REACT_SD,
                            spar=0.35)

  # create binomial plot
  # plot binomial distribution curve
  plot(binomial.group$BIN_P, binomial.group$BIN_SD, col = "red", lwd = 2,
       type = "l", ylim = c(0, 0.18), xlim=c(0, 0.5), main = "GAYLE PLOT",
       xlab = "PROBABILITY OF REACTION, PERCENT",
       ylab = "STANDARD DEVIATION, PERCENT")
  # plot data points used in original paper
  points(old$P, old$SD, col = "black", pch=15)
  # plot simulated std dev points
  points(obs.agg$P, obs.agg$PCT_REACT_SD, pch = 19)
  # plot smoothed curve for simulated std deviation points
  lines(sm.sim.sd, col = "blue", lwd=2)
  # legend
  legend(x = 0.395, y = 0.03, legend = c("Non-Truncated (Historical)",
        "Truncated (Simulated)"), col = "black", pch = c(15, 19))
}