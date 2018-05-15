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
  #   simp: Vector. Probabilities to use for simulated trials. Defaults to
  #     range of p = 0.01 to p = 0.5 in 0.05 steps.
  #
  require("data.table")
  
  arguments <- list(...)
  
  # generate groups of simulated data
  obs.group <- groups(probs = simp, ...)
  
  #aggregate by P and get std deviation of reactions
  obs.group$PCT_REACT_SD <- obs.group$PCT_REACT
  obs.agg <- aggregate(PCT_REACT_SD ~ P, FUN = sd, data = obs.group)
  obs.agg$P <- obs.agg$P * 100
  obs.agg$PCT_REACT_SD <- obs.agg$PCT_REACT_SD * 100
  
  # generate binomial distribution data
  simb = seq(0.01, 0.5, by=0.01)
  q <- 1 - simb
  n <- 20
  binomial.group <- data.frame(BIN_P = 100 * simb, 
                               BIN_SD = 100 * sqrt((simb * q) / n))
  
  # read historical data from rds file
  old <- readRDS("../data/D7905.rds")
  old$P <- old$P * 100
  old$SD <- old$SD * 100
  
  # smooth curve for simulated std deviation points
  sm.sim.sd = smooth.spline(obs.agg$P, obs.agg$PCT_REACT_SD, spar=0.35)

  # create binomial plot
  # plot binomial distribution curve
  plot(binomial.group$BIN_P, binomial.group$BIN_SD, col = "red", lwd = 2,
       type = "l", ylim = c(0, 18), xlim=c(0, 50), main = "GAYLE PLOT",
       xlab = "PROBABILITY OF REACTION, PERCENT",
       ylab = "STANDARD DEVIATION, PERCENT")
  # plot data points used in original paper
  points(old$P, old$SD, col = "black", pch=15)
  # plot simulated std dev points
  points(obs.agg$P, obs.agg$PCT_REACT_SD, pch = 19)
  # plot smoothed curve for simulated std deviation points
  lines(sm.sim.sd, col = "blue", lwd=2)
  # legend
  legend(x = 39.5, y = 3, legend = c("Non-Truncated (Historical)",
        "Truncated (Simulated)"), col = "black", pch = c(15, 19))
}