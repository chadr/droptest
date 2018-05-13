binomialplot <- function(...) {
  arguments <- list(...)
  
  # values of P for simulated data
  p.range <- seq(0.01, 0.5, by=0.05)

  # values of P for binomial distribution curve
  b.range <- seq(0.01, 0.5, by=0.01)
  
  # generate groups of simulated data
  obs.group <- groups(multi.p = TRUE, probs = p.range, ...)
  
  #aggregate by P and get std deviation of reactions
  obs.group$PCT_REACT_SD <- obs.group$PCT_REACT
  obs.group <- aggregate(PCT_REACT_SD ~ P, FUN = sd, data = obs.group)
  
  # generate binomial distribution data
  q <- 1 - b.range
  n <- arguments$max.trials
  binomial.group <- data.frame(BIN_P = b.range, 
  	                           BIN_SD = sqrt((b.range * q) / n))
  
  old <- data.frame(P = c(0.01, 0.02, 0.03, 0.04, 0.05, 0.0656, 0.0908, 0.1562, 0.256,0.4),
                    SD = c(.022, .027, 0.034, 0.045, 0.047, 0.060, 0.066, 0.079, 0.085, 0.125))
  
  # smooth curve for simulated std dev points
  smoothingSpline = smooth.spline(obs.group$P, obs.group$PCT_REACT_SD,
                                  spar=0.35)
  
  # create binomial plot
  # plot simulated std dev points
  plot(obs.group$P, obs.group$PCT_REACT_SD, pch = 19, ylim = c(0, 0.18),
       xlim=c(0, 0.5))
  # add smoothed curve for simulated std dev points
  lines(smoothingSpline, col = "blue", lwd=2)
  # add binomial distribution curve
  lines(binomial.group$BIN_P, binomial.group$BIN_SD, col = "red", lwd = 2,)
  # add data points used in original paper
  points(old$P, old$SD, col = "black", pch=15)
}
