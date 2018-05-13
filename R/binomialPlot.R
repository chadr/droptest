binomialplot <- function(...) {
  arguments <- list(...)
  #print(arguments)
  p.range <- seq(0.01, 0.5, by=0.01)
  # requires: max.trials, num.series, probs
  obs.group <- groups(multi.p = TRUE, probs = p.range, ...)
  #aggregate by p and get sd of $reactions
  obs.group$PCT_REACT_SD <- obs.group$PCT_REACT
  obs.group <- aggregate(PCT_REACT_SD ~ P, FUN = sd, data = obs.group)
  #print(obs.group)
  
  q <- 1 - p.range
  n <- arguments$max.trials
  binomial.group <- data.frame(BIN_P = p.range, BIN_SD = sqrt((p.range * q) / n))
  
  old <- data.frame(P = c(0.01, 0.02, 0.03, 0.04, 0.05, 0.0656, 0.0908, 0.1562, 0.256,0.4),
                    SD = c(.022, .027, 0.034, 0.045, 0.047, 0.060, 0.066, 0.079, 0.085, 0.125))
  smoothingSpline = smooth.spline(obs.group$P, obs.group$PCT_REACT_SD, spar=0.35)
  
  plot(obs.group$P, obs.group$PCT_REACT_SD, ylim = c(0, .20))
  lines(smoothingSpline, col = "blue", lwd=2)
  lines(binomial.group$BIN_P, binomial.group$BIN_SD, col = "red", lwd = 2,)
  points(old$P, old$SD, col = "red")
}
