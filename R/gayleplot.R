gayleplot <- function(simq = seq(0.01, 0.5, by = 0.05), ...) {
  #' Plot of std deviation percent, vs probability of reaction percent.
  #'
  #' \code{gayleplot} Creates plot similar to the one seen in NASA Technical
  #' Note \strong{NASA-TN-D-7905}. (1970) Written by J. B. Gayle. The namesake
  #' of this function.
  #' \url{https://ntrs.nasa.gov/archive/nasa/casi.ntrs.nasa.gov/19750014413.pdf}
  #' 
  #' Plots historical standard deviations (%) -- where test series is not halted
  #' mid-test -- against binomial process. Also plots standard deviations (%)
  #' from simulation using modern procedure.
  #' 
  #' Shows how historical data follows a binomial process, but simulated data
  #' produced using modern procedure does not.
  #'
  #' @param simq Vector. Probabilities of failure (q) to use for simulated
  #'   trials. Defaults to range of q = 0.01 to q = 0.5 in 0.05 steps.
  #' @param ... Passes values to \code{dgroups}.
  #'   
  #' @return none   
  #'   
  #' @examples
  #'   # recommended to use num.series value greater than or equal to 1000.
  #'   gayleplot(num.series = 100)
  #'   gayleplot(num.series = 100, simq = seq(0.01, 0.05, by = 0.01))
  #'   
  #' @author Chad Ross \email{chad.ross@gmail.com}
  #' 
  #' @seealso 
  #'   \code{\link{dgroups}} 
  #'   \code{\link{dseries}}
  #'   \code{\link{dtrials}}
  #'   \code{\link{droptest}}

  arguments <- list(...)
  
  # generate groups of simulated data
  obs.group <- dgroups(probs = simq, ...)
  
  #aggregate by Q and get std deviation of reactions
  obs.group$PCT_REACT_SD <- obs.group$PCT_REACT
  obs.agg <- aggregate(PCT_REACT_SD ~ Q, FUN = sd, data = obs.group)

  # convert decimal to percent
  obs.agg$Q <- obs.agg$Q * 100
  obs.agg$PCT_REACT_SD <- obs.agg$PCT_REACT_SD * 100
  
  # generate binomial distribution data
  simb = seq(0.01, 0.5, by=0.01)
  q <- 1 - simb
  n <- 20
  binomial.group <- data.table(BIN_P = 100 * simb, 
                               BIN_SD = 100 * sqrt((simb * q) / n))
  
  # historical non-truncated data
  old <- data.frame(droptest::D7905)
  old$P <- old$P * 100
  old$SD <- old$SD * 100
  
  # smooth curve for simulated std deviation points
  sm.sim.sd = smooth.spline(obs.agg$Q, obs.agg$PCT_REACT_SD, spar=0.35)

  # create binomial plot
  # plot binomial distribution curve
  plot(binomial.group$BIN_P, binomial.group$BIN_SD, col = "red", lwd = 2,
       type = "l", ylim = c(0, 18), xlim=c(0, 50), main = "GAYLE PLOT",
       xlab = "PROBABILITY OF REACTION, PERCENT",
       ylab = "STANDARD DEVIATION, PERCENT")
  # plot data points used in original paper
  points(old$P, old$SD, col = "black", pch=15)
  # plot simulated std dev points
  points(obs.agg$Q, obs.agg$PCT_REACT_SD, pch = 19)
  # plot smoothed curve for simulated std deviation points
  lines(sm.sim.sd, col = "blue", lwd=2)
  # legend
  legend("bottomright", legend = c("Non-Truncated (Historical)",
         "Truncated (Simulated)"), col = "black", pch = c(15, 19))
}