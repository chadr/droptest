gayleplot <- function(...) {
    # Creates plot of standard deviation (%), vs probability of reaction (%).
    # Similar to the one seen in NASA Technical Note NASA-TN D-7905. (1970)
    # Written by J. B. Gayle. The namesake of this function.
    # https://ntrs.nasa.gov/archive/nasa/casi.ntrs.nasa.gov/19750014413.pdf
    #
    # Plots historical non-truncated data standard deviations against binomial
    # process. Also plots standard deviations of truncated data (from
    # simulation) and "trial deviation" as defined by the function "trialdev"
    # in this package.
    #
    # Returns:
    #   Data table of multiple simulated drop tests. Note: Only data frame is
    #     supported at this time.
    require("data.table")

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

    trial.dev <- trialdev(obs.group)

    # generate binomial distribution data
    q <- 1 - b.range
    n <- arguments$max.trials
    binomial.group <- data.frame(BIN_P = b.range, 
                                 BIN_SD = sqrt((b.range * q) / n))

    # read historical data from rds file
    old <- readRDS("../data/D7905.rds")

    # smooth curve for simulated std dev points
    smoothingSpline = smooth.spline(obs.group$P, obs.group$PCT_REACT_SD,
                                    spar=0.35)
    sm = smooth.spline(trial.dev$P, trial.dev$TD, spar=0.35)

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
    points(trial.dev$P, trial.dev$TD, pch=17)
    lines(sm, lwd=2, col="green")
}
