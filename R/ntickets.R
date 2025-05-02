#' @title Number of Tickets
#' @param N number of seats
#' @param gamma value (0.01-0.99) that the train is overbooked
#' @param p probability (0.00-0.99) of NOT overbooking
#'
#' @description
#' A function that find the ideal numbder of tickets for given values to sell that will be unlikely to be overbooked.
#'
#' @returns a list: num tickets discrete, num tickets continuous, Num seats, gamma, probability confidence, and 2 plot in one
#' @export
#'
#' @importFrom stats dbinom pnorm
#' @importFrom graphics layout layout.show points abline
#'
#' @examples
#' ntickets(N = 400, gamma = 0.2, p = 0.95)
ntickets <- function(N, gamma, p) {
  # Define the possible ticket sales range
  npos <- seq(N, N * 1.5, by = 1)

  # Discrete method
  overbook_prob <- function(n) {
    sum(dbinom((N + 1):n, n, p))
  }

  # Compute probibilities for all npos
  # sapply applys the vector elements to the function
  probs <- sapply(npos, overbook_prob)

  # Find the index where the prob is closest to gamma
  index_discrete <- which.min(abs(probs - gamma))
  nd <- npos[index_discrete]

  # Normal approximation method
  mu <- npos * p  # Mean
  sigma <- sqrt(npos * p * (1 - p))  # Standard deviation

  # Compute z-score for overbooking prob
  z <- (N - mu) / sigma
  probs_normal <- 1 - pnorm(z)

  # Find the index where the prob is closest to gamma
  index_normal <- which.min(abs(probs_normal - gamma))
  nc <- npos[index_normal]

  # Compute the objective functions: prob difference from gamma
  obj_discrete <- probs - gamma
  obj_normal <- probs_normal - gamma

  # Set up two separate plots
  layout(matrix(1:2, nrow = 2, ncol = 2))
  layout.show(n = 2)

  # Plots the objective function for discrete approx
  plot(npos, obj_discrete, type = "l", col = "blue",
       ylab = "Objective Function", xlab = "Number of Tickets Sold",
       main = "Objective Function - Discrete")
  points(nd, probs[which(npos == nd)], col = "blue")
  abline(h = 0, col = "black", lty = 2)  # Reference line at 0

  # Plots the objective function for normal approx
  plot(npos, obj_normal, type = "l", col = "red",
       ylab = "Objective Function", xlab = "Number of Tickets Sold",
       main = "Objective Function - Normal Approximation")
  abline(h = 0, col = "black", lty = 2)
  points(nc, probs_normal[which(npos == nc)], col = "red")

  # Returns a named list of results
  return(list(nd = nd, nc = nc, N = N, p = p, gamma = gamma))
}
