#' @title My Normal Curve
#'
#' @param a value to check P(Y <= a)
#' @param mu mean of the formula
#' @param sigma standard Deviation of the formula
#'
#' @returns list with mu = mean, signma = standard deviation, area = area Y ~ N(x,y), P(Y <= a)
#' @export
#'
#' @examples
#' {myncurve(3, 10, 5)}
#' {myncurve(1, 0.5, 0.1)}
myncurve = function(a, mu, sigma){
  #Curve
  curve(dnorm(x, mean = mu, sd = sigma), xlim = c(mu - 3*sigma, mu + 3*sigma))

  # Separate the x and y curves
  xcurve <- seq((mu - 4*sigma), a, length = 1000)
  ycurve <- dnorm(xcurve, mean = mu, sd = sigma)

  # Plot the polygon
  polygon(x = c((mu - 4*sigma), xcurve, a), y = c(0, ycurve, 0), col = "purple")
  area <- round((pnorm(a, mean = mu, sd = sigma) - pnorm((mu - 4*sigma), mean = mu, sd = sigma)), 4)

  # Print Area on curve
  # mtext allows text outside the plot, and 1 means it is
  # on the bottom, line is how far away the text is
  mtext(paste0("Purple Area = ", area), side = 1, line = 2)

  list(mu = mu, sigma = sigma, area = area)
}
