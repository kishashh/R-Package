#' Normal Distribution Curve Function
#'
#' @param mu
#' @param sigma
#' @param a
#'
#' @return myncurve function from Lab 6
#' @export
#'
#' @examples
#' myncurve(mu = 0, sigma = 1, a = 1.5)
myncurve = function(mu, sigma, a){
  x=NULL
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma))
  xcurve = seq(mu-3*sigma,a,length = 1000)
  ycurve = dnorm(xcurve, mean=mu, sd=sigma)
  polygon(x = c(mu-3*sigma,xcurve,a),y = c(0,ycurve,0),col = "Red")

  prob = pnorm(a,mean=mu,sd=sigma)
  prob2 <- round(prob,4)
  prob2
}
