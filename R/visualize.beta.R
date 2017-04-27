#' Visualize Beta Distribution
#' 
#' Generates a plot of the Beta distribution with user specified parameters.
#' 
#' 
#' @param stat a statistic to obtain the probability from. When using the
#' "bounded" condition, you must supply the parameter as \code{stat =
#' c(lower_bound, upper_bound)}. Otherwise, a simple \code{stat =
#' desired_point} will suffice.
#' @param alpha \code{alpha} is considered to be \emph{shape1} by R's
#' implementation of the beta distribution. \code{alpha} must be greater than
#' 0.
#' @param beta \code{beta} is considered to be \emph{shape2} by R's
#' implementation of the beta distribution. \code{beta} must be greater than 0.
#' @param section Select how you want the statistic(s) evaluated via
#' \code{section=} either \code{"lower"},\code{"bounded"}, \code{"upper"},
#' or\code{"tails"}.
#' @return Returns a plot of the distribution according to the conditions
#' supplied.
#' @author James Balamuta
#' @export
#' @seealso \code{\link{visualize.it}}, \code{\link{dbeta}}.
#' @keywords visualize
#' @examples
#' 
#' # Evaluates lower tail.
#' visualize.beta(stat = 1, alpha = 2, beta = 3, section = "lower") 
#' 
#' # Evaluates bounded region.
#' visualize.beta(stat = c(.5,1), alpha = 4, beta = 3, section = "bounded") 
#' 
#' # Evaluates upper tail.
#' visualize.beta(stat = 1, alpha = 2, beta = 3, section = "upper") 
#' 
visualize.beta <-
function(stat = 1, alpha = 3, beta = 2, section = "lower") {
  visualize.it('beta', stat = stat, list(alpha = alpha, beta = beta),section = section)
}
