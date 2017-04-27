#' Visualize Exponential Distribution
#' 
#' Generates a plot of the Exponential distribution with user specified
#' parameters.
#' 
#' 
#' @param stat a statistic to obtain the probability from. When using the
#' "bounded" condition, you must supply the parameter as \code{stat =
#' c(lower_bound, upper_bound)}. Otherwise, a simple \code{stat =
#' desired_point} will suffice.
#' @param theta vector of rates
#' @param section Select how you want the statistic(s) evaluated via
#' \code{section=} either \code{"lower"},\code{"bounded"}, \code{"upper"},
#' or\code{"tails"}.
#' @return Returns a plot of the distribution according to the conditions
#' supplied.
#' @author James Balamuta
#' @export
#' @seealso \code{\link{visualize.it}}, \code{\link{dexp}}.
#' @keywords visualize
#' @examples
#' 
#' # Evaluates lower tail.
#' visualize.exp(stat = .5, theta = 3, section = "lower")
#' 
#' # Evaluates bounded region.
#' visualize.exp(stat = c(1,2), theta = 3, section = "bounded")
#' 
#' # Evaluates upper tail.
#' visualize.exp(stat = .5, theta = 3, section = "upper")
#' 
visualize.exp <-
function(stat = 1, theta = 1, section = "lower") {
  visualize.it('exp', stat = stat, list(theta = theta), section = section)
}
