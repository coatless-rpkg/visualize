#' Visualize Exponential Distribution
#' 
#' Generates a plot of the Exponential distribution with user specified
#' parameters.
#' 
#' 
#' @param stat a statistic to obtain the probability from. When using the
#' "bounded" condition, you must supply the parameter as `stat =
#' c(lower_bound, upper_bound)`. Otherwise, a simple `stat =
#' desired_point` will suffice.
#' @param theta vector of rates
#' @param section Select how you want the statistic(s) evaluated via
#' `section=` either `"lower"`,`"bounded"`, `"upper"`,
#' or`"tails"`.
#' @return Returns a plot of the distribution according to the conditions
#' supplied.
#' @author James Balamuta
#' @export
#' @seealso [visualize.it()], [dexp()].
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
