#' Visualize Log Normal Distribution
#' 
#' Generates a plot of the Log Normal distribution with user specified
#' parameters.
#' 
#' 
#' @param stat a statistic to obtain the probability from. When using the
#' "bounded" condition, you must supply the parameter as `stat =
#' c(lower_bound, upper_bound)`. Otherwise, a simple `stat =
#' desired_point` will suffice.
#' @param meanlog Mean of the distribution
#' @param sdlog Standard deviation of the distribution
#' @param section Select how you want the statistic(s) evaluated via
#' `section=` either `"lower"`,`"bounded"`, `"upper"`,
#' or`"tails"`.
#' @return Returns a plot of the distribution according to the conditions
#' supplied.
#' @author James Balamuta
#' @export
#' @seealso [visualize.it()], [dlnorm()].
#' @keywords visualize
#' @examples
#' 
#' # Evaluates lower tail.
#' visualize.lnorm(stat = 1, meanlog = 3, sdlog = 1, section = "lower") 
#' 
#' # Evaluates bounded region.
#' visualize.lnorm(stat = c(3,5), meanlog = 3, sdlog = 3, section = "bounded") 
#' 
#' # Evaluates upper tail.
#' visualize.lnorm(stat = 1, meanlog = 3, sdlog = 1, section = "upper") 
#' 
visualize.lnorm <-
function(stat = 1, meanlog = 3, sdlog = 1, section = "lower") {
  visualize.it('lnorm', stat = stat, list(meanlog = meanlog, sdlog = sdlog), section = section)
}
