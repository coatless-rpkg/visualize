#' Visualize Cauchy Distribution
#' 
#' Generates a plot of the Cauchy distribution with user specified parameters.
#' 
#' 
#' @param stat a statistic to obtain the probability from. When using the
#' "bounded" condition, you must supply the parameter as `stat =
#' c(lower_bound, upper_bound)`. Otherwise, a simple `stat =
#' desired_point` will suffice.
#' @param location location parameter
#' @param scale scale parameter
#' @param section Select how you want the statistic(s) evaluated via
#' `section=` either `"lower"`,`"bounded"`, `"upper"`,
#' or`"tails"`.
#' @return Returns a plot of the distribution according to the conditions
#' supplied.
#' @author James Balamuta
#' @export
#' @seealso [visualize.it()], [dcauchy()].
#' @keywords continuous-distribution
#' @examples
#' 
#' # Evaluates lower tail.
#' visualize.cauchy(stat = 1, location = 4, scale = 2, section = "lower") 
#' 
#' # Evaluates bounded region.
#' visualize.cauchy(stat = c(3,5), location = 5, scale = 3, section = "bounded") 
#' 
#' # Evaluates upper tail.
#' visualize.cauchy(stat = 1, location = 4, scale = 2, section = "upper") 
#' 
visualize.cauchy <-
function(stat = 1, location = 2, scale = 1, section = "lower") {
  visualize.it('cauchy', stat = stat, list(location = location, scale = scale), section = section)
}
