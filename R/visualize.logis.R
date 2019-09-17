#' Visualize Logistic distribution
#' 
#' Generates a plot of the Logistic distribution with user specified
#' parameters.
#' 
#' 
#' @param stat a statistic to obtain the probability from. When using the
#' "bounded" condition, you must supply the parameter as `stat =
#' c(lower_bound, upper_bound)`. Otherwise, a simple `stat =
#' desired_point` will suffice.
#' @param location Location of the distribution.
#' @param scale Scale of the distribution.
#' @param section Select how you want the statistic(s) evaluated via
#' `section=` either `"lower"`,`"bounded"`, `"upper"`,
#' or`"tails"`.
#' @return Returns a plot of the distribution according to the conditions
#' supplied.
#' @author James Balamuta
#' @export
#' @seealso [visualize.it()], [dlogis()].
#' @keywords visualize
#' @examples
#' 
#' # Evaluates lower tail.
#' visualize.logis(stat = 1, location = 4, scale = 2, section = "lower") 
#' 
#' # Evaluates bounded region.
#' visualize.logis(stat = c(3,5), location = 4, scale = 2, section = "bounded") 
#' 
#' # Evaluates upper tail.
#' visualize.logis(stat = 1, location = 4, scale = 2, section = "upper") 
#' 
visualize.logis <-
function(stat = 1, location = 3, scale = 1, section = "lower") {
  visualize.it('logis', stat = stat, list(location = location, scale = location), section = section)
}
