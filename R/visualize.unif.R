#' Visualize Uniform Distribution
#' 
#' Generates a plot of the Uniform distribution with user specified parameters.
#' 
#' 
#' @param stat a statistic to obtain the probability from. When using the
#' "bounded" condition, you must supply the parameter as `stat =
#' c(lower_bound, upper_bound)`. Otherwise, a simple `stat =
#' desired_point` will suffice.
#' @param a starting point. Note: `a`<`b`
#' @param b end point. Note: `b` > `a`
#' @param section Select how you want the statistic(s) evaluated via
#' `section=` either `"lower"`,`"bounded"`, `"upper"`,
#' or`"tails"`.
#' @author James Balamuta
#' @seealso [visualize.it()] , [dunif()].
#' @export
#' @keywords continuous-distribution
#' @examples
#' 
#' # Evaluates lower tail.
#' visualize.unif(stat = 8.75, a = 7, b = 10, section = "lower") 
#' 
#' # Evaluates bounded region.
#' visualize.unif(stat = c(3,6), a = 1, b = 7, section = "bounded")
#' 
#' # Evaluates upper tail.
#' visualize.unif(stat = 2, a = 1, b = 5, section = "upper")
#' 
visualize.unif <-
function(stat = 1, a = 0, b = 1, section = "lower") {
  visualize.it('unif', stat = stat, list(a = a, b = b), section = section)
}
