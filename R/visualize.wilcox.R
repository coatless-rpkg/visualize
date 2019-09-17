#' Visualize Cauchy Distribution
#' 
#' Generates a plot of the Wilcoxon Rank Sum distribution with user specified
#' parameters.
#' 
#' 
#' @param stat a statistic to obtain the probability from. When using the
#' "bounded" condition, you must supply the parameter as `stat =
#' c(lower_bound, upper_bound)`. Otherwise, a simple `stat =
#' desired_point` will suffice.
#' @param m Sample size from group 1.
#' @param n Sample size from group 2.
#' @param section Select how you want the statistic(s) evaluated via
#' `section=` either `"lower"`,`"bounded"`, `"upper"`,
#' or`"tails"`.
#' @return Returns a plot of the distribution according to the conditions
#' supplied.
#' @author James Balamuta
#' @export
#' @seealso [visualize.it()], [dwilcox()].
#' @keywords visualize
#' @examples
#' 
#' # Evaluates lower tail.
#' visualize.wilcox(stat = 1, m = 7, n = 3, section = "lower") 
#' 
#' # Evaluates bounded region.
#' visualize.wilcox(stat = c(2,3), m = 5, n = 4, section = "bounded") 
#' 
#' # Evaluates upper tail.
#' visualize.wilcox(stat = 1, m = 7, n = 3, section = "upper") 
#' 
visualize.wilcox <-
function(stat = 1, m = 7, n = 3, section = "lower") {
  visualize.it('wilcox', stat = stat, list(m = m, n = n), section = section)
}
