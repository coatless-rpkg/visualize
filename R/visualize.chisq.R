#' Visualize Chi-squared Distribution
#' 
#' Generates a plot of the Chi-squared distribution with user specified
#' parameters.
#' 
#' 
#' @param stat a statistic to obtain the probability from. When using the
#' "bounded" condition, you must supply the parameter as `stat =
#' c(lower_bound, upper_bound)`. Otherwise, a simple `stat =
#' desired_point` will suffice.
#' @param df degrees of freedom of Chi-squared distribution.
#' @param section Select how you want the statistic(s) evaluated via
#' `section=` either `"lower"`,`"bounded"`, `"upper"`,
#' or`"tails"`.
#' @return Returns a plot of the distribution according to the conditions
#' supplied.
#' @author James Balamuta
#' @export
#' @seealso [visualize.it()], [dchisq()].
#' @keywords visualize
#' @examples
#' 
#' # Evaluates lower tail.
#' visualize.chisq(stat = 1, df = 3, section = "lower")
#' # Evaluates bounded region.
#' visualize.chisq(stat = c(1,2), df = 6, section = "bounded")
#' # Evaluates upper tail.
#' visualize.chisq(stat = 1, df = 3, section = "upper")
#' 
#' 
visualize.chisq <-
function(stat = 1, df = 3, section = "lower") {
  visualize.it('chisq', stat = stat, list(df = df), section = section)
}
