#' Visualize Student's t distribution
#' 
#' Generates a plot of the Student's t distribution with user specified
#' parameters.
#' 
#' 
#' @param stat a statistic to obtain the probability from. When using the
#' "bounded" condition, you must supply the parameter as `stat =
#' c(lower_bound, upper_bound)`. Otherwise, a simple `stat =
#' desired_point` will suffice.
#' @param df Degrees of freedom
#' @param section Select how you want the statistic(s) evaluated via
#' `section=` either `"lower"`,`"bounded"`, `"upper"`,
#' or`"tails"`.
#' @return Returns a plot of the distribution according to the conditions
#' supplied.
#' @author James Balamuta
#' @export
#' @seealso [visualize.it()], [dt()].
#' @keywords visualize
#' @examples
#' 
#' # Evaluates lower tail.
#' visualize.t(stat = 1, df = 4, section = "lower") 
#' 
#' # Evaluates bounded region.
#' visualize.t(stat = c(3,5), df = 6, section = "bounded") 
#' 
#' # Evaluates upper tail.
#' visualize.t(stat = 1, df = 4, section = "upper") 
#' 
visualize.t <-
function(stat = 1, df = 3, section = "lower") {
  visualize.it('t', stat = stat, list(df = df), section = section)
}
