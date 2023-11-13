#' Visualize F distribution
#' 
#' Generates a plot of the F distribution with user specified parameters.
#' 
#' 
#' @param stat a statistic to obtain the probability from. When using the
#' "bounded" condition, you must supply the parameter as `stat =
#' c(lower_bound, upper_bound)`. Otherwise, a simple `stat =
#' desired_point` will suffice.
#' @param df1 First Degrees of Freedom
#' @param df2 Second Degrees of Freedom
#' @param section Select how you want the statistic(s) evaluated via
#' `section=` either `"lower"`,`"bounded"`, `"upper"`,
#' or`"tails"`.
#' @return Returns a plot of the distribution according to the conditions
#' supplied.
#' @author James Balamuta
#' @export
#' @seealso [visualize.it()], [df()].
#' @keywords continuous-distribution
#' @examples
#' 
#' # Evaluates lower tail.
#' visualize.f(stat = 1, df1 = 5, df2 = 4, section = "lower") 
#' 
#' # Evaluates bounded region.
#' visualize.f(stat = c(3,5), df1 = 6, df2 = 3, section = "bounded") 
#' 
#' # Evaluates upper tail.
#' visualize.f(stat = 1, df1 = 5, df2 = 4, section = "upper") 
#' 
visualize.f <-
function(stat = 1, df1 = 5, df2 = 4, section = "lower") {
  visualize.it('f', stat = stat, list(df1 = df1, df2 = df2), section = section)
}
