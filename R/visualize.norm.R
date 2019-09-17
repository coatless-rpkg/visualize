#' Visualize Normal Distribution
#' 
#' Generates a plot of the Normal distribution with user specified parameters.
#' 
#' 
#' @param stat a statistic to obtain the probability from. When using the
#' "bounded" condition, you must supply the parameter as `stat =
#' c(lower_bound, upper_bound)`. Otherwise, a simple `stat =
#' desired_point` will suffice.
#' @param mu mean of the Normal Distribution.
#' @param sd standard deviation of the Normal Distribution.
#' @param section Select how you want the statistic(s) evaluated via
#' `section=` either `"lower"`,`"bounded"`, `"upper"`,
#' or`"tails"`.
#' @export
#' @seealso [visualize.it()] , [dnorm()].
#' @keywords visualize
#' @examples
#' 
#' # Evaluates lower tail.
#' visualize.norm(stat = 1, mu = 4, sd = 5, section = "lower") 
#' 
#' # Evaluates bounded region.
#' visualize.norm(stat = c(3,6), mu = 5, sd = 3, section = "bounded")
#' 
#' # Evaluates upper tail.
#' visualize.norm(stat = 1, mu = 3, sd = 2, section = "upper")
#' 
visualize.norm <-
function(stat = 1, mu = 0, sd = 1, section = "lower") {
  visualize.it('norm', stat = stat, list(mu = mu, sd = sd),section = section)
}
