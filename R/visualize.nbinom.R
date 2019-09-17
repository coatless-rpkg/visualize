#' Visualize Negative Binomial Distribution
#' 
#' Generates a plot of the Negative Binomial distribution with user specified
#' parameters.
#' 
#' 
#' @param stat a statistic to obtain the probability from. When using the
#' "bounded" condition, you must supply the parameter as `stat =
#' c(lower_bound, upper_bound)`. Otherwise, a simple `stat =
#' desired_point` will suffice.
#' @param size number of objects.
#' @param prob probability of picking object.
#' @param section Select how you want the statistic(s) evaluated via
#' `section=` either `"lower"`,`"bounded"`, `"upper"`,
#' or`"tails"`.
#' @param strict Determines whether the probability will be generated as a
#' strict (<, >) or equal to (<=, >=) inequality. `strict=` requires
#' either values = 0 or = FALSE for equal to OR values =1 or =TRUE for strict.
#' For bounded condition use: `strict=c(0,1)` or
#' `strict=c(FALSE,TRUE)`.
#' @author James Balamuta
#' @export
#' @seealso [visualize.it()] , [dnbinom()].
#' @keywords visualize
#' @examples
#' 
#' # Evaluates lower tail.
#' visualize.nbinom(stat = 1, size = 5, prob = 0.5, section = "lower", strict = 0) 
#' 
#' # Evaluates bounded region.
#' visualize.nbinom(stat = c(1,3), size = 10, prob = 0.35, section = "bounded",
#'                     strict = c(TRUE, FALSE))
#' 
#' # Evaluates upper tail.
#' visualize.nbinom(stat = 1, size = 5, prob = 0.5, section = "upper", strict = 1)
#' 
visualize.nbinom <-
function(stat = 1, size = 6, prob = .5, section = "lower", strict = FALSE) {
  visualize.it('nbinom', stat = stat, list(size = size, prob = prob), section = section, strict=strict)
}
