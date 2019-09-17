#' Visualize Geometric Distribution
#' 
#' Generates a plot of the Geometric distribution with user specified
#' parameters.
#' 
#' 
#' @param stat a statistic to obtain the probability from. When using the
#' "bounded" condition, you must supply the parameter as `stat =
#' c(lower_bound, upper_bound)`. Otherwise, a simple `stat =
#' desired_point` will suffice.
#' @param prob probability of picking object.
#' @param section Select how you want the statistic(s) evaluated via
#' `section=` either `"lower"`,`"bounded"`, `"upper"`,
#' or`"tails"`.
#' @param strict Determines whether the probability will be generated as a
#' strict (<, >) or equal to (<=, >=) inequality. `strict=` requires
#' either values = 0 or =FALSE for equal to OR values =1 or =TRUE for strict.
#' For bounded condition use: `strict=c(0,1)` or
#' `strict=c(FALSE,TRUE)`.
#' @author James Balamuta
#' @export
#' @seealso [visualize.it()] , [dgeom()].
#' @keywords visualize
#' @examples
#' 
#' # Evaluates lower tail.
#' visualize.geom(stat = 1, prob = 0.5, section = "lower", strict = FALSE) 
#' 
#' # Evaluates bounded region.
#' visualize.geom(stat = c(1,3), prob = 0.35, section = "bounded", strict = c(0,1))
#' 
#' # Evaluates upper tail.
#' visualize.geom(stat = 1, prob = 0.5, section = "upper", strict = 1)
#' 
visualize.geom <-
function(stat = 1, prob = .3, section = "lower", strict = FALSE) {
  visualize.it('geom', stat = stat, list(prob = prob), section = section, strict=strict)
}
