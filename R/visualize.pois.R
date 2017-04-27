#' Visualize Poisson Distribution
#' 
#' Generates a plot of the Poisson distribution with user specified parameters.
#' 
#' 
#' @param stat a statistic to obtain the probability from. When using the
#' "bounded" condition, you must supply the parameter as \code{stat =
#' c(lower_bound, upper_bound)}. Otherwise, a simple \code{stat =
#' desired_point} will suffice.
#' @param lambda lambda value of the Poisson Distribution.
#' @param section Select how you want the statistic(s) evaluated via
#' \code{section=} either \code{"lower"},\code{"bounded"}, \code{"upper"},
#' or\code{"tails"}.
#' @param strict Determines whether the probability will be generated as a
#' strict (<, >) or equal to (<=, >=) inequality. \code{strict=} requires
#' either values = 0 or =FALSE for equal to OR values =1 or =TRUE for strict.
#' For bounded condition use: \code{strict=c(0,1)} or
#' \code{strict=c(FALSE,TRUE)}.
#' @author James Balamuta
#' @export
#' @seealso \code{\link{visualize.it}} , \code{\link{dpois}}.
#' @keywords visualize
#' @examples
#' 
#' # Evaluates lower tail.
#' visualize.pois(stat = 1, lambda = 2, section = "lower", strict = FALSE) 
#' 
#' # Evaluates bounded region.
#' visualize.pois(stat = c(1,3), lambda = 3, section = "bounded", strict = c(0,1))
#' 
#' # Evaluates upper tail.
#' visualize.pois(stat = 1, lambda = 2, section = "upper", strict = 1)
#' 
visualize.pois <-
function(stat = 1, lambda = 3.5, section = "lower", strict = FALSE) {
  visualize.it('pois', stat = stat, list(lambda = lambda), section = section, strict=strict)
}
