#' Visualize Hypergeometric Distribution
#' 
#' Generates a plot of the Hypergeometric distribution with user specified
#' parameters.
#' 
#' 
#' @param stat a statistic to obtain the probability from. When using the
#' "bounded" condition, you must supply the parameter as \code{stat =
#' c(lower_bound, upper_bound)}. Otherwise, a simple \code{stat =
#' desired_point} will suffice.
#' @param m \code{m} white balls. \code{m} must be greater than 0.
#' @param n \code{n} black balls. \code{n} must be greater than 0.
#' @param k draw \code{k} balls without replacement.
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
#' @seealso \code{\link{visualize.it}} , \code{\link{dhyper}}.
#' @keywords visualize
#' @examples
#' 
#' # Evaluates lower tail.
#' visualize.hyper(stat = 1, m=4, n=5, k=3, section = "lower", strict = 0) 
#' 
#' # Evaluates bounded region.
#' visualize.hyper(stat = c(2,4), m=14, n=5, k=2, section = "bounded", strict = c(0,1))
#' 
#' # Evaluates upper tail.
#' visualize.hyper(stat = 1, m=4, n=5, k=3, section = "upper", strict = 1)
#' 
visualize.hyper <-
function(stat = 1, m = 5, n = 4, k = 2, section = "lower", strict = FALSE) {
  visualize.it('hyper', stat = stat, list(m = m, n = n, k = k), section = section, strict=strict)
}
