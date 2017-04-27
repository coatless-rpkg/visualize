\name{visualize.continuous}
\alias{visualize.continuous}
\title{
Graphing function for Continuous Distributions.
}
\description{
Handles how continuous distributions are graphed. Users should not use this function. Instead, users should use \code{link{visualize.it}}.
}
\usage{
visualize.continuous(dist, stat = c(0, 1), params, section = "lower")
}
\arguments{
  \item{dist}{
	contains the distribution from \code{link{visualize.distributions}}.
}
  \item{stat}{
 a statistic to obtain the probability from. When using the "bounded" condition, you must supply the parameter as \code{stat = c(lower_bound, upper_bound)}. Otherwise, a simple \code{stat = desired_point} will suffice.}
  \item{params}{
A list that must contain the necessary parameters for each distribution. For example, \code{params = list(mu = 1, sd = 1)} would be for a normal distribution with mean 1 and standard deviation 1. If you are not aware of the parameters for the distribution, consider using the \code{visualize.}\emph{dist_name} functions listed under the "See Also" section.
}
  \item{section}{
Select how you want the statistic(s) evaluated via \code{section=} either \code{"lower"},\code{"bounded"}, \code{"upper"}, or\code{"tails"}. 
}
}

\author{
James Balamuta
}

\seealso{
\code{\link{visualize.it}}, \code{\link{visualize.beta}}, \code{\link{visualize.chisq}}, \code{\link{visualize.exp}}, \code{\link{visualize.gamma}},  \code{\link{visualize.norm}}, \code{\link{visualize.unif}}, \code{\link{visualize.cauchy}}*, \code{\link{visualize.f}}*, \code{\link{visualize.lnorm}}*, \code{\link{visualize.t}}*, \code{\link{visualize.wilcox}}*, \code{\link{visualize.logis}}*. \cr
* = added in v2.0. 
}
\examples{
#Function does not have dist look up, must go through visualize.it
visualize.it(dist='norm', stat = c(0,1), params = list(mu = 1, sd = 1), section = "bounded")
}
\keyword{visualize}