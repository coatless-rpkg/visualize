\name{visualize.it}
\alias{visualize.it}
\title{
Visualize's Processing Function
}
\description{
Acts as a director of traffic and first line of error handling regarding submitted visualization requests. This function should only be used by advanced users.
}
\usage{
visualize.it(dist = "norm", stat = c(0, 1), params = list(mu = 0, sd = 1), section = "lower", strict = c(0,1))
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{dist}{
	a string that should be contain a supported  probability distributions name in R. Supported continuous distributions: \code{"beta"}, \code{"chisq"}, \code{"exp"}, \code{"gamma"}, \code{"norm"}, and \code{"unif"}. Supported discrete distributions: \code{"binom"}, \code{"geom"}, \code{"hyper"}, \code{"nbinom"}, and \code{"pois"}.
}
  \item{stat}{
	a statistic to obtain the probability from. When using the "bounded" condition, you must supply the parameter as \code{stat = c(lower_bound, upper_bound)}. Otherwise, a simple \code{stat = desired_point} will suffice.
}
  \item{params}{
	A list that must contain the necessary parameters for each distribution. For example, \code{params = list(mu = 1, sd = 1)} would be for a normal distribution with mean 1 and standard deviation 1. If you are not aware of the parameters for the distribution, consider using the \code{visualize.dist} functions listed under the "See Also" section.
}
  \item{section}{
	Select how you want the statistic(s) evaluated via \code{section=} either \code{"lower"},\code{"bounded"}, or \code{"upper"}. 
}
  \item{strict}{
  Determines whether the probability will be generated as a strict (<, >) or equal to (<=, >=) inequality. \code{strict=} requires either values = 0 or =FALSE for strict OR values =1 or =TRUE for equal to. For bounded condition use: \code{strict=c(0,1)} or \code{strict=c(FALSE,TRUE)}. 
}
}

\value{
Returns a plot of the distribution according to the conditions supplied.
}
\references{
 http://cran.r-project.org/web/views/Distributions.html
}
\author{
James Balamuta
}

\seealso{
  \code{\link{visualize.beta}}, \code{\link{visualize.chisq}}, \code{\link{visualize.exp}}, \code{\link{visualize.gamma}},  \code{\link{visualize.norm}}, \code{\link{visualize.unif}},  \code{\link{visualize.binom}}, \code{\link{visualize.geom}},  \code{\link{visualize.hyper}}, \code{\link{visualize.nbinom}},  \code{\link{visualize.pois}}.
}
\examples{
	# Defaults to lower tail evaluation
visualize.it(dist = 'norm', stat = 1, list(mu = 3 , sd = 2), section = "upper")

#Set to evaluate the upper tail.
visualize.it(dist = 'norm', stat = 1, list(mu=3,sd=2),section="upper")

#Set to shade inbetween a bounded region.
visualize.it(dist = 'norm', stat = c(-1,1), list(mu=0,sd=1), section="bounded")

#Bare minimum of information results in a bounded evaluation. 
visualize.it(dist = 'norm', params = list(mu=2,sd=1))

#Gamma distribution evaluated at upper tail.
visualize.it(dist = 'gamma', stat = 2, params = list(alpha=2,beta=1),section="upper")

#Binomial distribution evaluated at lower tail.
visualize.it('binom', stat = 2, params = list(n=4,p=.5))

}
\keyword{visualize}
