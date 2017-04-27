\name{visualize.exp}
\alias{visualize.exp}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Visualize Exponential Distribution
}
\description{
Generates a plot of the Exponential distribution with user specified parameters.
}
\usage{
visualize.exp(stat = 1, theta = 1, section = "lower")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{stat}{
 a statistic to obtain the probability from. When using the "bounded" condition, you must supply the parameter as \code{stat = c(lower_bound, upper_bound)}. Otherwise, a simple \code{stat = desired_point} will suffice.
}
  \item{theta}{
	vector of rates
}
  \item{section}{
Select how you want the statistic(s) evaluated via \code{section=} either \code{"lower"},\code{"bounded"}, or \code{"upper"}. 
}
}
\value{
Returns a plot of the distribution according to the conditions supplied.
}
\author{
James Balamuta
}
\seealso{
	\code{\link{visualize.it}}, \code{\link{dexp}}.
}
\examples{
#Evaluates lower tail.
visualize.exp(stat = .5, theta = 3, section = "lower")
#Evaluates bounded region.
visualize.exp(stat = c(1,2), theta = 3, section = "bounded")
#Evaluates upper tail.
visualize.exp(stat = .5, theta = 3, section = "upper")
}
\keyword{visualize}