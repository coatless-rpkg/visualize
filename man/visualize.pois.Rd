\name{visualize.pois}
\alias{visualize.pois}
\title{
Visualize Poisson Distribution
}
\description{
Generates a plot of the Poisson distribution with user specified parameters.
}
\usage{
visualize.pois(stat = 1, lambda = 3.5, section = "lower")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{stat}{
   a statistic to obtain the probability from. When using the "bounded" condition, you must supply the parameter as \code{stat = c(lower_bound, upper_bound)}. Otherwise, a simple \code{stat = desired_point} will suffice.
}
  \item{lambda}{
	lambda value of the Poisson Distribution.
}
  \item{section}{
	Select how you want the statistic(s) evaluated via \code{section=} either \code{"lower"},\code{"bounded"}, or \code{"upper"}. 
}
}
\details{
	Note: This package only evaluates on inequalities that are not strict - i.e. (greater than or equal to) or (less than or equal to).
}

\author{
James Balamuta
}

\seealso{
	\code{\link{visualize.it}} , \code{\link{dpois}}. 
}
\examples{
#Evaluates lower tail.
visualize.pois(stat = 1, lambda = 2, section = "lower") 

#Evaluates bounded region.
visualize.pois(stat = c(1,3), lambda = 3, section = "bounded")

#Evaluates upper tail.
visualize.pois(stat = 1, lambda = 2, section = "upper")
}
\keyword{visualize}