\name{visualize.geom}
\alias{visualize.geom}

\title{
Visualize Geometric Distribution
}
\description{
Generates a plot of the Geometric distribution with user specified parameters.
}
\usage{
visualize.geom(stat = 1, prob = 0.3, section = "lower")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{stat}{
   a statistic to obtain the probability from. When using the "bounded" condition, you must supply the parameter as \code{stat = c(lower_bound, upper_bound)}. Otherwise, a simple \code{stat = desired_point} will suffice.
}
  \item{prob}{
	probability of picking object.	
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
	\code{\link{visualize.it}} , \code{\link{dgeom}}. 
}
\examples{
#Evaluates lower tail.
visualize.geom(stat = 1, prob = 0.5, section = "lower") 

#Evaluates bounded region.
visualize.geom(stat = c(1,3), prob = 0.35, section = "bounded")

#Evaluates upper tail.
visualize.geom(stat = 1, prob = 0.5, section = "upper")
}
\keyword{visualize}