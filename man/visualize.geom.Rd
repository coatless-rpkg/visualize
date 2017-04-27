\name{visualize.geom}
\alias{visualize.geom}

\title{
Visualize Geometric Distribution
}
\description{
Generates a plot of the Geometric distribution with user specified parameters.
}
\usage{
visualize.geom(stat = 1, prob = 0.3, section = "lower", strict = FALSE)
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
  \item{strict}{
  Determines whether the probability will be generated as a strict (<, >) or equal to (<=, >=) inequality. \code{strict=} requires either values = 0 or =FALSE for equal to OR values =1 or =TRUE for strict. For bounded condition use: \code{strict=c(0,1)} or \code{strict=c(FALSE,TRUE)}. 
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
visualize.geom(stat = 1, prob = 0.5, section = "lower", strict = FALSE) 

#Evaluates bounded region.
visualize.geom(stat = c(1,3), prob = 0.35, section = "bounded", strict = c(0,1))

#Evaluates upper tail.
visualize.geom(stat = 1, prob = 0.5, section = "upper", strict = 1)
}
\keyword{visualize}