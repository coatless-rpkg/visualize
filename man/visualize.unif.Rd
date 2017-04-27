\name{visualize.unif}
\alias{visualize.unif}
\title{
Visualize Uniform Distribution
}
\description{
Generates a plot of the Uniform distribution with user specified parameters.
}
\usage{
visualize.unif(stat = 1, a = 0, b = 1, section = "lower")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{stat}{
   a statistic to obtain the probability from. When using the "bounded" condition, you must supply the parameter as \code{stat = c(lower_bound, upper_bound)}. Otherwise, a simple \code{stat = desired_point} will suffice.
}
  \item{a}{
	starting point. Note: \code{a}<\code{b}
}
  \item{b}{
	end point. Note: \code{b} > \code{a}
}
  \item{section}{
Select how you want the statistic(s) evaluated via \code{section=} either \code{"lower"},\code{"bounded"}, \code{"upper"}, or\code{"tails"}. 
}
}
\author{
James Balamuta
}

\seealso{
	\code{\link{visualize.it}} , \code{\link{dunif}}. 
}
\examples{
#Evaluates lower tail.
visualize.unif(stat = 8.75, a = 7, b = 10, section = "lower") 

#Evaluates bounded region.
visualize.unif(stat = c(3,6), a = 1, b = 7, section = "bounded")

#Evaluates upper tail.
visualize.unif(stat = 2, a = 1, b = 5, section = "upper")
}
\keyword{visualize}