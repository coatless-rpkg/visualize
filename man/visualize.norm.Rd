\name{visualize.norm}
\alias{visualize.norm}
\title{
Visualize Normal Distribution
}
\description{
Generates a plot of the Normal distribution with user specified parameters.
}
\usage{
visualize.norm(stat = 1, mu = 0, sd = 1, section = "lower")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{stat}{
   a statistic to obtain the probability from. When using the "bounded" condition, you must supply the parameter as \code{stat = c(lower_bound, upper_bound)}. Otherwise, a simple \code{stat = desired_point} will suffice.
}
  \item{mu}{
  	mean of the Normal Distribution.
}
  \item{sd}{
  	standard deviation of the Normal Distribution.
}
  \item{section}{
Select how you want the statistic(s) evaluated via \code{section=} either \code{"lower"},\code{"bounded"}, \code{"upper"}, or \code{"tails"}. 
}
}
\seealso{
	\code{\link{visualize.it}} , \code{\link{dnorm}}. 
}
\examples{
#Evaluates lower tail.
visualize.norm(stat = 1, mu = 4, sd = 5, section = "lower") 

#Evaluates bounded region.
visualize.norm(stat = c(3,6), mu = 5, sd = 3, section = "bounded")

#Evaluates upper tail.
visualize.norm(stat = 1, mu = 3, sd = 2, section = "upper")
}
\keyword{visualize}
