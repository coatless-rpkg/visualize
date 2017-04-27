\name{visualize.t}
\alias{visualize.t}
\title{
Visualize Student's t distribution
}
\description{
Generates a plot of the Student's t distribution with user specified parameters.
}
\usage{
visualize.t(stat = 1, df = 3, section = "lower")
}
\arguments{
  \item{stat}{
a statistic to obtain the probability from. When using the "bounded" condition, you must supply the parameter as \code{stat = c(lower_bound, upper_bound)}. Otherwise, a simple \code{stat = desired_point} will suffice.
}
  \item{df}{
Degrees of freedom
}
  \item{section}{
Select how you want the statistic(s) evaluated via \code{section=} either \code{"lower"},\code{"bounded"}, \code{"upper"}, or \code{"tails"}. 
}
}
\value{
Returns a plot of the distribution according to the conditions supplied.
}
\author{
James Balamuta
}
\seealso{
  \code{\link{visualize.it}}, \code{\link{dt}}.
}
\examples{
#Evaluates lower tail.
visualize.t(stat = 1, df = 4, section = "lower") 

#Evaluates bounded region.
visualize.t(stat = c(3,5), df = 6, section = "bounded") 

#Evaluates upper tail.
visualize.t(stat = 1, df = 4, section = "upper") 
}
\keyword{ visualize }
