\name{visualize.logis}
\alias{visualize.logis}
\title{
Visualize Logistic distribution
}
\description{
Generates a plot of the Logistic distribution with user specified parameters.
}
\usage{
visualize.logis(stat = 1, location = 3, scale = 1, section = "lower")
}
\arguments{
  \item{stat}{
a statistic to obtain the probability from. When using the "bounded" condition, you must supply the parameter as \code{stat = c(lower_bound, upper_bound)}. Otherwise, a simple \code{stat = desired_point} will suffice.
}
  \item{location}{
Location of the distribution.
}
  \item{scale}{
Scale of the distribution.
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
  \code{\link{visualize.it}}, \code{\link{dlogis}}.
}
\examples{
#Evaluates lower tail.
visualize.logis(stat = 1, location = 4, scale = 2, section = "lower") 

#Evaluates bounded region.
visualize.logis(stat = c(3,5), location = 4, scale = 2, section = "bounded") 

#Evaluates upper tail.
visualize.logis(stat = 1, location = 4, scale = 2, section = "upper") 
}
\keyword{ visualize }