\name{visualize.lnorm}
\alias{visualize.lnorm}
\title{
Visualize Log Normal Distribution
}
\description{
Generates a plot of the Log Normal distribution with user specified parameters.
}
\usage{
visualize.lnorm(stat = 1, meanlog = 3, sdlog = 1, section = "lower")
}
\arguments{
  \item{stat}{
a statistic to obtain the probability from. When using the "bounded" condition, you must supply the parameter as \code{stat = c(lower_bound, upper_bound)}. Otherwise, a simple \code{stat = desired_point} will suffice.
}
  \item{meanlog}{
Mean of the distribution
}
  \item{sdlog}{
Standard deviation of the distribution
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
  \code{\link{visualize.it}}, \code{\link{dlnorm}}.
}
\examples{
#Evaluates lower tail.
visualize.lnorm(stat = 1, meanlog = 3, sdlog = 1, section = "lower") 

#Evaluates bounded region.
visualize.lnorm(stat = c(3,5), meanlog = 3, sdlog = 3, section = "bounded") 

#Evaluates upper tail.
visualize.lnorm(stat = 1, meanlog = 3, sdlog = 1, section = "upper") 
}
\keyword{ visualize }