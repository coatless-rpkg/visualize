\name{visualize.nbinom}
\alias{visualize.nbinom}
\title{
Visualize Negative Binomial Distribution
}
\description{
Generates a plot of the Negative Binomial distribution with user specified parameters.
}
\usage{
visualize.nbinom(stat = 1, size = 6, prob = 0.5, section = "lower")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{stat}{
   a statistic to obtain the probability from. When using the "bounded" condition, you must supply the parameter as \code{stat = c(lower_bound, upper_bound)}. Otherwise, a simple \code{stat = desired_point} will suffice.
}
  \item{size}{
	number of objects.
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
	\code{\link{visualize.it}} , \code{\link{dnbinom}}. 
}
\examples{
#Evaluates lower tail.
visualize.nbinom(stat = 1, size = 5, prob = 0.5, section = "lower") 

#Evaluates bounded region.
visualize.nbinom(stat = c(1,3), size = 10, prob = 0.35, section = "bounded")

#Evaluates upper tail.
visualize.nbinom(stat = 1, size = 5, prob = 0.5, section = "upper")
}
\keyword{visualize}