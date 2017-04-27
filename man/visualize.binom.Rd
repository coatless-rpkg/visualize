\name{visualize.binom}
\alias{visualize.binom}
\title{
Visualize Binomial Distribution
}
\description{
Generates a plot of the Binomial distribution with user specified parameters.
}
\usage{
visualize.binom(stat = 1, size = 3, prob = 0.5, section = "lower")
}

\arguments{
  \item{stat}{
   a statistic to obtain the probability from. When using the "bounded" condition, you must supply the parameter as \code{stat = c(lower_bound, upper_bound)}. Otherwise, a simple \code{stat = desired_point} will suffice.
}
  \item{size}{
   size of sample.
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
	\code{\link{visualize.it}} , \code{\link{dbinom}}. 
}
\examples{
#Evaluates lower tail.
visualize.binom(stat = 1, size = 3, prob = 0.5, section = "lower") 

#Evaluates bounded region.
visualize.binom(stat = c(1,2), size = 5, prob = 0.35, section = "bounded")

#Evaluates upper tail.
visualize.binom(stat = 1, size = 3, prob = 0.5, section = "upper")
}
\keyword{ visualize }