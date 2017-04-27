\name{visualize.binom}
\alias{visualize.binom}
\title{
Visualize Binomial Distribution
}
\description{
Generates a plot of the Binomial distribution with user specified parameters.
}
\usage{
visualize.binom(stat = 1, size = 3, prob = 0.5, section = "lower", strict = FALSE)
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
	\code{\link{visualize.it}} , \code{\link{dbinom}}. 
}
\examples{
#Evaluates lower tail with equal to inequality.
visualize.binom(stat = 1, size = 3, prob = 0.5, section = "lower", strict = FALSE) 

#Evaluates bounded region with lower bound equal to and upper bound strict inequality.
visualize.binom(stat = c(1,2), size = 5, prob = 0.35, section = "bounded", strict = c(0,1))

#Evaluates upper tail with strict inequality.
visualize.binom(stat = 1, size = 3, prob = 0.5, section = "upper", strict = TRUE)
}
\keyword{ visualize }