\name{visualize.chisq}
\alias{visualize.chisq}

\title{
Visualize Chi-squared Distribution
}

\description{
Generates a plot of the Chi-squared distribution with user specified parameters.
}

\usage{
visualize.chisq(stat = 1, df = 3, section = "lower")
}

\arguments{
  \item{stat}{
 a statistic to obtain the probability from. When using the "bounded" condition, you must supply the parameter as \code{stat = c(lower_bound, upper_bound)}. Otherwise, a simple \code{stat = desired_point} will suffice.
}
  \item{df}{
	degrees of freedom of Chi-squared distribution.
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
	\code{\link{visualize.it}}, \code{\link{dchisq}}.
}
\examples{
#Evaluates lower tail.
visualize.chisq(stat = 1, df = 3, section = "lower")
#Evaluates bounded region.
visualize.chisq(stat = c(1,2), df = 6, section = "bounded")
#Evaluates upper tail.
visualize.chisq(stat = 1, df = 3, section = "upper")

}

\keyword{visualize}