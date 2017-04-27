\name{visualize.gamma}
\alias{visualize.gamma}

\title{
Visualize Gamma Distribution
}
\description{
Generates a plot of the Gamma distribution with user specified parameters.
}
\usage{
visualize.gamma(stat = 1, alpha = 1, theta = 1, section = "lower")
}

\arguments{
  \item{stat}{
a statistic to obtain the probability from. When using the "bounded" condition, you must supply the parameter as \code{stat = c(lower_bound, upper_bound)}. Otherwise, a simple \code{stat = desired_point} will suffice.
}
  \item{alpha}{
	\code{alpha} is considered to be \emph{shape} by R's implementation of the gamma distribution. \code{alpha} must be greater than 0.
}
  \item{theta}{
	\code{theta} is considered to be \emph{rate} by R's implementation of the gamma distribution. \code{theta} must be greater than 0.
}
  \item{section}{
Select how you want the statistic(s) evaluated via \code{section=} either \code{"lower"},\code{"bounded"}, \code{"upper"}, or \code{"tails"}. 
}
}

\author{
James Balamuta
}

\seealso{
	\code{\link{visualize.it}}, \code{\link{dgamma}}.
}
\examples{
#Evaluate lower tail.
visualize.gamma(stat = 1, alpha = 3, theta = 1, section = "lower") 

#Evaluate bounded section.
visualize.gamma(stat = c(0.75,1), alpha = 3, theta = 1, section = "bounded") 

#Evaluate upper tail.
visualize.gamma(stat = 1, alpha = 3, theta = 1, section = "upper") 

}
\keyword{ visualize }