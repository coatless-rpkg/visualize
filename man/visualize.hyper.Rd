\name{visualize.hyper}
\alias{visualize.hyper}
\title{
Visualize Hypergeometric Distribution
}
\description{
Generates a plot of the Hypergeometric distribution with user specified parameters.
}
\usage{
visualize.hyper(stat = 1, m = 5, n = 4, k = 2, section = "lower", strict = FALSE)
}
\arguments{
  \item{stat}{
   a statistic to obtain the probability from. When using the "bounded" condition, you must supply the parameter as \code{stat = c(lower_bound, upper_bound)}. Otherwise, a simple \code{stat = desired_point} will suffice.
}
  \item{m}{
	\code{m} white balls. \code{m} must be greater than 0.
}
  \item{n}{
	\code{n} black balls. \code{n} must be greater than 0.
}
  \item{k}{
	draw \code{k} balls without replacement. 
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
	\code{\link{visualize.it}} , \code{\link{dhyper}}. 
}
\examples{
#Evaluates lower tail.
visualize.hyper(stat = 1, m=4, n=5, k=3, section = "lower", strict = 0) 

#Evaluates bounded region.
visualize.hyper(stat = c(2,4), m=14, n=5, k=2, section = "bounded", strict = c(0,1))

#Evaluates upper tail.
visualize.hyper(stat = 1, m=4, n=5, k=3, section = "upper", strict = 1)
}
\keyword{visualize}