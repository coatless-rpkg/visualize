% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/visualize.cauchy.R
\name{visualize.cauchy}
\alias{visualize.cauchy}
\title{Visualize Cauchy Distribution}
\usage{
visualize.cauchy(stat = 1, location = 2, scale = 1, section = "lower")
}
\arguments{
\item{stat}{a statistic to obtain the probability from. When using the
"bounded" condition, you must supply the parameter as \code{stat = c(lower_bound, upper_bound)}. Otherwise, a simple \code{stat = desired_point} will suffice.}

\item{location}{location parameter}

\item{scale}{scale parameter}

\item{section}{Select how you want the statistic(s) evaluated via
\verb{section=} either \code{"lower"},\code{"bounded"}, \code{"upper"},
or\code{"tails"}.}
}
\value{
Returns a plot of the distribution according to the conditions
supplied.
}
\description{
Generates a plot of the Cauchy distribution with user specified parameters.
}
\examples{

# Evaluates lower tail.
visualize.cauchy(stat = 1, location = 4, scale = 2, section = "lower") 

# Evaluates bounded region.
visualize.cauchy(stat = c(3,5), location = 5, scale = 3, section = "bounded") 

# Evaluates upper tail.
visualize.cauchy(stat = 1, location = 4, scale = 2, section = "upper") 

}
\seealso{
\code{\link[=visualize.it]{visualize.it()}}, \code{\link[=dcauchy]{dcauchy()}}.
}
\author{
James Balamuta
}
\keyword{continuous-distribution}
