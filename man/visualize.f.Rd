\name{visualize.f}
\alias{visualize.f}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Visualize F distribution
}
\description{
Generates a plot of the F distribution with user specified parameters.
}
\usage{
visualize.f(stat = 1, df1 = 5, df2 = 4, section = "lower")
}
\arguments{
  \item{stat}{
a statistic to obtain the probability from. When using the "bounded" condition, you must supply the parameter as \code{stat = c(lower_bound, upper_bound)}. Otherwise, a simple \code{stat = desired_point} will suffice.
}
  \item{df1}{
First Degrees of Freedom
}
  \item{df2}{
Second Degrees of Freedom
}
  \item{section}{
Select how you want the statistic(s) evaluated via \code{section=} either \code{"lower"},\code{"bounded"}, \code{"upper"}, or\code{"tails"}. 
}
}
\value{
Returns a plot of the distribution according to the conditions supplied.
}
\author{
James Balamuta
}
\seealso{
  \code{\link{visualize.it}}, \code{\link{df}}.
}
\examples{
#Evaluates lower tail.
visualize.f(stat = 1, df1 = 5, df2 = 4, section = "lower") 

#Evaluates bounded region.
visualize.f(stat = c(3,5), df1 = 6, df2 = 3, section = "bounded") 

#Evaluates upper tail.
visualize.f(stat = 1, df1 = 5, df2 = 4, section = "upper") 
}
\keyword{ visualize }