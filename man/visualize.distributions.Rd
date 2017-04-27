\name{visualize.distributions}
\alias{visualize.distributions}
\title{
Visualize's Supported Distributions
}
\description{
All of visualize's supported distributions with their density, probability, and quantile functions. In addition, mean and variance functions are present. Other descriptors also exist and are documented below.
}
\format{
	  Distributions are loaded with the following format:
  \tabular{ll}{
    type:\tab specify either \code{"continuous"} or \code{"discrete"}\cr
     \tab to direct the query to the right graph handler.\cr
    name:\tab specify the name of the distribution. \cr
    \tab In example, "Poisson Distribution." \cr
    \tab This is used in the main graph title. \cr
    
    variable:\tab specify the variable in probability statement.\cr
    \tab In example, P(z < 5). \cr
    \tab This is used in the probability subtitle. \cr
    
    varsymbols:\tab specify the variable symbols for distribution.\cr
    \tab In example, mu = 1 sd = 2. \cr
    \tab This is used in the distribution subtitle. \cr
    
    params:\tab  specify the amount of params required for distribution.\cr
    \tab This is used in the first error handling check to ensure\cr
    \tab the correct number of params is supplied.\cr

    init(params, ...):\tab Function that generates the mean and variance of a distribution.\cr
    
    density(x,params, noncent, lowtail, problog, ...):\tab   	Function that provides the density value using vectors of the \cr
    \tab quantiles from the distribution. \cr
    \tab This serves as a wrapper for \bold{d}\emph{distr_name}. \cr
    
    probability(x,params, noncent, lowtail, problog, ...) \tab Function that provides the probability value \cr
    \tab using vectors of quantiles from the distribution. \cr
    \tab This serves as a wrapper for \bold{p}\emph{distr_name}.\cr
    
    quantile(,params, noncent, lowtail, problog, ...) \tab Function that provides the quantile value \cr
    \tab using vectors of probabilities from the distribution. \cr
    \tab This serves as a wrapper for \bold{q}\emph{distr_name}.\cr
  
	}

}
\author{
James Balamuta
}
\examples{
visualize.distributions <- list(
  'beta' = list(
    type = "continuous",
    name = "Beta Distribution",
    variable = "b",
    params = 2,
    init  = function(params, ...) {
      shape1 = params[[1]]; shape2 = params[[2]]
      if(shape1 <= 0 || shape2 <= 0) stop("Error: Need alpha, beta  > 0")
      mean = shape1 / (shape1 + shape2)
      var = (shape1 * shape2)/((shape1 + shape2 + 1)*(shape1 + shape2)^2)
      c(mean, var)
    },
    density = function(x,params, noncent = 0, lowtail = TRUE, problog = FALSE, ...){
      if(params[[1]] <= 0 || params[[2]] <= 0) stop("Error: Need alpha, beta  > 0")
        dbeta(x,params[[1]], params[[2]], ncp = noncent, log = problog)
    },
    probability = function(x,params, noncent = 0, lowtail = TRUE, problog = FALSE, ...){
      if(params[[1]] <= 0 || params[[2]] <= 0) stop("Error: Need alpha, beta  > 0")
      pbeta(x,params[[1]], params[[2]], ncp = noncent, lower.tail = lowtail, log.p = problog)
    },
    quantile = function(x,params, noncent = 0, lowtail = TRUE, problog = FALSE, ...){
      if(params[[1]] <= 0 || params[[2]] <= 0) stop("Error: Need alpha, beta  > 0")
      qbeta(x,params[[1]], params[[2]], ncp = noncent, lower.tail = lowtail, log.p = problog)
    }
  )
)
}
\keyword{datasets}
