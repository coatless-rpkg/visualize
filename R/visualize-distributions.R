#' Visualize's Supported Distributions
#' 
#' All of visualize's supported distributions with their density, probability,
#' and quantile functions. In addition, mean and variance functions are
#' present. Other descriptors also exist and are documented below.
#' 
#' @format 
#' Distributions are loaded with the following format: 
#' \tabular{ll}{
#'     type: \tab specify either `"continuous"` or `"discrete"` \cr
#'           \tab to direct the query to the right graph handler. \cr
#'     name: \tab specify the name of the distribution. \cr 
#'           \tab In example, "Poisson Distribution."   \cr 
#'           \tab This is used in the main graph title. \cr
#' variable: \tab specify the variable in probability statement.\cr
#'           \tab In example, P(z < 5). \cr
#'           \tab This is used in the probability subtitle. \cr
#' varsymbols:\tab specify the variable symbols for distribution.\cr
#'            \tab In example, mu = 1 sd = 2. \cr
#'            \tab This is used in the distribution subtitle. \cr
#' params:    \tab specify the amount of params required for distribution.\cr
#'            \tab This is used in the first error handling check to ensure\cr
#'            \tab the correct number of params is supplied.\cr
#' init(params, ...):\tab Function that generates the mean and variance \cr
#'                    \tab of a distribution.\cr
#' 
#' density(x, params, ncp = 0, lower.tail = TRUE, log = FALSE, ...):\tab
#' Function that provides the density value using vectors of the \cr 
#' \tab quantiles from the distribution. \cr 
#' \tab This serves as a wrapper for **d***distr_name*. \cr
#' 
#' probability(x, params, ncp = 0, lower.tail = TRUE, log.p = FALSE, ...) \tab
#' Function that provides the probability value \cr 
#' \tab using vectors of quantiles from the distribution. \cr 
#' \tab This serves as a wrapper for **p***distr_name*.\cr
#' 
#' quantile(x, params, ncp = 0, lower.tail = TRUE, log.p = FALSE, ...) \tab
#' Function that provides the quantile value \cr
#' \tab using vectors of probabilities from the distribution. \cr
#' \tab This serves as a wrapper for **q***distr_name*.\cr
#' 
#' }
#' 
#' The distributions currently available to use are: \tabular{llll}{
#' Distribution \tab r Name \tab Distribution \tab r Name \cr
#' Beta \tab beta \tab Lognormal* \tab lnorm \cr
#' Binomial \tab binom \tab Negative Binomial \tab nbinom \cr
#' Cauchy* \tab cauchy \tab Normal \tab norm \cr
#' Chisquare \tab chisq \tab Poisson \tab pois\cr
#' Exponential \tab exp \tab Student t* \tab t\cr
#' F* \tab f \tab Uniform \tab unif\cr
#' Gamma \tab gamma \tab Geometric \tab geom \cr
#' Hypergeometric \tab hyper \tab Wilcoxon* \tab wilcox\cr
#' Logistic* \tab logis\tab \tab \cr }
#' * denotes the distribution was added in v2.0.
#' @author James Balamuta
#' @keywords datasets internal
#' @examples
#' visualize.distributions = list(
#'   'beta' = list(
#'     type = "continuous",
#'     name = "Beta Distribution",
#'     variable = "b",
#'     varsymbols = c("\u03B1","\u03B2"),
#'     params = 2,
#'     init  = function(params, ...) {
#'       shape1 = params[[1]]; shape2 = params[[2]]
#'       if(shape1 <= 0 || shape2 <= 0) stop("Error: Need alpha, beta  > 0")
#'       mean = shape1 / (shape1 + shape2)
#'       var = (shape1 * shape2)/((shape1 + shape2 + 1)*(shape1 + shape2)^2)
#'       c(mean, var)
#'     },
#'     density = function(x,params, ncp = 0, lower.tail = TRUE, log = FALSE, ...){
#'       if(params[[1]] <= 0 || params[[2]] <= 0) stop("Error: Need alpha, beta  > 0")
#'         dbeta(x,params[[1]], params[[2]], ncp = ncp, log = log)
#'     },
#'     probability = function(q,params, ncp = 0, lower.tail = TRUE, log.p = FALSE, ...){
#'       if(params[[1]] <= 0 || params[[2]] <= 0) stop("Error: Need alpha, beta  > 0")
#'       pbeta(q,params[[1]], params[[2]], ncp = ncp, lower.tail = lower.tail, log.p = log.p)
#'     },
#'     quantile = function(p,params, ncp = 0, lower.tail = TRUE, log.p = FALSE, ...){
#'       if(params[[1]] <= 0 || params[[2]] <= 0) stop("Error: Need alpha, beta  > 0")
#'       qbeta(p,params[[1]], params[[2]], ncp = ncp, lower.tail = lower.tail, log.p = log.p)
#'     }
#'   )
#' )
#' 
"visualize.distributions"
