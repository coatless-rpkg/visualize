# Visualize's Supported Distributions

All of visualize's supported distributions with their density,
probability, and quantile functions. In addition, mean and variance
functions are present. Other descriptors also exist and are documented
below.

## Usage

``` r
visualize.distributions
```

## Format

Distributions are loaded with the following format:

|  |  |
|----|----|
| type: | specify either `"continuous"` or `"discrete"` |
|  | to direct the query to the right graph handler. |
| name: | specify the name of the distribution. |
|  | In example, "Poisson Distribution." |
|  | This is used in the main graph title. |
| variable: | specify the variable in probability statement. |
|  | In example, P(z \< 5). |
|  | This is used in the probability subtitle. |
| varsymbols: | specify the variable symbols for distribution. |
|  | In example, mu = 1 sd = 2. |
|  | This is used in the distribution subtitle. |
| params: | specify the amount of params required for distribution. |
|  | This is used in the first error handling check to ensure |
|  | the correct number of params is supplied. |
| init(params, ...): | Function that generates the mean and variance |
|  | of a distribution. |
| density(x, params, ncp = 0, lower.tail = TRUE, log = FALSE, ...): | Function that provides the density value using vectors of the |
|  | quantiles from the distribution. |
|  | This serves as a wrapper for **d***distr_name*. |
| probability(x, params, ncp = 0, lower.tail = TRUE, log.p = FALSE, ...) | Function that provides the probability value |
|  | using vectors of quantiles from the distribution. |
|  | This serves as a wrapper for **p***distr_name*. |
| quantile(x, params, ncp = 0, lower.tail = TRUE, log.p = FALSE, ...) | Function that provides the quantile value |
|  | using vectors of probabilities from the distribution. |
|  | This serves as a wrapper for **q***distr_name*. |

The distributions currently available to use are:

|                |        |                   |        |
|----------------|--------|-------------------|--------|
| Distribution   | r Name | Distribution      | r Name |
| Beta           | beta   | Lognormal\*       | lnorm  |
| Binomial       | binom  | Negative Binomial | nbinom |
| Cauchy\*       | cauchy | Normal            | norm   |
| Chisquare      | chisq  | Poisson           | pois   |
| Exponential    | exp    | Student t\*       | t      |
| F\*            | f      | Uniform           | unif   |
| Gamma          | gamma  | Geometric         | geom   |
| Hypergeometric | hyper  | Wilcoxon\*        | wilcox |
| Logistic\*     | logis  |                   |        |

- denotes the distribution was added in v2.0.

## Author

James Balamuta

## Examples

``` r

visualize.distributions = list(
  'beta' = list(
    type = "continuous",
    name = "Beta Distribution",
    variable = "b",
    varsymbols = c("\u03B1","\u03B2"),
    params = 2,
    init  = function(params, ...) {
      shape1 = params[[1]]; shape2 = params[[2]]
      if(shape1 <= 0 || shape2 <= 0) stop("Error: Need alpha, beta  > 0")
      mean = shape1 / (shape1 + shape2)
      var = (shape1 * shape2)/((shape1 + shape2 + 1)*(shape1 + shape2)^2)
      c(mean, var)
    },
    density = function(x,params, ncp = 0, lower.tail = TRUE, log = FALSE, ...){
      if(params[[1]] <= 0 || params[[2]] <= 0) stop("Error: Need alpha, beta  > 0")
        dbeta(x,params[[1]], params[[2]], ncp = ncp, log = log)
    },
    probability = function(q,params, ncp = 0, lower.tail = TRUE, log.p = FALSE, ...){
      if(params[[1]] <= 0 || params[[2]] <= 0) stop("Error: Need alpha, beta  > 0")
      pbeta(q,params[[1]], params[[2]], ncp = ncp, lower.tail = lower.tail, log.p = log.p)
    },
    quantile = function(p,params, ncp = 0, lower.tail = TRUE, log.p = FALSE, ...){
      if(params[[1]] <= 0 || params[[2]] <= 0) stop("Error: Need alpha, beta  > 0")
      qbeta(p,params[[1]], params[[2]], ncp = ncp, lower.tail = lower.tail, log.p = log.p)
    }
  )
)
```
