# Creates the visualize.distribution object
# Allows for object-oriented selection of distributions.

visualize.distributions = list(
  #Start Continuous Distributions
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
    density = function(x,params, noncent = 0, lowtail = TRUE, problog = FALSE, ...){
      if(params[[1]] <= 0 || params[[2]] <= 0) stop("Error: Need alpha, beta  > 0")
        dbeta(x,params[[1]], params[[2]], ncp = noncent, log = problog)
    },
    probability = function(q,params, noncent = 0, lowtail = TRUE, problog = FALSE, ...){
      if(params[[1]] <= 0 || params[[2]] <= 0) stop("Error: Need alpha, beta  > 0")
      pbeta(q,params[[1]], params[[2]], ncp = noncent, lower.tail = lowtail, log.p = problog)
    },
    quantile = function(p,params, noncent = 0, lowtail = TRUE, problog = FALSE, ...){
      if(params[[1]] <= 0 || params[[2]] <= 0) stop("Error: Need alpha, beta  > 0")
      qbeta(p,params[[1]], params[[2]], ncp = noncent, lower.tail = lowtail, log.p = problog)
    }
  ),
  chisq = list(
    type = "continuous",
    name  = "Chi-square Distribution",
    variable = "\u03A7\u00B2",
    varsymbols = c("r"),
    params = 1,
    init  = function(params, ...) {
      r = params[[1]]
      if(r <= 0) stop("Error: Need degree freedom > 0")
      mean = r
      var = 2*r
      c(mean, var)
    },
    density = function(x, params, noncent = 0, problog = FALSE, ...) {
      if(params[[1]] <= 0) stop("Error: Need degree freedom > 0")
      dchisq(x, df = params[[1]], ncp = noncent, log = problog)
    },
    probability = function(q, params, noncent = 0, lowtail = TRUE, problog = FALSE, ...) {
      if(params[[1]] <= 0) stop("Error: Need degree freedom > 0")
      pchisq(q, df = params[[1]], ncp = noncent, lower.tail = lowtail, log.p = problog)
    },
    quantile = function(p, params, lowtail = TRUE, problog = FALSE){
      if(params[[1]] <= 0) stop("Error: Need degree freedom > 0")
      pchisq(p, df = params[[1]], ncp = noncent, lower.tail = lowtail, log.p = problog)
    }
  ),
  
  exp = list(
    type = "continuous",
    name  = "Exponential Distribution",
    variable = "e",
    varsymbols = c("\u03B8"),
    params = 1,
    init  = function(params, ...) {
      theta = params[[1]]
      if(theta <= 0) stop("Error: Need theta > 0")
      mean = theta
      var = theta^2
      c(mean, var)
    },
    density = function(x, params, problog = FALSE, ...) {
      if(params[[1]] <= 0) stop("Error: Need theta > 0")
      dexp(x, rate = params[[1]], log = problog)
    },
    probability = function(q, params, lowtail = TRUE, problog = FALSE, ...) {
      if(params[[1]] <= 0) stop("Error: Need theta > 0")
      pexp(q, rate = params[[1]], lower.tail = lowtail, log.p = problog)
    },
    quantile = function(p, params, lowtail = TRUE, problog = FALSE){
      if(params[[1]] <= 0) stop("Error: Need theta > 0")
      qexp(p, rate = params[[1]], lower.tail = lowtail, log.p = problog)
    }
  ),
  
  gamma = list(
    type = "continuous",
    name  = "Gamma Distribution",
    variable = "g",
    varsymbols = c("\u03B1","\u03B8"),
    params = 2,
    init  = function(params,  ...) {
      alpha = params[[1]]; theta = params[[2]]
      if(alpha <= 0 || theta <= 0) stop("Error: Need alpha, theta > 0")
      mean = alpha*theta
      var = alpha*theta^2
      c(mean, var)
    },
    density = function(x, params, problog = FALSE, ...) {
      if(params[[1]] <= 0 || params[[2]] <= 0) stop("Error: Need alpha, theta > 0")
      dgamma(x, shape = params[[1]], rate = params[[2]], log = problog)
    },
    probability = function(q, params, lowtail = TRUE, problog = FALSE, ...) {
      if(params[[1]] <= 0 || params[[2]] <= 0) stop("Error: Need alpha, theta > 0")
      pgamma(q, shape = params[[1]], rate = params[[2]], lower.tail = lowtail, log.p = problog)
    },
    quantile = function(p, params, lowtail = TRUE, problog = FALSE, ...){
      if(params[[1]] <= 0 || params[[2]] <= 0) stop("Error: Need alpha, theta > 0")
      qgamma(p, shape = params[[1]], rate = params[[2]], lower.tail = lowtail, log.p = problog)
    }
  ),
  
  norm = list(
    type = "continuous",
    name  = "Normal Distribution",
    variable = "z",
    varsymbols = c("\u03BC","\u03C3"),
    params = 2,
    init  = function(params,  ...) {
      if(params[[2]] < 0) stop("Error: Need standard deviation >= 0")
      mean = params[[1]]
      var = (params[[2]])^2
      c(mean, var)
    },
    density = function(x, params, problog = FALSE, ...) {
      if(params[[2]] < 0) stop("Error: Need standard deviation >= 0")
      dnorm(x, mean = params[[1]], sd = params[[2]], log = problog)
    },
    probability = function(q, params, lowtail = TRUE, problog = FALSE, ...) {
      if(params[[2]] < 0) stop("Error: Need standard deviation >= 0")
      pnorm(q, mean = params[[1]], sd = params[[2]], lower.tail = lowtail, log.p = problog)
    },
    quantile = function(p, params, lowtail = TRUE, problog = FALSE, ...){
      if(params[[2]] < 0) stop("Error: Need standard deviation >= 0")
      qnorm(p, mean = params[[1]], sd = params[[2]], lower.tail = lowtail, log.p = problog)
    }
  ),
  
  unif = list(
    type = "continuous",
    name  = "Uniform Distribution",
    variable = "u",
    varsymbols = c("a","b"),
    params = 2,
    init  = function(params,  ...) {
      a = params[[1]]; b = params[[2]]
      if(a > b) stop("Error: a > b")
      mean = (a+b)/2
      var = (b-a)^2/12
      c(mean, var)
    },
    density = function(x, params, problog = FALSE, ...) {
      if(params[[1]] > params[[2]]) stop("Error: a > b")
      dunif(x, min = params[[1]], max = params[[2]], log = problog)
    },
    probability = function(q, params, lowtail = TRUE, problog = FALSE, ...) {
      if(params[[1]] > params[[2]]) stop("Error: a > b")
      punif(q, min = params[[1]], max = params[[2]], lower.tail = lowtail, log.p = problog)
    },
    quantile = function(p, params, lowtail = TRUE, problog = FALSE, ...){
      if(params[[1]] > params[[2]]) stop("Error: a > b")
      qunif(p, min = params[[1]], max = params[[2]], lower.tail = lowtail, log.p = problog)
    }
  ),
  #End Continuous Distributions
  
  #Discrete Distributions
  binom = list(
    type = "discrete",
    name  = "Binomial Distribution",
    variable = "b",
    varsymbols = c("n","p"),
    params = 2,
    init  = function(params,  ...) {
      n = params[[1]]; prob = params[[2]]
      if(n < 0 || (prob > 1 || prob < 0)) stop("Error: n < 0 or p is not between 0 < p < 1")
      mean = n*prob
      var = n*prob*(1-prob)
      c(mean, var)
    },
    density = function(x, params, problog = FALSE, ...) {
      if(params[[1]] < 0 || (params[[2]] > 1 || params[[2]] < 0)) stop("Error: n < 0 or p is not between 0 < p < 1")
      dbinom(x, size = params[[1]], prob = params[[2]], log = problog)
    },
    probability = function(q, params, lowtail = TRUE, problog = FALSE, ...) {
      if(params[[1]] < 0 || (params[[2]] > 1 || params[[2]] < 0)) stop("Error: n < 0 or p is not between 0 < p < 1")
      pbinom(q, size = params[[1]], prob = params[[2]], lower.tail = lowtail, log.p = problog)
    },
    quantile = function(p, params, lowtail = TRUE, problog = FALSE, ...){
      if(params[[1]] < 0 || (params[[2]] > 1 || params[[2]] < 0)) stop("Error: n < 0 or p is not between 0 < p < 1")
      qbinom(p, size = params[[1]], prob = params[[2]], lower.tail = lowtail, log.p = problog)
    }
  ),
  
  geom = list(
    type = "discrete",
    name  = "Geometric Distribution",
    variable = "g",
    varsymbols = c("p"),
    params = 1,
    init  = function(params,  ...) {
      prob = params[[1]]
      if( (prob > 1 || prob < 0) ) stop("Error: p is not between 0 < p < 1")
      mean = 1/prob
      var = (1-prob)/(prob^2)
      c(mean, var)
    },
    density = function(x, params, problog = FALSE, ...) {
      if( (params[[1]] > 1 || params[[1]] < 0) ) stop("Error: p is not between 0 < p < 1")
      dgeom(x, prob = params[[1]], log = problog)
    },
    probability = function(q, params, lowtail = TRUE, problog = FALSE, ...) {
      if( (params[[1]] > 1 || params[[1]] < 0) ) stop("Error: p is not between 0 < p < 1")
      pgeom(q, prob = params[[1]], lower.tail = lowtail, log.p = problog)
    },
    quantile = function(p, params, lowtail = TRUE, problog = FALSE, ...){
      if( (params[[1]] > 1 || params[[1]] < 0) ) stop("Error: p is not between 0 < p < 1")
      qgeom(p, prob = params[[1]], lower.tail = lowtail, log.p = problog)
    }
  ),
  
  hyper = list(
    type = "discrete",
    name  = "Hypergeometric Distribution",
    variable = "h",
    varsymbols = c("n","m","k"),
    params = 3,
    init  = function(params, ...) {
      m = params[[1]];n = params[[2]];k = params[[3]]
      if( m < 0 || n < 0 ) stop("Error: N_1 < 0 or N_2 < 0")
      mean = k*(m/(m+n))
      var = k*(m/(m+n))*(n/(m+n))*((m+n-k)/(m+n-1))
      c(mean, var)
    },
    density = function(x, params, problog = FALSE, ...) {
      if( params[[1]] < 0 || params[[2]] < 0 ) stop("Error: N_1 < 0 or N_2 < 0")
      dhyper(x, m=params[[1]], n=params[[2]], k=params[[3]], log = problog)
    },
    probability = function(q, params, lowtail = TRUE, problog = FALSE, ...) {
      if( params[[1]] < 0 || params[[2]] < 0 ) stop("Error: N_1 < 0 or N_2 < 0")
      phyper(q, m=params[[1]], n=params[[2]], k=params[[3]], lower.tail = lowtail, log.p = problog)
    },
    quantile = function(p, params, lowtail = TRUE, problog = FALSE, ...){
      if( params[[1]] < 0 || params[[2]] < 0 ) stop("Error: N_1 < 0 or N_2 < 0")
      qhyper(p, m=params[[1]], n=params[[2]], k=params[[3]], lower.tail = lowtail, log.p = problog)
    }
  ),
  
  nbinom = list(
    type = "discrete",
    name  = "Negative Binomial Distribution",
    variable = "nb",
    varsymbols = c("n","p"),
    params = 2,
    init  = function(params, ...) {
      size = params[[1]]; prob = params[[2]]
      if( size <= 0 || (prob > 1 || prob < 0) ) stop("Error: Size < 0 or p is not between 0 < p < 1")
      mean = size*(1/prob)
      var = (size*(1-prob))/(prob^2)
      c(mean, var)
    },
    density = function(x, params, problog = FALSE, ...) {
      if( params[[1]] <= 0 || (params[[2]] > 1 || params[[2]] < 0) ) stop("Error: Size < 0 or p is not between 0 < p < 1")
      dnbinom(x, size = params[[1]], prob = params[[2]], log = problog)
    },
    probability = function(q, params, lowtail = TRUE, problog = FALSE, ...) {
      if( params[[1]] <= 0 || (params[[2]] > 1 || params[[2]] < 0) ) stop("Error: Size < 0 or p is not between 0 < p < 1")
      pnbinom(q, size = params[[1]], prob = params[[2]], lower.tail = lowtail, log.p = problog)
    },
    quantile = function(p, params, lowtail = TRUE, problog = FALSE, ...){
      if( params[[1]] <= 0 || (params[[2]] > 1 || params[[2]] < 0) ) stop("Error: Size < 0 or p is not between 0 < p < 1")
      qnbinom(p, size = params[[1]], prob = params[[2]], lower.tail = lowtail, log.p = problog)
    }
  ),
  
  pois = list(
    type = "discrete",
    name  = "Poisson Distribution",
    variable = "p",
    varsymbols = c("\u03BB"),
    params = 1,
    init  = function(params, ...) {
      lambda = params[[1]]
      if( lambda <= 0 ) stop("Error: Lambda <= 0 ")
      mean = lambda
      var = lambda
      c(mean, var)
    },
    density = function(x, params, problog = FALSE, ...) {
      if( params[[1]] <= 0 ) stop("Error: Lambda <= 0 ")
      dpois(x, lambda = params[[1]], log = problog)
    },
    probability = function(q, params, lowtail = TRUE, problog = FALSE, ...) {
      if( params[[1]] <= 0 ) stop("Error: Lambda <= 0 ")
      ppois(q, lambda = params[[1]], lower.tail = lowtail, log.p = problog)
    },
    quantile = function(p, params, lowtail = TRUE, problog = FALSE, ...){
      if( params[[1]] <= 0 ) stop("Error: Lambda <= 0 ")
      qpois(p, lambda = params[[1]], lower.tail = lowtail, log.p = problog)
    }
  )
  #end discrete
)
#end supported distributions

class(visualize.distributions)= "distributions"