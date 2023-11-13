# Creates the visualize.distribution object
# Allows for object-oriented selection of distributions.

visualize.distributions = list(
  #Start Continuous Distributions
  'beta' = list(
    type = "continuous",
    name = "Beta Distribution",
    variable = "b",
    varsymbols = c("alpha","beta"),
    params = 2,
    init  = function(params, ...) {
      shape1 = params[[1]]; shape2 = params[[2]]
      if(shape1 <= 0 ) stop("`alpha` must be greater than zero. Please set `alpha` > 0.")
      if(shape2 <= 0 ) stop("`beta` must be greater than zero. Please set `beta` > 0.")
      mean = shape1 / (shape1 + shape2)
      var = (shape1 * shape2)/((shape1 + shape2 + 1)*(shape1 + shape2)^2)
      c(mean, var)
    },
    density = function(x, params, ncp = 0, lower.tail = TRUE, log = FALSE, ...){
      if(params[[1]] <= 0 ) stop("`alpha` must be greater than zero. Please set `alpha` > 0.")
      if(params[[2]] <= 0 ) stop("`beta` must be greater than zero. Please set `beta` > 0.")
        dbeta(x,params[[1]], params[[2]], ncp = ncp, log = log)
    },
    probability = function(q, params, ncp = 0, lower.tail = TRUE, log.p = FALSE, ...){
      if(params[[1]] <= 0 ) stop("`alpha` must be greater than zero. Please set `alpha` > 0.")
      if(params[[2]] <= 0 ) stop("`beta` must be greater than zero. Please set `beta` > 0.")
      pbeta(q,params[[1]], params[[2]], ncp = ncp, lower.tail = lower.tail, log.p = log.p)
    },
    quantile = function(p, params, ncp = 0, lower.tail = TRUE, log.p = FALSE, ...){
      if(params[[1]] <= 0 ) stop("`alpha` must be greater than zero. Please set `alpha` > 0.")
      if(params[[2]] <= 0 ) stop("`beta` must be greater than zero. Please set `beta` > 0.")
      qbeta(p,params[[1]], params[[2]], ncp = ncp, lower.tail = lower.tail, log.p = log.p)
    }
  ),
  chisq = list(
    type = "continuous",
    name  = "Chi-square Distribution",
    variable = "chi^2",
    varsymbols = c("r"),
    params = 1,
    init  = function(params, ...) {
      r = params[[1]]
      if(r <= 0) stop("Degrees of freedom (`df`) was less than or equal to zero. Please set `df` to be greater than zero.")
      mean = r
      var = 2*r
      c(mean, var)
    },
    density = function(x, params, ncp = 0, log = FALSE, ...) {
      if(params[[1]] <= 0) stop("Degrees of freedom (`df`) was less than or equal to zero. Please set `df` to be greater than zero.")
      dchisq(x, df = params[[1]], ncp = ncp, log = log)
    },
    probability = function(q, params, ncp = 0, lower.tail = TRUE, log.p = FALSE, ...) {
      if(params[[1]] <= 0) stop("Degrees of freedom (`df`) was less than or equal to zero. Please set `df` to be greater than zero.")
      pchisq(q, df = params[[1]], ncp = ncp, lower.tail = lower.tail, log.p = log.p)
    },
    quantile = function(p, params, ncp = 0, lower.tail = TRUE, log.p = FALSE){
      if(params[[1]] <= 0) stop("Degrees of freedom (`df`) was less than or equal to zero. Please set `df` to be greater than zero.")
      pchisq(p, df = params[[1]], ncp = ncp, lower.tail = lower.tail, log.p = log.p)
    }
  ),
  
  exp = list(
    type = "continuous",
    name  = "Exponential Distribution",
    variable = "e",
    varsymbols = c("theta"),
    params = 1,
    init  = function(params, ...) {
      theta = params[[1]]
      if(theta <= 0) stop("`theta` was less than or equal to zero. Please set `theta` to be greater than zero.")
      mean = theta
      var = theta^2
      c(mean, var)
    },
    density = function(x, params, log = FALSE, ...) {
      if(params[[1]] <= 0) stop("`theta` was less than or equal to zero. Please set `theta` to be greater than zero.")
      dexp(x, rate = params[[1]], log = log)
    },
    probability = function(q, params, lower.tail = TRUE, log.p = FALSE, ...) {
      if(params[[1]] <= 0) stop("`theta` was less than or equal to zero. Please set `theta` to be greater than zero.")
      pexp(q, rate = params[[1]], lower.tail = lower.tail, log.p = log.p)
    },
    quantile = function(p, params, lower.tail = TRUE, log.p = FALSE){
      if(params[[1]] <= 0) stop("`theta` was less than or equal to zero. Please set `theta` to be greater than zero.")
      qexp(p, rate = params[[1]], lower.tail = lower.tail, log.p = log.p)
    }
  ),
  
  gamma = list(
    type = "continuous",
    name  = "Gamma Distribution",
    variable = "g",
    varsymbols = c("alpha","theta"),
    params = 2,
    init  = function(params,  ...) {
      alpha = params[[1]]; theta = params[[2]]
      if(alpha <= 0) stop("`alpha` was less than or equal to zero. Please set `alpha` to be greater than zero.")
      if(theta <= 0) stop("`theta` was less than or equal to zero. Please set `theta` to be greater than zero.")
      mean = alpha*theta
      var = alpha*theta^2
      c(mean, var)
    },
    density = function(x, params, log = FALSE, ...) {
      if(params[[1]] <= 0) stop("`alpha` was less than or equal to zero. Please set `alpha` to be greater than zero.")
      if(params[[2]] <= 0) stop("`theta` was less than or equal to zero. Please set `theta` to be greater than zero.")
      dgamma(x, shape = params[[1]], rate = params[[2]], log = log)
    },
    probability = function(q, params, lower.tail = TRUE, log.p = FALSE, ...) {
      if(params[[1]] <= 0) stop("`alpha` was less than or equal to zero. Please set `alpha` to be greater than zero.")
      if(params[[2]] <= 0) stop("`theta` was less than or equal to zero. Please set `theta` to be greater than zero.")
      pgamma(q, shape = params[[1]], rate = params[[2]], lower.tail = lower.tail, log.p = log.p)
    },
    quantile = function(p, params, lower.tail = TRUE, log.p = FALSE, ...){
      if(params[[1]] <= 0) stop("`alpha` was less than or equal to zero. Please set `alpha` to be greater than zero.")
      if(params[[2]] <= 0) stop("`theta` was less than or equal to zero. Please set `theta` to be greater than zero.")
      qgamma(p, shape = params[[1]], rate = params[[2]], lower.tail = lower.tail, log.p = log.p)
    }
  ),
  
  norm = list(
    type = "continuous",
    name  = "Normal Distribution",
    variable = "z",
    varsymbols = c("mu","sigma"),
    params = 2,
    init  = function(params,  ...) {
      if(params[[2]] < 0) stop("`sd` was less than zero. Please set `sd` to be greater than or equal to zero.")
      mean = params[[1]]
      var = (params[[2]])^2
      c(mean, var)
    },
    density = function(x, params, log = FALSE, ...) {
      if(params[[2]] < 0) stop("`sd` was less than zero. Please set `sd` to be greater than or equal to zero.")
      dnorm(x, mean = params[[1]], sd = params[[2]], log = log)
    },
    probability = function(q, params, lower.tail = TRUE, log.p = FALSE, ...) {
      if(params[[2]] < 0) stop("`sd` was less than zero. Please set `sd` to be greater than or equal to zero.")
      pnorm(q, mean = params[[1]], sd = params[[2]], lower.tail = lower.tail, log.p = log.p)
    },
    quantile = function(p, params, lower.tail = TRUE, log.p = FALSE, ...){
      if(params[[2]] < 0) stop("`sd` was less than zero. Please set `sd` to be greater than or equal to zero.")
      qnorm(p, mean = params[[1]], sd = params[[2]], lower.tail = lower.tail, log.p = log.p)
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
      if(a > b) stop("The start point `a` was greater than the end point `b`. Please provide values such that a < b.")
      mean = (a+b)/2
      var = (b-a)^2/12
      c(mean, var)
    },
    density = function(x, params, log = FALSE, ...) {
      if(params[[1]] > params[[2]]) stop("The start point `a` was greater than the end point `b`. Please provide values such that a < b.")
      dunif(x, min = params[[1]], max = params[[2]], log = log)
    },
    probability = function(q, params, lower.tail = TRUE, log.p = FALSE, ...) {
      if(params[[1]] > params[[2]]) stop("The start point `a` was greater than the end point `b`. Please provide values such that a < b.")
      punif(q, min = params[[1]], max = params[[2]], lower.tail = lower.tail, log.p = log.p)
    },
    quantile = function(p, params, lower.tail = TRUE, log.p = FALSE, ...){
      if(params[[1]] > params[[2]]) stop("The start point `a` was greater than the end point `b`. Please provide values such that a < b.")
      qunif(p, min = params[[1]], max = params[[2]], lower.tail = lower.tail, log.p = log.p)
    }
  ),
#V1.5
  cauchy = list(
    type = "continuous",
    name = "Cauchy Distribution",
    variable = "c",
    varsymbols = c("t","s"),
    params = 2,
    init  = function(params, ...) {
      c("Undefined","Undefined")
    },
    density = function(x,params, lower.tail = TRUE, log = FALSE, ...){
      if(params[[2]] < 0) stop("`scale` was less than zero. Please set `scale` to be greater than zero.")
      dcauchy(x,location = params[[1]], scale = params[[2]], log = log)
    },
    probability = function(q,params, lower.tail = TRUE, log.p = FALSE, ...){
      if(params[[2]] < 0) stop("`scale` was less than zero. Please set `scale` to be greater than zero.")
      pcauchy(q,location = params[[1]], scale = params[[2]], lower.tail = lower.tail, log.p = log.p)
    },
    quantile = function(p, params, lower.tail = TRUE, log.p = FALSE, ...){
      if(params[[2]] < 0) stop("`scale` was less than zero. Please set `scale` to be greater than zero.")
      qcauchy(p,location = params[[1]], scale = params[[2]], lower.tail = lower.tail, log.p = log.p)
    }
  ),
  
  lnorm = list(
    type = "continuous",
    name = "Log Normal Distribution",
    variable = "log(z)",
    varsymbols = c("mu","sigma"),
    params = 2,
    init  = function(params, ...) {
      if(params[[2]] < 0) stop("`sd` was less than zero. Please set `sd` to be greater than or equal to zero.")
      mean = exp(params[[1]]+((params[[2]]*params[[2]])/2))
      var = (exp(params[[2]]*params[[2]])-1)*exp(2*params[[1]]+params[[2]]*params[[2]])
      c(mean,var)
    },
    density = function(x,params, lower.tail = TRUE, log = FALSE, ...){
      if(params[[2]] < 0) stop("`sd` was less than zero. Please set `sd` to be greater than or equal to zero.")
      dlnorm(x, meanlog = params[[1]], sdlog = params[[2]], log = log)
    },
    probability = function(q,params, lower.tail = TRUE, log.p = FALSE, ...){
      if(params[[2]] < 0) stop("`sd` was less than zero. Please set `sd` to be greater than or equal to zero.")
      plnorm(q, meanlog = params[[1]], sdlog = params[[2]], lower.tail = lower.tail, log.p = log.p)
    },
    quantile = function(p,params, lower.tail = TRUE, log.p = FALSE, ...){
      if(params[[2]] < 0) stop("`sd` was less than zero. Please set `sd` to be greater than or equal to zero.")
      qlnorm(p, meanlog = params[[1]], sdlog = params[[2]], lower.tail = lower.tail, log.p = log.p)
    }
    ),
  
  t = list(
    type = "continuous",
    name  = "Student t Distribution",
    variable = "t",
    varsymbols = c("df"),
    params = 1,
    init  = function(params,  ...) {
      df = params[[1]]
      if(df < 0) stop("Degrees of freedom (`df`) was less than zero. Please set `df` to be greater than zero.")
      mean = 0
      if(df > 2) var = df/(df-2)
      else if(df <= 2 & df > 1) var = "+Inf"
      else var = "Undefined"
      c(mean, var)
    },
    density = function(x, params, ncp, log = FALSE, ...) {
      if(params[[1]] < 0) stop("Degrees of freedom (`df`) was less than zero. Please set `df` to be greater than zero.")
      dt(x, df = params[[1]], ncp = ncp, log = log)
    },
    probability = function(q, params, ncp, lower.tail = TRUE, log.p = FALSE, ...) {
      if(params[[1]] < 0) stop("Degrees of freedom (`df`) was less than zero. Please set `df` to be greater than zero.")
      pt(q, df = params[[1]], ncp = ncp, lower.tail = lower.tail, log.p = log.p)
    },
    quantile = function(p, params, ncp, lower.tail = TRUE, log.p = FALSE, ...){
      if(params[[1]] < 0) stop("Degrees of freedom (`df`) was less than zero. Please set `df` to be greater than zero.")
      qt(p, df = params[[1]], ncp = ncp, lower.tail = lower.tail, log.p = log.p)
    }
  ),
  
  f = list(
    type = "continuous",
    name  = "F Distribution",
    variable = "F",
    varsymbols = c("df1","df2"),
    params = 2,
    init  = function(params,  ...) {
      df1 = params[[1]]; df2 = params[[2]]
      if( params[[1]] < 0 ) stop("Degrees of freedom (`df1`) was less than zero. Please set `df1` to be greater than zero.")
      if( params[[2]] < 0 ) stop("Degrees of freedom (`df2`) was less than zero. Please set `df2` to be greater than zero.")
      
      if(df2 > 2){mean = df2/(df2-2)}
      else{mean = "Undefined"}
      if(df2 > 4) var = 2*df2*df2*(df1+df2-2)/(df1*(df2-2)*(df2-2)*(df2-4))
      else var = "Undefined"
      c(mean, var)
    },
    density = function(x, params, ncp, log = FALSE, ...) {
      if( params[[1]] < 0 ) stop("Degrees of freedom (`df1`) was less than zero. Please set `df1` to be greater than zero.")
      if( params[[2]] < 0 ) stop("Degrees of freedom (`df2`) was less than zero. Please set `df2` to be greater than zero.")
      df(x, df1 = params[[1]], df2 = params[[2]], ncp = ncp, log = log)
    },
    probability = function(q, params, ncp, lower.tail = TRUE, log.p = FALSE, ...) {
      if( params[[1]] < 0 ) stop("Degrees of freedom (`df1`) was less than zero. Please set `df1` to be greater than zero.")
      if( params[[2]] < 0 ) stop("Degrees of freedom (`df2`) was less than zero. Please set `df2` to be greater than zero.")
      pf(q, df1 = params[[1]], df2 = params[[2]], ncp = ncp, lower.tail = lower.tail, log.p = log.p)
    },
    quantile = function(p, params, ncp, lower.tail = TRUE, log.p = FALSE, ...){
      if( params[[1]] < 0 ) stop("Degrees of freedom (`df1`) was less than zero. Please set `df1` to be greater than zero.")
      if( params[[2]] < 0 ) stop("Degrees of freedom (`df2`) was less than zero. Please set `df2` to be greater than zero.")
      qf(p, df1 = params[[1]], df2 = params[[2]], ncp = ncp, lower.tail = lower.tail, log.p = log.p)
    }
  ),
  
  wilcox = list(
    type = "continuous",
    name  = "Wilcox Rank Sum Distribution",
    variable = "w",
    varsymbols = c("m","n"),
    params = 2,
    init  = function(params,  ...) {
      m = params[[1]]; n = params[[2]]
      mean =  m * n / 2
      var =  m * n * (m + n + 1) / 12
      c(mean, var)
    },
    density = function(x, params, log = FALSE, ...) {
      dwilcox(x, m = params[[1]], n = params[[2]], log = log)
    },
    probability = function(q, params, lower.tail = TRUE, log.p = FALSE, ...) {
      pwilcox(q, m = params[[1]], n = params[[2]], lower.tail = lower.tail, log.p = log.p)
    },
    quantile = function(p, params, lower.tail = TRUE, log.p = FALSE, ...){
      qwilcox(p, m = params[[1]], n = params[[2]], lower.tail = lower.tail, log.p = log.p)
    }
  ),
  
  logis = list(
    type = "continuous",
    name  = "Logistic Distribution",
    variable = "l",
    varsymbols = c("loc","scale"),
    params = 2,
    init  = function(params,  ...) {
      if(params[[2]] < 0) stop("`scale` was less than zero. Please set `scale` to be greater than zero.")
      mean =  params[[1]]
      var =  1/3 * params[[2]] * params[[2]] * pi * pi
      c(mean, var)
    },
    density = function(x, params, log = FALSE, ...) {
      if(params[[2]] < 0) stop("`scale` was less than zero. Please set `scale` to be greater than zero.")
      dlogis(x, location = params[[1]], scale = params[[2]], log = log)
    },
    probability = function(q, params, lower.tail = TRUE, log.p = FALSE, ...) {
      if(params[[2]] < 0) stop("`scale` was less than zero. Please set `scale` to be greater than zero.")
      plogis(q, location = params[[1]], scale = params[[2]], lower.tail = lower.tail, log.p = log.p)
    },
    quantile = function(p, params, lower.tail = TRUE, log.p = FALSE, ...){
      if(params[[2]] < 0) stop("`scale` was less than zero. Please set `scale` to be greater than zero.")
      qlogis(p, location = params[[1]], scale = params[[2]], lower.tail = lower.tail, log.p = log.p)
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
      if( n <= 0 ) stop("`size` is less than zero. Please set size > 0.")
      if( prob > 1 || prob < 0 ) stop("`prob` is not a valid probability. Please set `prob` to be between 0 < p < 1.")
      mean = n*prob
      var = n*prob*(1-prob)
      c(mean, var)
    },
    density = function(x, params, log = FALSE, ...) {
      if( params[[1]] <= 0 ) stop("`size` is less than zero. Please set size > 0.")
      if( params[[2]] > 1 || params[[2]] < 0 ) stop("`prob` is not a valid probability. Please set `prob` to be between 0 < p < 1.")
      dbinom(x, size = params[[1]], prob = params[[2]], log = log)
    },
    probability = function(q, params, lower.tail = TRUE, log.p = FALSE, ...) {
      if( params[[1]] <= 0 ) stop("`size` is less than zero. Please set size > 0.")
      if( params[[2]] > 1 || params[[2]] < 0 ) stop("`prob` is not a valid probability. Please set `prob` to be between 0 < p < 1.")
      pbinom(q, size = params[[1]], prob = params[[2]], lower.tail = lower.tail, log.p = log.p)
    },
    quantile = function(p, params, lower.tail = TRUE, log.p = FALSE, ...){
      if( params[[1]] <= 0 ) stop("`size` is less than zero. Please set size > 0.")
      if( params[[2]] > 1 || params[[2]] < 0 ) stop("`prob` is not a valid probability. Please set `prob` to be between 0 < p < 1.")
      qbinom(p, size = params[[1]], prob = params[[2]], lower.tail = lower.tail, log.p = log.p)
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
      if( (prob > 1 || prob < 0) ) stop("`prob` is not a valid probability. Please set `prob` to be between 0 < p < 1.")
      mean = 1/prob
      var = (1-prob)/(prob^2)
      c(mean, var)
    },
    density = function(x, params, log = FALSE, ...) {
      if( (params[[1]] > 1 || params[[1]] < 0) ) stop("`prob` is not a valid probability. Please set `prob` to be between 0 < p < 1.")
      dgeom(x, prob = params[[1]], log = log)
    },
    probability = function(q, params, lower.tail = TRUE, log.p = FALSE, ...) {
      if( (params[[1]] > 1 || params[[1]] < 0) ) stop("`prob` is not a valid probability. Please set `prob` to be between 0 < p < 1.")
      pgeom(q, prob = params[[1]], lower.tail = lower.tail, log.p = log.p)
    },
    quantile = function(p, params, lower.tail = TRUE, log.p = FALSE, ...){
      if( (params[[1]] > 1 || params[[1]] < 0) ) stop("`prob` is not a valid probability. Please set `prob` to be between 0 < p < 1.")
      qgeom(p, prob = params[[1]], lower.tail = lower.tail, log.p = log.p)
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
      if( m < 0 ) stop("`m` was less than zero. Please set `m` to be greater than zero.")
      if( n < 0 ) stop("`n` was less than zero. Please set `n` to be greater than zero.")
      mean = k*(m/(m+n))
      var = k*(m/(m+n))*(n/(m+n))*((m+n-k)/(m+n-1))
      c(mean, var)
    },
    density = function(x, params, log = FALSE, ...) {
      if( params[[1]] < 0 ) stop("`m` was less than zero. Please set `m` to be greater than zero.")
      if( params[[2]] < 0 ) stop("`n` was less than zero. Please set `n` to be greater than zero.")
      dhyper(x, m=params[[1]], n=params[[2]], k=params[[3]], log = log)
    },
    probability = function(q, params, lower.tail = TRUE, log.p = FALSE, ...) {
      if( params[[1]] < 0 ) stop("`m` was less than zero. Please set `m` to be greater than zero.")
      if( params[[2]] < 0 ) stop("`n` was less than zero. Please set `n` to be greater than zero.")
      phyper(q, m=params[[1]], n=params[[2]], k=params[[3]], lower.tail = lower.tail, log.p = log.p)
    },
    quantile = function(p, params, lower.tail = TRUE, log.p = FALSE, ...){
      if( params[[1]] < 0 ) stop("`m` was less than zero. Please set `m` to be greater than zero.")
      if( params[[2]] < 0 ) stop("`n` was less than zero. Please set `n` to be greater than zero.")
      qhyper(p, m=params[[1]], n=params[[2]], k=params[[3]], lower.tail = lower.tail, log.p = log.p)
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
      if( params[[1]] <= 0 ) stop("`size` is less than zero. Please set size > 0.")
      if( params[[2]] > 1 || params[[2]] < 0 ) stop("`prob` is not a valid probability. Please set `prob` to be between 0 < p < 1.")
      mean = size*(1/prob)
      var = (size*(1-prob))/(prob^2)
      c(mean, var)
    },
    density = function(x, params, log = FALSE, ...) {
      if( params[[1]] <= 0 ) stop("`size` is less than zero. Please set size > 0.")
      if( params[[2]] > 1 || params[[2]] < 0 ) stop("`prob` is not a valid probability. Please set `prob` to be between 0 < p < 1.")
      dnbinom(x, size = params[[1]], prob = params[[2]], log = log)
    },
    probability = function(q, params, lower.tail = TRUE, log.p = FALSE, ...) {
      if( params[[1]] <= 0 ) stop("`size` is less than zero. Please set size > 0.")
      if( params[[2]] > 1 || params[[2]] < 0 ) stop("`prob` is not a valid probability. Please set `prob` to be between 0 < p < 1.")
      pnbinom(q, size = params[[1]], prob = params[[2]], lower.tail = lower.tail, log.p = log.p)
    },
    quantile = function(p, params, lower.tail = TRUE, log.p = FALSE, ...){
      if( params[[1]] <= 0 ) stop("`size` is less than zero. Please set size > 0.")
      if( params[[2]] > 1 || params[[2]] < 0 ) stop("`prob` is not a valid probability. Please set `prob` to be between 0 < p < 1.")
      qnbinom(p, size = params[[1]], prob = params[[2]], lower.tail = lower.tail, log.p = log.p)
    }
  ),
  
  pois = list(
    type = "discrete",
    name  = "Poisson Distribution",
    variable = "p",
    varsymbols = c("lambda"),
    params = 1,
    init  = function(params, ...) {
      lambda = params[[1]]
      if( lambda <= 0 ) stop("`lambda` must be greater than 0. Please set lambda > 0.")
      mean = lambda
      var = lambda
      c(mean, var)
    },
    density = function(x, params, log = FALSE, ...) {
      if( params[[1]] <= 0 ) stop("`lambda` must be greater than 0. Please set lambda > 0.")
      dpois(x, lambda = params[[1]], log = log)
    },
    probability = function(q, params, lower.tail = TRUE, log.p = FALSE, ...) {
      if( params[[1]] <= 0 ) stop("`lambda` must be greater than 0. Please set lambda > 0.")
      ppois(q, lambda = params[[1]], lower.tail = lower.tail, log.p = log.p)
    },
    quantile = function(p, params, lower.tail = TRUE, log.p = FALSE, ...){
      if( params[[1]] <= 0 ) stop("`lambda` must be greater than 0. Please set lambda > 0.")
      qpois(p, lambda = params[[1]], lower.tail = lower.tail, log.p = log.p)
    }
  )
  #end discrete
)
#end supported distributions

class(visualize.distributions) = "distributions"


# use sysdata
usethis::use_data(visualize.distributions, internal = TRUE, overwrite = TRUE)
