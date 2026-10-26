# Visualize's Processing Function

Acts as a director of traffic and first line of error handling regarding
submitted visualization requests. This function should only be used by
advanced users.

## Usage

``` r
visualize.it(
  dist = "norm",
  stat = c(0, 1),
  params = list(mu = 0, sd = 1),
  section = "lower",
  strict = c(0, 1)
)
```

## Arguments

- dist:

  a string that should be contain a supported probability distributions
  name in R. Supported continuous distributions: `"beta"`, `"chisq"`,
  `"exp"`, `"gamma"`, `"norm"`, and `"unif"`. Supported discrete
  distributions: `"binom"`, `"geom"`, `"hyper"`, `"nbinom"`, and
  `"pois"`.

- stat:

  a statistic to obtain the probability from. When using the "bounded"
  condition, you must supply the parameter as
  `stat = c(lower_bound, upper_bound)`. Otherwise, a simple
  `stat = desired_point` will suffice.

- params:

  A list that must contain the necessary parameters for each
  distribution. For example, `params = list(mu = 1, sd = 1)` would be
  for a normal distribution with mean 1 and standard deviation 1. If you
  are not aware of the parameters for the distribution, consider using
  the `visualize.dist` functions listed under the "See Also" section.

- section:

  Select how you want the statistic(s) evaluated via `section=` either
  `"lower"`,`"bounded"`, `"upper"`, or`"tails"`.

- strict:

  Determines whether the probability will be generated as a strict (\<,
  \>) or equal to (\<=, \>=) inequality. `strict=` requires either
  values = 0 or =FALSE for strict OR values =1 or =TRUE for equal to.
  For bounded condition use: `strict=c(0,1)` or `strict=c(FALSE,TRUE)`.

## Value

Returns a plot of the distribution according to the conditions supplied.

## References

http://cran.r-project.org/web/views/Distributions.html

## See also

[`visualize.beta()`](http://r-pkg.thecoatlessprofessor.com/visualize/reference/visualize.beta.md),
[`visualize.chisq()`](http://r-pkg.thecoatlessprofessor.com/visualize/reference/visualize.chisq.md),
[`visualize.exp()`](http://r-pkg.thecoatlessprofessor.com/visualize/reference/visualize.exp.md),
[`visualize.gamma()`](http://r-pkg.thecoatlessprofessor.com/visualize/reference/visualize.gamma.md),
[`visualize.norm()`](http://r-pkg.thecoatlessprofessor.com/visualize/reference/visualize.norm.md),
[`visualize.unif()`](http://r-pkg.thecoatlessprofessor.com/visualize/reference/visualize.unif.md),
[`visualize.binom()`](http://r-pkg.thecoatlessprofessor.com/visualize/reference/visualize.binom.md),
[`visualize.geom()`](http://r-pkg.thecoatlessprofessor.com/visualize/reference/visualize.geom.md),
[`visualize.hyper()`](http://r-pkg.thecoatlessprofessor.com/visualize/reference/visualize.hyper.md),
[`visualize.nbinom()`](http://r-pkg.thecoatlessprofessor.com/visualize/reference/visualize.nbinom.md),
[`visualize.pois()`](http://r-pkg.thecoatlessprofessor.com/visualize/reference/visualize.pois.md).

## Author

James Balamuta

## Examples

``` r

# Defaults to lower tail evaluation
visualize.it(dist = 'norm', stat = 1, list(mu = 3 , sd = 2), section = "lower")


# Set to evaluate the upper tail.
visualize.it(dist = 'norm', stat = 1, list(mu=3,sd=2),section="upper")


# Set to shade inbetween a bounded region.
visualize.it(dist = 'norm', stat = c(-1,1), list(mu=0,sd=1), section="bounded")


# Gamma distribution evaluated at upper tail.
visualize.it(dist = 'gamma', stat = 2, params = list(alpha=2,beta=1),section="upper")


# Binomial distribution evaluated at lower tail.
visualize.it('binom', stat = 2, params = list(n=4,p=.5))


```
