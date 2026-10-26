# Graphing function for Continuous Distributions.

Handles how continuous distributions are graphed. Users should not use
this function. Instead, users should use
[`visualize.it()`](http://r-pkg.thecoatlessprofessor.com/visualize/reference/visualize.it.md).

## Usage

``` r
visualize.continuous(dist, stat = c(0, 1), params, section = "lower")
```

## Arguments

- dist:

  contains a supported continuos distribution shortname.

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
  the `visualize.`*dist_name* functions listed under the "See Also"
  section.

- section:

  Select how you want the statistic(s) evaluated via `section=` either
  `"lower"`,`"bounded"`, `"upper"`, or`"tails"`

## See also

[`visualize.it()`](http://r-pkg.thecoatlessprofessor.com/visualize/reference/visualize.it.md),
[`visualize.beta()`](http://r-pkg.thecoatlessprofessor.com/visualize/reference/visualize.beta.md),
[`visualize.chisq()`](http://r-pkg.thecoatlessprofessor.com/visualize/reference/visualize.chisq.md),
[`visualize.exp()`](http://r-pkg.thecoatlessprofessor.com/visualize/reference/visualize.exp.md),
[`visualize.gamma()`](http://r-pkg.thecoatlessprofessor.com/visualize/reference/visualize.gamma.md),
[`visualize.norm()`](http://r-pkg.thecoatlessprofessor.com/visualize/reference/visualize.norm.md),
[`visualize.unif()`](http://r-pkg.thecoatlessprofessor.com/visualize/reference/visualize.unif.md),
[`visualize.cauchy()`](http://r-pkg.thecoatlessprofessor.com/visualize/reference/visualize.cauchy.md),
[`visualize.f()`](http://r-pkg.thecoatlessprofessor.com/visualize/reference/visualize.f.md),
[`visualize.lnorm()`](http://r-pkg.thecoatlessprofessor.com/visualize/reference/visualize.lnorm.md),
[`visualize.t()`](http://r-pkg.thecoatlessprofessor.com/visualize/reference/visualize.t.md),
[`visualize.wilcox()`](http://r-pkg.thecoatlessprofessor.com/visualize/reference/visualize.wilcox.md),
[`visualize.logis()`](http://r-pkg.thecoatlessprofessor.com/visualize/reference/visualize.logis.md).

## Author

James Balamuta

## Examples

``` r
# Function does not have dist look up, must go through visualize.it
visualize.it(dist='norm', stat = c(0,1), params = list(mu = 1, sd = 1), section = "bounded")

```
