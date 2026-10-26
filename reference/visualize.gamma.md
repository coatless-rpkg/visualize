# Visualize Gamma Distribution

Generates a plot of the Gamma distribution with user specified
parameters.

## Usage

``` r
visualize.gamma(stat = 1, alpha = 1, theta = 1, section = "lower")
```

## Arguments

- stat:

  a statistic to obtain the probability from. When using the "bounded"
  condition, you must supply the parameter as
  `stat = c(lower_bound, upper_bound)`. Otherwise, a simple
  `stat = desired_point` will suffice.

- alpha:

  `alpha` is considered to be *shape* by R's implementation of the gamma
  distribution. `alpha` must be greater than 0.

- theta:

  `theta` is considered to be *rate* by R's implementation of the gamma
  distribution. `theta` must be greater than 0.

- section:

  Select how you want the statistic(s) evaluated via `section=` either
  `"lower"`,`"bounded"`, `"upper"`, or`"tails"`.

## See also

[`visualize.it()`](http://r-pkg.thecoatlessprofessor.com/visualize/reference/visualize.it.md),
[`dgamma()`](https://rdrr.io/r/stats/GammaDist.html).

## Author

James Balamuta

## Examples

``` r

# Evaluate lower tail.
visualize.gamma(stat = 1, alpha = 3, theta = 1, section = "lower") 


# Evaluate bounded section.
visualize.gamma(stat = c(0.75,1), alpha = 3, theta = 1, section = "bounded") 


# Evaluate upper tail.
visualize.gamma(stat = 1, alpha = 3, theta = 1, section = "upper") 


```
