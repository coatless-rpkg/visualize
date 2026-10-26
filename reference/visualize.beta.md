# Visualize Beta Distribution

Generates a plot of the Beta distribution with user specified
parameters.

## Usage

``` r
visualize.beta(stat = 1, alpha = 3, beta = 2, section = "lower")
```

## Arguments

- stat:

  a statistic to obtain the probability from. When using the "bounded"
  condition, you must supply the parameter as
  `stat = c(lower_bound, upper_bound)`. Otherwise, a simple
  `stat = desired_point` will suffice.

- alpha:

  `alpha` is considered to be *shape1* by R's implementation of the beta
  distribution. `alpha` must be greater than 0.

- beta:

  `beta` is considered to be *shape2* by R's implementation of the beta
  distribution. `beta` must be greater than 0.

- section:

  Select how you want the statistic(s) evaluated via `section=` either
  `"lower"`,`"bounded"`, `"upper"`, or`"tails"`.

## Value

Returns a plot of the distribution according to the conditions supplied.

## See also

[`visualize.it()`](http://r-pkg.thecoatlessprofessor.com/visualize/reference/visualize.it.md),
[`dbeta()`](https://rdrr.io/r/stats/Beta.html).

## Author

James Balamuta

## Examples

``` r

# Evaluates lower tail.
visualize.beta(stat = 1, alpha = 2, beta = 3, section = "lower") 


# Evaluates bounded region.
visualize.beta(stat = c(.5,1), alpha = 4, beta = 3, section = "bounded") 


# Evaluates upper tail.
visualize.beta(stat = 1, alpha = 2, beta = 3, section = "upper") 

```
