# Visualize Cauchy Distribution

Generates a plot of the Cauchy distribution with user specified
parameters.

## Usage

``` r
visualize.cauchy(stat = 1, location = 2, scale = 1, section = "lower")
```

## Arguments

- stat:

  a statistic to obtain the probability from. When using the "bounded"
  condition, you must supply the parameter as
  `stat = c(lower_bound, upper_bound)`. Otherwise, a simple
  `stat = desired_point` will suffice.

- location:

  location parameter

- scale:

  scale parameter

- section:

  Select how you want the statistic(s) evaluated via `section=` either
  `"lower"`,`"bounded"`, `"upper"`, or`"tails"`.

## Value

Returns a plot of the distribution according to the conditions supplied.

## See also

[`visualize.it()`](http://r-pkg.thecoatlessprofessor.com/visualize/reference/visualize.it.md),
[`dcauchy()`](https://rdrr.io/r/stats/Cauchy.html).

## Author

James Balamuta

## Examples

``` r

# Evaluates lower tail.
visualize.cauchy(stat = 1, location = 4, scale = 2, section = "lower") 
#> Warning: df2 < 2, mean is not able to be generated.


# Evaluates bounded region.
visualize.cauchy(stat = c(3,5), location = 5, scale = 3, section = "bounded") 
#> Warning: df2 < 2, mean is not able to be generated.


# Evaluates upper tail.
visualize.cauchy(stat = 1, location = 4, scale = 2, section = "upper") 
#> Warning: df2 < 2, mean is not able to be generated.

```
