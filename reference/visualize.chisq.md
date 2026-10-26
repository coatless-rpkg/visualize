# Visualize Chi-squared Distribution

Generates a plot of the Chi-squared distribution with user specified
parameters.

## Usage

``` r
visualize.chisq(stat = 1, df = 3, section = "lower")
```

## Arguments

- stat:

  a statistic to obtain the probability from. When using the "bounded"
  condition, you must supply the parameter as
  `stat = c(lower_bound, upper_bound)`. Otherwise, a simple
  `stat = desired_point` will suffice.

- df:

  degrees of freedom of Chi-squared distribution.

- section:

  Select how you want the statistic(s) evaluated via `section=` either
  `"lower"`,`"bounded"`, `"upper"`, or`"tails"`.

## Value

Returns a plot of the distribution according to the conditions supplied.

## See also

[`visualize.it()`](http://r-pkg.thecoatlessprofessor.com/visualize/reference/visualize.it.md),
[`dchisq()`](https://rdrr.io/r/stats/Chisquare.html).

## Author

James Balamuta

## Examples

``` r

# Evaluates lower tail.
visualize.chisq(stat = 1, df = 3, section = "lower")

# Evaluates bounded region.
visualize.chisq(stat = c(1,2), df = 6, section = "bounded")

# Evaluates upper tail.
visualize.chisq(stat = 1, df = 3, section = "upper")


```
