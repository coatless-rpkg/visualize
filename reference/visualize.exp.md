# Visualize Exponential Distribution

Generates a plot of the Exponential distribution with user specified
parameters.

## Usage

``` r
visualize.exp(stat = 1, theta = 1, section = "lower")
```

## Arguments

- stat:

  a statistic to obtain the probability from. When using the "bounded"
  condition, you must supply the parameter as
  `stat = c(lower_bound, upper_bound)`. Otherwise, a simple
  `stat = desired_point` will suffice.

- theta:

  vector of rates

- section:

  Select how you want the statistic(s) evaluated via `section=` either
  `"lower"`,`"bounded"`, `"upper"`, or`"tails"`.

## Value

Returns a plot of the distribution according to the conditions supplied.

## See also

[`visualize.it()`](http://r-pkg.thecoatlessprofessor.com/visualize/reference/visualize.it.md),
[`dexp()`](https://rdrr.io/r/stats/Exponential.html).

## Author

James Balamuta

## Examples

``` r

# Evaluates lower tail.
visualize.exp(stat = .5, theta = 3, section = "lower")


# Evaluates bounded region.
visualize.exp(stat = c(1,2), theta = 3, section = "bounded")


# Evaluates upper tail.
visualize.exp(stat = .5, theta = 3, section = "upper")

```
