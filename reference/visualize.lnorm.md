# Visualize Log Normal Distribution

Generates a plot of the Log Normal distribution with user specified
parameters.

## Usage

``` r
visualize.lnorm(stat = 1, meanlog = 3, sdlog = 1, section = "lower")
```

## Arguments

- stat:

  a statistic to obtain the probability from. When using the "bounded"
  condition, you must supply the parameter as
  `stat = c(lower_bound, upper_bound)`. Otherwise, a simple
  `stat = desired_point` will suffice.

- meanlog:

  Mean of the distribution

- sdlog:

  Standard deviation of the distribution

- section:

  Select how you want the statistic(s) evaluated via `section=` either
  `"lower"`,`"bounded"`, `"upper"`, or`"tails"`.

## Value

Returns a plot of the distribution according to the conditions supplied.

## See also

[`visualize.it()`](http://r-pkg.thecoatlessprofessor.com/visualize/reference/visualize.it.md),
[`dlnorm()`](https://rdrr.io/r/stats/Lognormal.html).

## Author

James Balamuta

## Examples

``` r

# Evaluates lower tail.
visualize.lnorm(stat = 1, meanlog = 3, sdlog = 1, section = "lower") 


# Evaluates bounded region.
visualize.lnorm(stat = c(3,5), meanlog = 3, sdlog = 3, section = "bounded") 


# Evaluates upper tail.
visualize.lnorm(stat = 1, meanlog = 3, sdlog = 1, section = "upper") 

```
