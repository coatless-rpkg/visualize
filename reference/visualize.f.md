# Visualize F distribution

Generates a plot of the F distribution with user specified parameters.

## Usage

``` r
visualize.f(stat = 1, df1 = 5, df2 = 4, section = "lower")
```

## Arguments

- stat:

  a statistic to obtain the probability from. When using the "bounded"
  condition, you must supply the parameter as
  `stat = c(lower_bound, upper_bound)`. Otherwise, a simple
  `stat = desired_point` will suffice.

- df1:

  First Degrees of Freedom

- df2:

  Second Degrees of Freedom

- section:

  Select how you want the statistic(s) evaluated via `section=` either
  `"lower"`,`"bounded"`, `"upper"`, or`"tails"`.

## Value

Returns a plot of the distribution according to the conditions supplied.

## See also

[`visualize.it()`](http://r-pkg.thecoatlessprofessor.com/visualize/reference/visualize.it.md),
[`df()`](https://rdrr.io/r/stats/Fdist.html).

## Author

James Balamuta

## Examples

``` r

# Evaluates lower tail.
visualize.f(stat = 1, df1 = 5, df2 = 4, section = "lower") 
#> Warning: df2 < 4, variance is not able to be generated.


# Evaluates bounded region.
visualize.f(stat = c(3,5), df1 = 6, df2 = 3, section = "bounded") 
#> Warning: df2 < 4, variance is not able to be generated.


# Evaluates upper tail.
visualize.f(stat = 1, df1 = 5, df2 = 4, section = "upper") 
#> Warning: df2 < 4, variance is not able to be generated.

```
