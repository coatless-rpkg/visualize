# Visualize Cauchy Distribution

Generates a plot of the Wilcoxon Rank Sum distribution with user
specified parameters.

## Usage

``` r
visualize.wilcox(stat = 1, m = 7, n = 3, section = "lower")
```

## Arguments

- stat:

  a statistic to obtain the probability from. When using the "bounded"
  condition, you must supply the parameter as
  `stat = c(lower_bound, upper_bound)`. Otherwise, a simple
  `stat = desired_point` will suffice.

- m:

  Sample size from group 1.

- n:

  Sample size from group 2.

- section:

  Select how you want the statistic(s) evaluated via `section=` either
  `"lower"`,`"bounded"`, `"upper"`, or`"tails"`.

## Value

Returns a plot of the distribution according to the conditions supplied.

## See also

[`visualize.it()`](http://r-pkg.thecoatlessprofessor.com/visualize/reference/visualize.it.md),
[`dwilcox()`](https://rdrr.io/r/stats/Wilcoxon.html).

## Author

James Balamuta

## Examples

``` r

# Evaluates lower tail.
visualize.wilcox(stat = 1, m = 7, n = 3, section = "lower") 


# Evaluates bounded region.
visualize.wilcox(stat = c(2,3), m = 5, n = 4, section = "bounded") 


# Evaluates upper tail.
visualize.wilcox(stat = 1, m = 7, n = 3, section = "upper") 

```
