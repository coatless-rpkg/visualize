# Visualize Uniform Distribution

Generates a plot of the Uniform distribution with user specified
parameters.

## Usage

``` r
visualize.unif(stat = 1, a = 0, b = 1, section = "lower")
```

## Arguments

- stat:

  a statistic to obtain the probability from. When using the "bounded"
  condition, you must supply the parameter as
  `stat = c(lower_bound, upper_bound)`. Otherwise, a simple
  `stat = desired_point` will suffice.

- a:

  starting point. Note: `a`\<`b`

- b:

  end point. Note: `b` \> `a`

- section:

  Select how you want the statistic(s) evaluated via `section=` either
  `"lower"`,`"bounded"`, `"upper"`, or`"tails"`.

## See also

[`visualize.it()`](http://r-pkg.thecoatlessprofessor.com/visualize/reference/visualize.it.md)
, [`dunif()`](https://rdrr.io/r/stats/Uniform.html).

## Author

James Balamuta

## Examples

``` r

# Evaluates lower tail.
visualize.unif(stat = 8.75, a = 7, b = 10, section = "lower") 


# Evaluates bounded region.
visualize.unif(stat = c(3,6), a = 1, b = 7, section = "bounded")


# Evaluates upper tail.
visualize.unif(stat = 2, a = 1, b = 5, section = "upper")

```
