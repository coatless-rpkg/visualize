# Visualize Student's t distribution

Generates a plot of the Student's t distribution with user specified
parameters.

## Usage

``` r
visualize.t(stat = 1, df = 3, section = "lower")
```

## Arguments

- stat:

  a statistic to obtain the probability from. When using the "bounded"
  condition, you must supply the parameter as
  `stat = c(lower_bound, upper_bound)`. Otherwise, a simple
  `stat = desired_point` will suffice.

- df:

  Degrees of freedom

- section:

  Select how you want the statistic(s) evaluated via `section=` either
  `"lower"`,`"bounded"`, `"upper"`, or`"tails"`.

## Value

Returns a plot of the distribution according to the conditions supplied.

## See also

[`visualize.it()`](http://r-pkg.thecoatlessprofessor.com/visualize/reference/visualize.it.md),
[`dt()`](https://rdrr.io/r/stats/TDist.html).

## Author

James Balamuta

## Examples

``` r

# Evaluates lower tail.
visualize.t(stat = 1, df = 4, section = "lower") 


# Evaluates bounded region.
visualize.t(stat = c(3,5), df = 6, section = "bounded") 


# Evaluates upper tail.
visualize.t(stat = 1, df = 4, section = "upper") 

```
