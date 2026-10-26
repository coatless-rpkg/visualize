# Visualize Geometric Distribution

Generates a plot of the Geometric distribution with user specified
parameters.

## Usage

``` r
visualize.geom(stat = 1, prob = 0.3, section = "lower", strict = FALSE)
```

## Arguments

- stat:

  a statistic to obtain the probability from. When using the "bounded"
  condition, you must supply the parameter as
  `stat = c(lower_bound, upper_bound)`. Otherwise, a simple
  `stat = desired_point` will suffice.

- prob:

  probability of picking object.

- section:

  Select how you want the statistic(s) evaluated via `section=` either
  `"lower"`,`"bounded"`, `"upper"`, or`"tails"`.

- strict:

  Determines whether the probability will be generated as a strict (\<,
  \>) or equal to (\<=, \>=) inequality. `strict=` requires either
  values = 0 or =FALSE for equal to OR values =1 or =TRUE for strict.
  For bounded condition use: `strict=c(0,1)` or `strict=c(FALSE,TRUE)`.

## See also

[`visualize.it()`](http://r-pkg.thecoatlessprofessor.com/visualize/reference/visualize.it.md)
, [`dgeom()`](https://rdrr.io/r/stats/Geometric.html).

## Author

James Balamuta

## Examples

``` r

# Evaluates lower tail.
visualize.geom(stat = 1, prob = 0.5, section = "lower", strict = FALSE) 


# Evaluates bounded region.
visualize.geom(stat = c(1,3), prob = 0.35, section = "bounded", strict = c(0,1))


# Evaluates upper tail.
visualize.geom(stat = 1, prob = 0.5, section = "upper", strict = 1)

```
