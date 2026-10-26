# Visualize Poisson Distribution

Generates a plot of the Poisson distribution with user specified
parameters.

## Usage

``` r
visualize.pois(stat = 1, lambda = 3.5, section = "lower", strict = FALSE)
```

## Arguments

- stat:

  a statistic to obtain the probability from. When using the "bounded"
  condition, you must supply the parameter as
  `stat = c(lower_bound, upper_bound)`. Otherwise, a simple
  `stat = desired_point` will suffice.

- lambda:

  lambda value of the Poisson Distribution.

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
, [`dpois()`](https://rdrr.io/r/stats/Poisson.html).

## Author

James Balamuta

## Examples

``` r

# Evaluates lower tail.
visualize.pois(stat = 1, lambda = 2, section = "lower", strict = FALSE) 


# Evaluates bounded region.
visualize.pois(stat = c(1,3), lambda = 3, section = "bounded", strict = c(0,1))


# Evaluates upper tail.
visualize.pois(stat = 1, lambda = 2, section = "upper", strict = 1)

```
