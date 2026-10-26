# Visualize Hypergeometric Distribution

Generates a plot of the Hypergeometric distribution with user specified
parameters.

## Usage

``` r
visualize.hyper(
  stat = 1,
  m = 5,
  n = 4,
  k = 2,
  section = "lower",
  strict = FALSE
)
```

## Arguments

- stat:

  a statistic to obtain the probability from. When using the "bounded"
  condition, you must supply the parameter as
  `stat = c(lower_bound, upper_bound)`. Otherwise, a simple
  `stat = desired_point` will suffice.

- m:

  `m` white balls. `m` must be greater than 0.

- n:

  `n` black balls. `n` must be greater than 0.

- k:

  draw `k` balls without replacement.

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
, [`dhyper()`](https://rdrr.io/r/stats/Hypergeometric.html).

## Author

James Balamuta

## Examples

``` r

# Evaluates lower tail.
visualize.hyper(stat = 1, m=4, n=5, k=3, section = "lower", strict = 0) 


# Evaluates bounded region.
visualize.hyper(stat = c(2,4), m=14, n=5, k=2, section = "bounded", strict = c(0,1))


# Evaluates upper tail.
visualize.hyper(stat = 1, m=4, n=5, k=3, section = "upper", strict = 1)

```
