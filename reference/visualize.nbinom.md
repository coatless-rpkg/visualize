# Visualize Negative Binomial Distribution

Generates a plot of the Negative Binomial distribution with user
specified parameters.

## Usage

``` r
visualize.nbinom(
  stat = 1,
  size = 6,
  prob = 0.5,
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

- size:

  number of objects.

- prob:

  probability of picking object.

- section:

  Select how you want the statistic(s) evaluated via `section=` either
  `"lower"`,`"bounded"`, `"upper"`, or`"tails"`.

- strict:

  Determines whether the probability will be generated as a strict (\<,
  \>) or equal to (\<=, \>=) inequality. `strict=` requires either
  values = 0 or = FALSE for equal to OR values =1 or =TRUE for strict.
  For bounded condition use: `strict=c(0,1)` or `strict=c(FALSE,TRUE)`.

## See also

[`visualize.it()`](http://r-pkg.thecoatlessprofessor.com/visualize/reference/visualize.it.md)
, [`dnbinom()`](https://rdrr.io/r/stats/NegBinomial.html).

## Author

James Balamuta

## Examples

``` r

# Evaluates lower tail.
visualize.nbinom(stat = 1, size = 5, prob = 0.5, section = "lower", strict = 0) 


# Evaluates bounded region.
visualize.nbinom(stat = c(1,3), size = 10, prob = 0.35, section = "bounded",
                    strict = c(TRUE, FALSE))


# Evaluates upper tail.
visualize.nbinom(stat = 1, size = 5, prob = 0.5, section = "upper", strict = 1)

```
