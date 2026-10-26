# Visualize Logistic distribution

Generates a plot of the Logistic distribution with user specified
parameters.

## Usage

``` r
visualize.logis(stat = 1, location = 3, scale = 1, section = "lower")
```

## Arguments

- stat:

  a statistic to obtain the probability from. When using the "bounded"
  condition, you must supply the parameter as
  `stat = c(lower_bound, upper_bound)`. Otherwise, a simple
  `stat = desired_point` will suffice.

- location:

  Location of the distribution.

- scale:

  Scale of the distribution.

- section:

  Select how you want the statistic(s) evaluated via `section=` either
  `"lower"`,`"bounded"`, `"upper"`, or`"tails"`.

## Value

Returns a plot of the distribution according to the conditions supplied.

## See also

[`visualize.it()`](http://r-pkg.thecoatlessprofessor.com/visualize/reference/visualize.it.md),
[`dlogis()`](https://rdrr.io/r/stats/Logistic.html).

## Author

James Balamuta

## Examples

``` r

# Evaluates lower tail.
visualize.logis(stat = 1, location = 4, scale = 2, section = "lower") 


# Evaluates bounded region.
visualize.logis(stat = c(3,5), location = 4, scale = 2, section = "bounded") 


# Evaluates upper tail.
visualize.logis(stat = 1, location = 4, scale = 2, section = "upper") 

```
