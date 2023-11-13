
<!-- README.md is generated from README.Rmd. Please edit that file -->

# visualize

<!-- badges: start -->

[![R-CMD-check](https://github.com/coatless-rpkg/visualize/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/coatless-rpkg/visualize/actions/workflows/R-CMD-check.yaml)
[![CRAN RStudio mirror
downloads](https://cranlogs.r-pkg.org/badges/visualize)](https://www.r-pkg.org/pkg/visualize)
[![CRAN
status](https://www.r-pkg.org/badges/version/visualize)](https://CRAN.R-project.org/package=visualize)
<!-- badges: end -->

The goal of visualize is to graph the pdf or pmf and highlight what area
or probability is present in user defined locations. Visualize is able
to provide lower tail, bounded, upper tail, and two tail calculations.
Supports strict and equal to inequalities. Also provided on the graph is
the mean and variance of the distribution.

## Installation

You can install the released version of visualize from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("visualize")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("coatless/visualize")
```

## Examples

To use visualize, load the package with:

``` r
library("visualize")
```

Then, construct a graph by following the `visualize.dist()` pattern. For
example, the normal distribution can be shown with:

``` r
# Graph a standard normal distribution with a z-score of 1.96
visualize.norm(1.96)
```

<img src="man/figures/README-visualize-normal-1.png" width="50%" />

``` r
# Change the location of the tail
visualize.norm(1.96, section = "upper")
```

<img src="man/figures/README-visualize-normal-2.png" width="50%" />

``` r
# Create a bounded range.
visualize.norm(c(-1.96, 1.96), section = "bounded")
```

<img src="man/figures/README-visualize-normal-3.png" width="50%" />

``` r
# Highlight both tails
visualize.norm(c(-1.96, 1.96), section = "tails")
```

<img src="man/figures/README-visualize-normal-4.png" width="50%" />

The parameters of the distribution can also be modified. Take for
example the Binomial distribution.

``` r
visualize.binom(stat = 9, size = 20, p = 0.5)
```

<img src="man/figures/README-visualize-binomial-1.png" width="50%" />

``` r
# Visualize the binominal distribution with a higher size and lower probability.
visualize.binom(stat = 9, size = 24, p = 0.25)
```

<img src="man/figures/README-visualize-binomial-2.png" width="50%" />

Discrete distributions can also handle a level of strict (`<`, `>`) or
equal to (`<=`, `>=`) inequality.

``` r
# Create a strict inequality (lower < x < higher)
visualize.pois(stat = c(4, 6), lambda = 3.5, section = "bounded",
  strict = c(TRUE, TRUE))
```

<img src="man/figures/README-discrete-inequalities-1.png" width="50%" />

``` r
# Allow for equality on either side only on the right side of the bounded region.
visualize.pois(stat = c(4, 6), lambda = 3.5, section = "bounded",
  strict = c(TRUE, FALSE))
```

<img src="man/figures/README-discrete-inequalities-2.png" width="50%" />

``` r
# Allow for equality on either side of the inequality (lower <= x <= higher)
visualize.pois(stat = c(4, 6), lambda = 3.5, section = "bounded",
  strict = c(FALSE, FALSE))
```

<img src="man/figures/README-discrete-inequalities-3.png" width="50%" />

## Author

James Joseph Balamuta

## Citing the `visualize` package

To ensure future development of the package, please cite `visualize`
package if used during an analysis or simulation study. Citation
information for the package may be acquired by using in *R*:

``` r
citation("visualize")
```

## License

MIT License
