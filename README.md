
<!-- README.md is generated from README.Rmd. Please edit that file -->

# skewsamp

<!-- badges: start -->
<!-- badges: end -->

The goal of skewsamp is to provide access to sample size estimation
methods for group comparisons where the underlying data are skewed and
thus violate the assumptions for common methods of sample size
estimation.

In particular, skewsamp offers an approach based on generalized linear
models (GLM) as described by Cundill & Alexander (2015) and an approach
based on the nonparametric Wilxocon-Mann-Whitney test in the location
shift paradigm as described by Chakraborti, Hong, & van de Wiel (2006).

## Installation

You can install the package using the binary file
`build/skewsamp_1.0.0.tgz` with the following command:

``` r
install.packages("<path-to-binary>/skewsamp_1.0.0.tgz", repos = NULL, type = "binary")
```

## Example 1

Sample size determination in the GLM approach for gamma-distributed
data:

``` r
library(skewsamp)
skewsamp::n_gamma(mean0 = 1, effect = 0.5, shape0 = 1, alpha = 0.05, power = 0.9)
#> Estimated sample size for group difference.
#> Generalized Regression, Gamma Distribution, link: log 
#> 
#> N (total)         87.48 
#> n0 (Group 0)      43.74 
#> n1 (Group 1)      43.74 
#> 
#> Effect size       0.5 
#> Effect type       1 - (mean1/mean0) 
#> Type I error      0.05 
#> Target power      0.9 
#> Two-sided         TRUE 
#> 
#> Call: skewsamp::n_gamma(mean0 = 1, effect = 0.5, shape0 = 1, alpha = 0.05, 
#>     power = 0.9)
```

## Example 2

Sample size determination in the location shiftb approach. This approach
requires pilot data, which we draw from an exponential distribution for
the sake of the example:

``` r
library(skewsamp)
skewsamp::n_locshift(s1 = rexp(10), s2 = rexp(10), delta = 0.5, alpha = 0.05, power = 0.9)
#> Estimated sample size for group difference.
#> Wilcoxon-Mann-Whitney Test, Location shift 
#> 
#> N (total)         78.71 
#> n0 (Group 0)      39.35 
#> n1 (Group 1)      39.35 
#> 
#> Effect size       0.5 
#> Effect type       location shift 
#> Type I error      0.05 
#> Target power      0.9 
#> Two-sided         FALSE 
#> 
#> Call: skewsamp::n_locshift(s1 = rexp(10), s2 = rexp(10), delta = 0.5, 
#>     alpha = 0.05, power = 0.9)
```

# References

-   Cundill, B., & Alexander, N. D. E. (2015). Sample size calculations
    for skewed distributions. BMC Medical Research Methodology, 15(1),
    1–9. <https://doi.org/10.1186/s12874-015-0023-0>
-   Chakraborti, S., Hong, B., & Van De Wiel, M. A. (2006). A note on
    sample size determination for a nonparametric test of location.
    Technometrics, 48(1), 88–94.
    <https://doi.org/10.1198/004017005000000193>
