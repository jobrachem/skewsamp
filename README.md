
<!-- README.md is generated from README.Rmd. Please edit that file -->

# skewsamp

<!-- badges: start -->

[![R-CMD-check](https://github.com/jobrachem/skewsamp/workflows/R-CMD-check/badge.svg)](https://github.com/jobrachem/skewsamp/actions)
<!-- badges: end -->

The goal of skewsamp is to provide access to sample size estimation
methods for group comparisons where the underlying data are skewed and
thus violate the assumptions for common methods of sample size
estimation.

In particular, skewsamp offers an approach based on generalized linear
models (GLM) as described by Cundill & Alexander (2015) and the “NECDF”
(Noether Empirical Distribution Function) approach based on the
nonparametric Wilxocon-Mann-Whitney test in the location shift paradigm
as described by Chakraborti, Hong, & van de Wiel (2006).

## Installation

You can install the package directly from github:

``` r
# install.packages("devtools") # if you do not have devtools already installed, you need it for the installation
devtools::install_github("https://github.com/jobrachem/skewsamp)
```

## Documentation

All function are documented, so that you can use R’s builtin help
system. You can also refer to the [online
documentation](https://jobrachem.github.io/skewsamp/), which includes a
[list of all
functions](https://jobrachem.github.io/skewsamp/reference/index.html).

## Simulation study

We verified the correctness of our implementation through extensive
simulations. The data, code and final report are available on the Open
Science Framework. Note that the report is written in *german*.

-   <https://osf.io/z5vtf/> (Project)
-   <https://osf.io/yb5xm/> (Report)

The simulations revealed that the GLM-based approach (Cundill &
Alexander, 2015) works robustly. The nonparametric NECDF approach is
dependent on pilot data and can provide significant underestimations of
the required sample sizes. Please consult the report linked above for
further details.

## Usage

### Example 1

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

### Example 2

Sample size determination in the location shift approach. This approach
requires pilot data, which we draw from an exponential distribution for
the sake of the example:

``` r
library(skewsamp)
skewsamp::n_locshift(s1 = rexp(10), s2 = rexp(10), delta = 0.5, alpha = 0.05, power = 0.9)
#> Estimated sample size for group difference.
#> Wilcoxon-Mann-Whitney Test, Location shift 
#> 
#> N (total)         97.35 
#> n0 (Group 0)      48.68 
#> n1 (Group 1)      48.68 
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
