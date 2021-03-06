% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/empirical_cdf.R
\name{qemp}
\alias{qemp}
\title{Empirical quantile function}
\usage{
qemp(p, sample)
}
\arguments{
\item{p}{probability, can be a vector}

\item{sample}{numeric vector of sample values to base the ECDF on}
}
\value{
Returns the value for which \code{pemp(x, sample) = p},
i.e. the probability that a value drawn at random from the ECDF
is smaller or equal to \code{x} is \code{p}.
}
\description{
Empirical quantile function, i.e. inverse of the empirical cumulative
density function \code{\link[=pemp]{pemp()}}. Based on the latter function as presented
by Chakraborti (2006).
}
\examples{
x <- 1:5
qemp(0.1, x)
}
\references{
Chakraborti, S., Hong, B., & Van De Wiel, M. A. (2006).
A note on sample size determination for a nonparametric test of location.
Technometrics, 48(1), 88–94. https://doi.org/10.1198/004017005000000193
}
