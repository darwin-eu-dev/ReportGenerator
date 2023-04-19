
<!-- README.md is generated from README.Rmd. Please edit that file -->

# ReportGenerator

<!-- badges: start -->

[![R-CMD-check](https://github.com/darwin-eu-dev/ReportGenerator/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/darwin-eu-dev/ReportGenerator/actions/workflows/R-CMD-check.yaml)
[![codecov.io](https://codecov.io/github/darwin-eu-dev/ReportGenerator/coverage.svg?branch=main)](https://codecov.io/github/darwin-eu-dev/ReportGenerator/coverage.svg?branch=main)
[![Lifecycle:Experimental](https://img.shields.io/badge/Lifecycle-Experimental-339999)](https://lifecycle.r-lib.org/articles/stages.html)
<!-- badges: end -->

Automatic report generator of the Darwin EU studies.

## Installation

You can install the development version of reportGenerator like so:

``` r
install.packages("remotes")
remotes::install_github("darwin-eu-dev/reportGenerator")
```

To test the package, generate first the mock data with
`generateMockData()`. It will create a “Results” folder in the working
directory that contains IncidencePrevalence data in csv and zip format.

``` r
 generateMockData(databaseName = c("Synthea", "IPCI", "CPRD"))
```

`reportGenerator()` takes as an input zip or csv files with results from
IncidencePrevalence.

To launch the Shiny app just type in the console `reportDashboard()`.

``` r
reportGenerator()
```
