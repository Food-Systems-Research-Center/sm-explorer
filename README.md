
<!-- README.md is generated from README.Rmd. Please edit that file -->

# `{SMexplorer}`

<!-- badges: start -->
<!-- badges: end -->

## Installation

You can install the development version of `{SMexplorer}` like so:

``` r
# FILL THIS IN! HOW CAN PEOPLE INSTALL YOUR DEV PACKAGE?
```

## Run

You can launch the application by running:

``` r
SMexplorer::run_app()
```

## About

You are reading the doc about version : 0.0.0.9000

This README has been compiled on the

``` r
Sys.time()
#> [1] "2024-09-20 19:33:53 EDT"
```

Here are the tests results and package coverage:

``` r
devtools::check(quiet = TRUE)
#> ══ Documenting ═════════════════════════════════════════════════════════════════
#> ℹ Installed roxygen2 version (7.3.2) doesn't match required (7.1.1)
#> ✖ `check()` will not re-document this package
#> ── R CMD check results ────────────────────────────── SMexplorer 0.0.0.9000 ────
#> Duration: 18.7s
#> 
#> ❯ checking DESCRIPTION meta-information ... NOTE
#>   Malformed Description field: should contain one or more complete sentences.
#> 
#> 0 errors ✔ | 0 warnings ✔ | 1 note ✖
```

``` r
covr::package_coverage()
#> SMexplorer Coverage: 0.00%
#> R/app_config.R: 0.00%
#> R/app_ui.R: 0.00%
#> R/run_app.R: 0.00%
```
