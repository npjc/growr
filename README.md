
<!-- README.md is generated from README.Rmd. Please edit that file -->

# growr

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/growr)](https://CRAN.R-project.org/package=growr)
<!-- badges: end -->

growr helps you with the pieces of growth curve analysis.

1.  Preprocess growth curve data: `preprocess()`
2.  Summarise data with a family of functions: `fit_*()`
      - use built-in or your own `metric_*()` functions:
      - use built-in or your own models via `fit_nls(formula = )` 3

### High Level Interface

Currently, growr has high level interface that is meant to be simple
with reasonable defaults:

1.  `preprocess()` transform raw measures data into values suitable for
    summarization.
2.  `summarise_fit()` summarise a fit method into growth-curve metrics
    such as the fit’s \(A\), \(\mu\), \(\lambda\).
3.  `add_grouping()` and `add_groupings()` provide a convenient way to
    add relationships between sets of wells of a microtitre plate.
    Reference and target well designations can then be used to rescale
    summary metrics within groups (with for example, the provided
    `relatively()`).

### lower level functions

growr also exports an extensive set of models that make up a low level
interface. Nearly all these functions have previously been used in
growth curve analyses. *The majority of this functionality is not new*.
It reflects a collection from various public source from this author’s
exploration. Citations provided in documentation of the individual
methods.

## Example

#### Preprocessing

`preprocess()` takes a vector of values and returns a transformed
version. The details of the transformation depend on the argument
choices but follows the preprocessing steps laid out in the precog
paper.

``` r
library(tidyverse)
#> ── Attaching packages ────────────────────────────────────────────── tidyverse 1.2.1 ──
#> ✔ ggplot2 3.2.1.9000     ✔ purrr   0.3.2     
#> ✔ tibble  2.1.3          ✔ dplyr   0.8.3     
#> ✔ tidyr   1.0.0.9000     ✔ stringr 1.4.0     
#> ✔ readr   1.3.1          ✔ forcats 0.4.0
#> ── Conflicts ───────────────────────────────────────────────── tidyverse_conflicts() ──
#> ✖ dplyr::filter() masks stats::filter()
#> ✖ dplyr::lag()    masks stats::lag()
library(growr)

data <- mtpview::mtp_example3
data
#> # A tibble: 9,024 x 6
#>    well  strain drug        dose_um runtime measure
#>    <chr> <chr>  <chr>         <dbl>   <dbl>   <dbl>
#>  1 A01   wt     6772625 (2)       0     926   0.111
#>  2 A01   wt     6772625 (2)       0    1802   0.110
#>  3 A01   wt     6772625 (2)       0    2678   0.119
#>  4 A01   wt     6772625 (2)       0    3552   0.124
#>  5 A01   wt     6772625 (2)       0    4427   0.111
#>  6 A01   wt     6772625 (2)       0    5301   0.124
#>  7 A01   wt     6772625 (2)       0    6176   0.115
#>  8 A01   wt     6772625 (2)       0    7050   0.114
#>  9 A01   wt     6772625 (2)       0    7924   0.124
#> 10 A01   wt     6772625 (2)       0    8799   0.126
#> # … with 9,014 more rows
data %>% 
  group_by(well) %>% 
  mutate(measure_pp = preprocess(measure))
#> # A tibble: 9,024 x 7
#> # Groups:   well [96]
#>    well  strain drug        dose_um runtime measure measure_pp
#>    <chr> <chr>  <chr>         <dbl>   <dbl>   <dbl>      <dbl>
#>  1 A01   wt     6772625 (2)       0     926   0.111     0     
#>  2 A01   wt     6772625 (2)       0    1802   0.110     0.0139
#>  3 A01   wt     6772625 (2)       0    2678   0.119     0.0353
#>  4 A01   wt     6772625 (2)       0    3552   0.124     0.0422
#>  5 A01   wt     6772625 (2)       0    4427   0.111     0.0491
#>  6 A01   wt     6772625 (2)       0    5301   0.124     0.0491
#>  7 A01   wt     6772625 (2)       0    6176   0.115     0.0503
#>  8 A01   wt     6772625 (2)       0    7050   0.114     0.0511
#>  9 A01   wt     6772625 (2)       0    7924   0.124     0.0557
#> 10 A01   wt     6772625 (2)       0    8799   0.126     0.0604
#> # … with 9,014 more rows
```

#### Summarising fit

``` r
# well_summaries <- data %>% 
#   group_by(well) %>% 
#   mutate(measure_pp = preprocess(measure)) %>% 
#   summarise(metrics = list(summarise_fit(x = runtime, y = measure_pp))) %>% 
#   unnest(cols = c(metrics))
# well_summaries
```

#### Grouping

`add_groupings()` takes one or more strings with grouping shorthand,
expands them and joins them onto the provided data. The `source
well(s)`-\>`target well(s)`. Sets of wells are specified with the
familiar rectanuglar colon style notation: `A01:A12` is expanded to
contain row A wells from column 1 to column 12 inclusive.

``` r
# well_summaries %>% 
#   add_groupings(c("A01->A02:A12", "B01:H01->B02:H12"), nrow = 8, ncol = 12)
```

A more complex example (using the internal fxn for demonstration of the
parsing that goes on under the hood of `add_groupings()`)

``` r
# growr:::parse_grouping('p1!A01,B01:B04,H03->A01:H12', 8 , 12)
```

#### Relatively

Rescale you data by group, for example:

``` r
# well_summaries %>% 
#   add_groupings(c("A01->A02:A12", "B01:H01->B02:H12"), nrow = 8, ncol = 12) %>% 
#   group_by(group) %>% 
#   mutate(relative_mu = relatively(mu, is_ref)) 
```

-----

## Installation

``` r
# install.packages("growr")
remotes::install_github("npjc/growr")
```

-----

## Model Extension Interface:

`growr` provides many model choices, but you may want to add your own.
For this reason there is a standard way to add a new model interface

``` r
new_model_fxn <- function(x, y, ...) {
  model_obj <- fit_new_model(x, y, ...)
  
  
  output <- list(model = model_summary,
                 component = component_summary,
                 observation = observation_summary
                 )
}
```
