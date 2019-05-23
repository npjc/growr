
<!-- README.md is generated from README.Rmd. Please edit that file -->

# growr

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
<!-- badges: end -->

growr helps you with the pieces of growth curve analysis.

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
#> Registered S3 methods overwritten by 'ggplot2':
#>   method         from 
#>   [.quosures     rlang
#>   c.quosures     rlang
#>   print.quosures rlang
#> ── Attaching packages ────────────────────────────────── tidyverse 1.2.1 ──
#> ✔ ggplot2 3.1.1          ✔ purrr   0.3.2     
#> ✔ tibble  2.1.1          ✔ dplyr   0.8.1     
#> ✔ tidyr   0.8.3.9000     ✔ stringr 1.4.0     
#> ✔ readr   1.3.1          ✔ forcats 0.4.0
#> ── Conflicts ───────────────────────────────────── tidyverse_conflicts() ──
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
well_summaries <- data %>% 
  group_by(well) %>% 
  mutate(measure_pp = preprocess(measure)) %>% 
  summarise(metrics = list(summarise_fit(x = runtime, y = measure_pp))) %>% 
  unnest(cols = c(metrics))
```

#### Grouping

`add_groupings()` takes one or more strings with grouping shorthand,
expands them and joins them onto the provided data. The `source
well(s)`-\>`target well(s)`. Sets of wells are specified with the
familiar rectanuglar colon style notation: `A01:A12` is expanded to
contain row A wells from column 1 to column 12 inclusive.

``` r
well_summaries %>% 
  add_groupings(c("A01->A02:A12", "B01:H01->B02:H12"), nrow = 8, ncol = 12)
#> # A tibble: 96 x 6
#>    well      A        mu lambda group        is_ref
#>    <chr> <dbl>     <dbl>  <dbl> <chr>        <lgl> 
#>  1 A01    2.13 0.0000764 15652. A01->A02:A12 TRUE  
#>  2 A02    2.36 0.0000817 14799. A01->A02:A12 FALSE 
#>  3 A03    2.34 0.0000783 14045. A01->A02:A12 FALSE 
#>  4 A04    1.97 0.0000616 19335. A01->A02:A12 FALSE 
#>  5 A05    1.96 0.0000671 20799. A01->A02:A12 FALSE 
#>  6 A06    1.89 0.0000633 21611. A01->A02:A12 FALSE 
#>  7 A07    2.33 0.0000837  1057. A01->A02:A12 FALSE 
#>  8 A08    2.32 0.0000830 15236. A01->A02:A12 FALSE 
#>  9 A09    2.35 0.000100    926  A01->A02:A12 FALSE 
#> 10 A10    2.34 0.0000906  3155. A01->A02:A12 FALSE 
#> # … with 86 more rows
```

A more complex example (using the internal fxn for demonstration of the
parsing that goes on under the hood of `add_groupings()`)

``` r
growr:::parse_grouping('p1!A01,B01:B04,H03->A01:H12', 8 , 12)
#> $plate
#> [1] "p1"
#> 
#> $group
#> [1] "A01,B01:B04,H03->A01:H12"
#> 
#> $well
#>  [1] "A01" "B01" "B02" "B03" "B04" "H03" "C01" "D01" "E01" "F01" "G01"
#> [12] "H01" "A02" "C02" "D02" "E02" "F02" "G02" "H02" "A03" "C03" "D03"
#> [23] "E03" "F03" "G03" "A04" "C04" "D04" "E04" "F04" "G04" "H04" "A05"
#> [34] "B05" "C05" "D05" "E05" "F05" "G05" "H05" "A06" "B06" "C06" "D06"
#> [45] "E06" "F06" "G06" "H06" "A07" "B07" "C07" "D07" "E07" "F07" "G07"
#> [56] "H07" "A08" "B08" "C08" "D08" "E08" "F08" "G08" "H08" "A09" "B09"
#> [67] "C09" "D09" "E09" "F09" "G09" "H09" "A10" "B10" "C10" "D10" "E10"
#> [78] "F10" "G10" "H10" "A11" "B11" "C11" "D11" "E11" "F11" "G11" "H11"
#> [89] "A12" "B12" "C12" "D12" "E12" "F12" "G12" "H12"
#> 
#> $is_ref
#>  [1]  TRUE  TRUE  TRUE  TRUE  TRUE  TRUE FALSE FALSE FALSE FALSE FALSE
#> [12] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
#> [23] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
#> [34] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
#> [45] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
#> [56] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
#> [67] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
#> [78] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
#> [89] FALSE FALSE FALSE FALSE FALSE FALSE FALSE FALSE
```

#### Relatively

Rescale you data by group, for example:

``` r
well_summaries %>% 
  add_groupings(c("A01->A02:A12", "B01:H01->B02:H12"), nrow = 8, ncol = 12) %>% 
  group_by(group) %>% 
  mutate(relative_mu = relatively(mu, is_ref)) 
#> # A tibble: 96 x 7
#> # Groups:   group [2]
#>    well      A        mu lambda group        is_ref relative_mu
#>    <chr> <dbl>     <dbl>  <dbl> <chr>        <lgl>        <dbl>
#>  1 A01    2.13 0.0000764 15652. A01->A02:A12 TRUE         1    
#>  2 A02    2.36 0.0000817 14799. A01->A02:A12 FALSE        1.07 
#>  3 A03    2.34 0.0000783 14045. A01->A02:A12 FALSE        1.02 
#>  4 A04    1.97 0.0000616 19335. A01->A02:A12 FALSE        0.805
#>  5 A05    1.96 0.0000671 20799. A01->A02:A12 FALSE        0.878
#>  6 A06    1.89 0.0000633 21611. A01->A02:A12 FALSE        0.827
#>  7 A07    2.33 0.0000837  1057. A01->A02:A12 FALSE        1.09 
#>  8 A08    2.32 0.0000830 15236. A01->A02:A12 FALSE        1.09 
#>  9 A09    2.35 0.000100    926  A01->A02:A12 FALSE        1.31 
#> 10 A10    2.34 0.0000906  3155. A01->A02:A12 FALSE        1.19 
#> # … with 86 more rows
```

-----

## Installation

``` r
# install.packages("growr")
remotes::install_github("npjc/growr")
```

-----

<!-- ### Other examples -->

<!-- ```{r preprocess} -->

<!-- library(tidyverse) -->

<!-- library(mtpview) -->

<!-- library(growr) -->

<!-- data <- mtpview::mtp_example3 -->

<!-- one_well <- data %>% filter(well == 'A01') -->

<!-- one_well %>% -->

<!--     mutate(measure_pp = preprocess(measure, log_base = F, bg_subtract = F)) %>% -->

<!--     ggplot(aes(x = runtime)) + -->

<!--     geom_point(aes(y = measure)) + -->

<!--     geom_point(aes(y = measure_pp), size = 0.25, color = "red") + -->

<!--     theme_light() + -->

<!--     theme(axis.ticks.y = element_blank(), -->

<!--           axis.ticks.length = unit(.pt * 2, "pt"), -->

<!--           panel.grid.minor.y = element_blank(), -->

<!--           panel.grid.major.x = element_line(linetype = 'dashed', color = 'grey80'), -->

<!--           panel.grid.minor.x = element_line(linetype = 'dashed', color = 'grey90'), -->

<!--           axis.text.x = element_text(hjust = 0, vjust = 1, margin = margin(t = 0, r = 0, b = 0, l = 0, unit = "pt"))) -->

<!-- ``` -->
