Iteration and List Columns
================

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
knitr::opts_chunk$set(
  fig.width = 6,
  fig.asp = .6,
  out.width = "90%"
)

theme_set(theme_minimal() + theme(legend.position = "bottom"))

options(
  ggplot2.continuous.colour = "viridis",
  ggplot2.continuous.fill = "viridis"
)

scale_colour_discrete = scale_colour_viridis_d
scale_fill_discrete = scale_fill_viridis_d
```

Load key packages

``` r
library(tidyverse)
library(rvest)
```

    ## 
    ## Attaching package: 'rvest'

    ## The following object is masked from 'package:readr':
    ## 
    ##     guess_encoding

## Here’s some lists

``` r
l = list(
  vec_numeric = 1:4,
  unif_sample = runif(100),
  mat = matrix(1:8, nrow = 2, ncol = 4, byrow = TRUE),
  summary = summary(rnorm(1000))
  )

l
```

    ## $vec_numeric
    ## [1] 1 2 3 4
    ## 
    ## $unif_sample
    ##   [1] 0.936113263 0.771509981 0.114223731 0.842866175 0.290783388 0.311900390
    ##   [7] 0.428277114 0.470946602 0.297424587 0.062995394 0.295897958 0.012935999
    ##  [13] 0.514550439 0.417612323 0.369012134 0.488644807 0.962725495 0.840847380
    ##  [19] 0.521514830 0.326404081 0.923265788 0.267125855 0.714836315 0.588723352
    ##  [25] 0.290848085 0.369884024 0.554328830 0.553005145 0.614057526 0.494127707
    ##  [31] 0.147261380 0.853896082 0.640150763 0.907963718 0.053604448 0.058194325
    ##  [37] 0.948004780 0.954252888 0.536712118 0.046302785 0.339752437 0.041947290
    ##  [43] 0.073763605 0.996149049 0.495699925 0.788455092 0.007907673 0.090633125
    ##  [49] 0.932995892 0.491698681 0.991544412 0.244703628 0.426536875 0.627059246
    ##  [55] 0.189336507 0.763548497 0.889626308 0.396396301 0.573835444 0.887232888
    ##  [61] 0.376768311 0.041979739 0.043622863 0.771561428 0.519278707 0.650772595
    ##  [67] 0.472554366 0.891095975 0.820591975 0.859384242 0.750727470 0.225201617
    ##  [73] 0.914788569 0.767409844 0.203374540 0.313977236 0.929895494 0.177525691
    ##  [79] 0.370129504 0.126671659 0.632125984 0.734443496 0.877303772 0.153318775
    ##  [85] 0.373851005 0.860664240 0.291585771 0.359391768 0.692842007 0.333269600
    ##  [91] 0.034984837 0.899416683 0.060085680 0.891706332 0.622574083 0.449586955
    ##  [97] 0.737940931 0.702126323 0.677604636 0.902682414
    ## 
    ## $mat
    ##      [,1] [,2] [,3] [,4]
    ## [1,]    1    2    3    4
    ## [2,]    5    6    7    8
    ## 
    ## $summary
    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    ## -3.14971 -0.65466 -0.03546 -0.01832  0.68462  3.15841

``` r
l$mat
```

    ##      [,1] [,2] [,3] [,4]
    ## [1,]    1    2    3    4
    ## [2,]    5    6    7    8

``` r
l[["mat"]][1, 3] #access list of matrix "mat", row 1, col 3
```

    ## [1] 3

``` r
l[[1]]
```

    ## [1] 1 2 3 4

``` r
l[[4]]
```

    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    ## -3.14971 -0.65466 -0.03546 -0.01832  0.68462  3.15841

Make a list that’s hopefully a bit more useful.

To transmit typing across multiple lines –\> on Mac shift+option +
highlight multiple lines + type

``` r
list_norm = 
  list(
    a = rnorm(20, 0, 5),
    b = rnorm(20, 4, 5),
    c = rnorm(20, 0, 10),
    d = rnorm(20, 4, 10)
  )

list_norm[["b"]]
```

    ##  [1]  0.9691726  5.2614060  1.2412517 11.6305469 12.4619125 10.8395631
    ##  [7]  2.7460464 13.3637867  3.5989413 10.2074523  2.7197695  4.1219888
    ## [13]  0.8338736  7.5393347 -0.8440625  9.5221086  2.3589939  5.4812792
    ## [19] 12.9768086 13.1160013

Let’s reuse the function we wrote last time.

``` r
mean_and_sd = function(x) {
  
  mean_x = mean(x)
  sd_x = sd(x)
  
  out_df = 
    tibble(
      mean = mean_x,
      sd = sd_x
    )
  
  return(out_df)
  
}
```

Let’s use the function to take mean and sd of all samples.

``` r
mean_and_sd(list_norm[["a"]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 0.817  4.24

``` r
mean_and_sd(list_norm[["b"]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  6.51  4.83

``` r
mean_and_sd(list_norm[["c"]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -3.13  7.59

``` r
mean_and_sd(list_norm[["d"]])
```

    ## # A tibble: 1 × 2
    ##     mean    sd
    ##    <dbl> <dbl>
    ## 1 -0.765  11.4
