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
    ##   [1] 0.415256318 0.879321020 0.576550160 0.466759166 0.317570460 0.820237455
    ##   [7] 0.862383952 0.353566453 0.717124348 0.925225176 0.181969369 0.052452321
    ##  [13] 0.728023367 0.570878634 0.956135976 0.994372488 0.829450325 0.477759396
    ##  [19] 0.450519813 0.625846790 0.315334498 0.744778367 0.195207165 0.731707190
    ##  [25] 0.823625018 0.116735538 0.109690963 0.988849631 0.991428106 0.389062334
    ##  [31] 0.109889095 0.650244529 0.780841956 0.338689016 0.819773039 0.631593554
    ##  [37] 0.720838855 0.383982433 0.246427622 0.179713517 0.529602112 0.332653678
    ##  [43] 0.029176195 0.541682863 0.740331876 0.483493653 0.007201101 0.526000751
    ##  [49] 0.467171445 0.799567715 0.868170057 0.832291387 0.174504520 0.424321517
    ##  [55] 0.933010946 0.670458297 0.937907369 0.678695176 0.843978256 0.553905592
    ##  [61] 0.547348403 0.996791652 0.595067390 0.480226709 0.771425335 0.133522062
    ##  [67] 0.840390699 0.082265697 0.612561697 0.621982146 0.074985102 0.846590387
    ##  [73] 0.399449022 0.019698526 0.948087754 0.879296467 0.010278864 0.338245003
    ##  [79] 0.806988856 0.283375666 0.812879155 0.842355666 0.782582103 0.955007702
    ##  [85] 0.295273168 0.818133894 0.680931768 0.040137500 0.533555402 0.527733594
    ##  [91] 0.487725266 0.585382306 0.886216505 0.609139423 0.580390089 0.356098006
    ##  [97] 0.305638051 0.131982630 0.815922839 0.895177764
    ## 
    ## $mat
    ##      [,1] [,2] [,3] [,4]
    ## [1,]    1    2    3    4
    ## [2,]    5    6    7    8
    ## 
    ## $summary
    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    ## -3.87953 -0.61349  0.04089  0.02428  0.69168  4.42716

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
    ## -3.87953 -0.61349  0.04089  0.02428  0.69168  4.42716

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

    ##  [1]  4.78226058  3.84359296  7.40750896 -2.53747463 -1.88155979  8.59754616
    ##  [7] 10.25908444 -0.61256888 10.93303304  9.72986778  6.46310400  7.14679306
    ## [13] 11.13185430  7.19209185 -0.28332840  0.81982316  8.09076304  5.37587956
    ## [19] -0.05028668  7.81581769

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
    ##     mean    sd
    ##    <dbl> <dbl>
    ## 1 -0.890  4.69

``` r
mean_and_sd(list_norm[["b"]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  5.21  4.46

``` r
mean_and_sd(list_norm[["c"]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.70  8.83

``` r
mean_and_sd(list_norm[["d"]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.72  8.82

## Use a for loop

Create output list, and run a for loop

``` r
output = vector("list", length = 4) #creating an empty list with four spots

for (i in 1:4) {
  
  output[[i]] = mean_and_sd(list_norm[[i]])
  
}

output
```

    ## [[1]]
    ## # A tibble: 1 × 2
    ##     mean    sd
    ##    <dbl> <dbl>
    ## 1 -0.890  4.69
    ## 
    ## [[2]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  5.21  4.46
    ## 
    ## [[3]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.70  8.83
    ## 
    ## [[4]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.72  8.82

## Do the ame thing

but with `map` instead

``` r
output = map(list_norm, mean_and_sd)
```

let’s do a couple of other things

``` r
output = map(list_norm, median)

output = map_dbl(list_norm, IQR) #dbl simplifies the output from a list to something easier to see

output = map_dfr(list_norm, mean_and_sd) #map into dataframne


output = 
  map(list_norm, mean_and_sd) |> 
  bind_rows()
```

## List Columns
