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
    ##   [1] 0.237018112 0.623506880 0.292517486 0.968698485 0.456770548 0.749781001
    ##   [7] 0.840043699 0.310801116 0.180670677 0.565680088 0.995026020 0.828104344
    ##  [13] 0.418499358 0.750775266 0.548694443 0.678795292 0.571011954 0.010181146
    ##  [19] 0.826980949 0.526995589 0.332982532 0.773891651 0.064459604 0.948635866
    ##  [25] 0.223008466 0.389200616 0.226575126 0.762049336 0.373504635 0.487151563
    ##  [31] 0.563849294 0.036831445 0.704221076 0.774780552 0.623493522 0.996611000
    ##  [37] 0.765324825 0.211550229 0.734669519 0.316579730 0.258452893 0.655104850
    ##  [43] 0.258304561 0.121085369 0.233998500 0.232011511 0.465138941 0.409628687
    ##  [49] 0.318469284 0.606378619 0.458862377 0.274962144 0.137000096 0.327608522
    ##  [55] 0.058706418 0.433050623 0.003812823 0.092886606 0.386376726 0.785005480
    ##  [61] 0.428341276 0.332316671 0.113537324 0.563488243 0.907680600 0.640578388
    ##  [67] 0.605662972 0.740360798 0.104989360 0.203121742 0.224203156 0.474117310
    ##  [73] 0.568945827 0.047350083 0.854680301 0.185469161 0.649650320 0.440453520
    ##  [79] 0.028243005 0.969700802 0.054542175 0.840667525 0.612728218 0.948397934
    ##  [85] 0.403146943 0.154565744 0.605891719 0.241115337 0.857688148 0.288146277
    ##  [91] 0.452893718 0.015985471 0.948823802 0.613388898 0.529760653 0.067116602
    ##  [97] 0.109371318 0.291559461 0.939051790 0.901246177
    ## 
    ## $mat
    ##      [,1] [,2] [,3] [,4]
    ## [1,]    1    2    3    4
    ## [2,]    5    6    7    8
    ## 
    ## $summary
    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    ## -3.74985 -0.61258  0.02501  0.01107  0.69296  2.97161

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
    ## -3.74985 -0.61258  0.02501  0.01107  0.69296  2.97161

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

    ##  [1]  0.8702825 11.4166389  2.5593967 12.3490863  4.8276847  9.3092421
    ##  [7] -0.1354360 13.8037681  3.2822895 -0.2209559  8.9545666  6.4190480
    ## [13]  9.2453402 -0.4752642  3.2130649 -2.3455283  5.7460191  7.1565182
    ## [19] -0.4028411  5.4184160

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
    ## 1 -0.340  4.85

``` r
mean_and_sd(list_norm[["b"]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  5.05  4.75

``` r
mean_and_sd(list_norm[["c"]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -1.14  7.94

``` r
mean_and_sd(list_norm[["d"]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 0.781  7.97

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
    ## 1 -0.340  4.85
    ## 
    ## [[2]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  5.05  4.75
    ## 
    ## [[3]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -1.14  7.94
    ## 
    ## [[4]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 0.781  7.97

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

``` r
listcol_df = 
  tibble(
    name = c("a", "b", "c", "d"), #need to have same # of things in name column and ilst_norm columns
    samp = list_norm
  )

listcol_df #output shows that there are 20 numbers in each row
```

    ## # A tibble: 4 × 2
    ##   name  samp        
    ##   <chr> <named list>
    ## 1 a     <dbl [20]>  
    ## 2 b     <dbl [20]>  
    ## 3 c     <dbl [20]>  
    ## 4 d     <dbl [20]>

``` r
listcol_df |> 
  filter(name %in% c("a", "b")) #just get the first 2 rows
```

    ## # A tibble: 2 × 2
    ##   name  samp        
    ##   <chr> <named list>
    ## 1 a     <dbl [20]>  
    ## 2 b     <dbl [20]>

``` r
listcol_df |> 
  select(-samp)
```

    ## # A tibble: 4 × 1
    ##   name 
    ##   <chr>
    ## 1 a    
    ## 2 b    
    ## 3 c    
    ## 4 d

``` r
mean_and_sd(listcol_df[["samp"]][["a"]])
```

    ## # A tibble: 1 × 2
    ##     mean    sd
    ##    <dbl> <dbl>
    ## 1 -0.340  4.85

``` r
mean_and_sd(listcol_df[["samp"]][["b"]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  5.05  4.75

``` r
#use map to do the same thing
map(listcol_df[["samp"]], mean_and_sd) #mapping across samp column that was extracted from listcol_df and calculate mean_and_sd
```

    ## $a
    ## # A tibble: 1 × 2
    ##     mean    sd
    ##    <dbl> <dbl>
    ## 1 -0.340  4.85
    ## 
    ## $b
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  5.05  4.75
    ## 
    ## $c
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -1.14  7.94
    ## 
    ## $d
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 0.781  7.97

``` r
#Difference between map and map_dbl
  #map by default always returns a list
  #map_dbl makes the list of numbers into vectors (simplified)
```

Add a list column

``` r
listcol_df |> 
  mutate(output = map(samp, mean_and_sd)) #samp is a column in the dataframe (no need to extract), so map across column samp, and compute mean and sd and add a new column 
```

    ## # A tibble: 4 × 3
    ##   name  samp         output          
    ##   <chr> <named list> <named list>    
    ## 1 a     <dbl [20]>   <tibble [1 × 2]>
    ## 2 b     <dbl [20]>   <tibble [1 × 2]>
    ## 3 c     <dbl [20]>   <tibble [1 × 2]>
    ## 4 d     <dbl [20]>   <tibble [1 × 2]>

``` r
listcol_df |> 
  mutate(output = map(samp, mean_and_sd),
  iqr = map_dbl(samp, IQR))
```

    ## # A tibble: 4 × 4
    ##   name  samp         output             iqr
    ##   <chr> <named list> <named list>     <dbl>
    ## 1 a     <dbl [20]>   <tibble [1 × 2]>  5.36
    ## 2 b     <dbl [20]>   <tibble [1 × 2]>  8.41
    ## 3 c     <dbl [20]>   <tibble [1 × 2]> 12.0 
    ## 4 d     <dbl [20]>   <tibble [1 × 2]> 13.6

``` r
listcol_df |> 
  mutate(output = map(samp, mean_and_sd),
  iqr = map_dbl(samp, IQR)) |> 
  select(-samp) |> 
  unnest(output) #similar to bindrows fx to df -- taking a nested  df and expanding it
```

    ## # A tibble: 4 × 4
    ##   name    mean    sd   iqr
    ##   <chr>  <dbl> <dbl> <dbl>
    ## 1 a     -0.340  4.85  5.36
    ## 2 b      5.05   4.75  8.41
    ## 3 c     -1.14   7.94 12.0 
    ## 4 d      0.781  7.97 13.6
