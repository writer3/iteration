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
    ##   [1] 0.574234507 0.008839984 0.393920990 0.571243033 0.721319533 0.051828393
    ##   [7] 0.307681497 0.265964056 0.710173151 0.189136842 0.661159823 0.551353844
    ##  [13] 0.264937052 0.337916756 0.486656805 0.838014188 0.497079470 0.583517743
    ##  [19] 0.549843510 0.525999057 0.985316370 0.632772462 0.777560173 0.485372651
    ##  [25] 0.991376063 0.951247060 0.515471329 0.550164690 0.686028015 0.740689649
    ##  [31] 0.323860465 0.039649726 0.405283807 0.919041029 0.088916601 0.220482127
    ##  [37] 0.669022155 0.366820238 0.615628324 0.136878409 0.271913971 0.708681630
    ##  [43] 0.307274395 0.896011308 0.312452146 0.003125973 0.425999362 0.084422982
    ##  [49] 0.027008670 0.241415621 0.697680165 0.686444014 0.553736870 0.271276459
    ##  [55] 0.409657620 0.970597506 0.091148637 0.858286103 0.027387975 0.592516005
    ##  [61] 0.986350505 0.967903937 0.075062756 0.212466084 0.340581625 0.878500554
    ##  [67] 0.426641670 0.651065345 0.680136235 0.008595546 0.257285563 0.818802365
    ##  [73] 0.484183020 0.045002596 0.577848932 0.313610138 0.493964204 0.074180519
    ##  [79] 0.075269665 0.268701612 0.819981759 0.246815394 0.111532780 0.108589829
    ##  [85] 0.556112216 0.357697061 0.256669096 0.302220099 0.841032383 0.078251305
    ##  [91] 0.730364119 0.678920141 0.720061529 0.595763769 0.235955000 0.968191337
    ##  [97] 0.567141205 0.766962893 0.391291161 0.500880446
    ## 
    ## $mat
    ##      [,1] [,2] [,3] [,4]
    ## [1,]    1    2    3    4
    ## [2,]    5    6    7    8
    ## 
    ## $summary
    ##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
    ## -3.679056 -0.734378 -0.002525 -0.040927  0.604840  3.041135

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

    ##      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
    ## -3.679056 -0.734378 -0.002525 -0.040927  0.604840  3.041135

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

    ##  [1]  1.4069297 10.7040498 10.0680905  7.7944997  8.4213080  8.4066734
    ##  [7]  2.0568680  1.5949259  4.9651075  0.1588657  5.2393008 -3.4189468
    ## [13]  5.5154412  5.0964993  3.5535094  2.7149219  4.5296100 13.2759599
    ## [19]  4.8086563 -1.8741250

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
    ## 1 0.600  5.45

``` r
mean_and_sd(list_norm[["b"]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  4.75  4.23

``` r
mean_and_sd(list_norm[["c"]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  1.50  11.1

``` r
mean_and_sd(list_norm[["d"]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  4.45  9.55

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
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 0.600  5.45
    ## 
    ## [[2]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  4.75  4.23
    ## 
    ## [[3]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  1.50  11.1
    ## 
    ## [[4]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  4.45  9.55

## Do the ame thing

but with `map` instead

``` r
output = map(list_norm, mean_and_sd)
```
