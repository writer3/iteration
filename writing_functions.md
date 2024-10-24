Writing Functions
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

## writing my first function

as an example here’s a z-score computation

``` r
x_vec = rnorm(n = 25, mean = 10, sd = 3.5)

(x_vec - mean(x_vec)) / sd(x_vec)
```

    ##  [1]  0.08241721 -1.04757776 -1.07003046  0.46977272  0.29977054  0.34797300
    ##  [7] -1.56628053 -0.43117114 -0.27272548 -0.76787311  0.51550812  1.54817089
    ## [13]  1.09117613 -0.16001250 -0.34472455  1.36260951  0.24393413 -1.22835501
    ## [19] -0.56030633  0.34753274 -1.94024013  2.04729911 -0.22360898  1.45387026
    ## [25] -0.19712837

Now i’ll write a function to do this.

``` r
z_scores = function (x) {
  
  if (!is.numeric(x)) {
    stop("x needs to be numeric")
  }
  
  if (length(x) < 5) {
    stop("you need at least five nunmbers to compute the z score")
  }
    
  z = (x - mean(x)) / sd(x)
  
  return(z)
  
}

z_scores(x = x_vec)
```

    ##  [1]  0.08241721 -1.04757776 -1.07003046  0.46977272  0.29977054  0.34797300
    ##  [7] -1.56628053 -0.43117114 -0.27272548 -0.76787311  0.51550812  1.54817089
    ## [13]  1.09117613 -0.16001250 -0.34472455  1.36260951  0.24393413 -1.22835501
    ## [19] -0.56030633  0.34753274 -1.94024013  2.04729911 -0.22360898  1.45387026
    ## [25] -0.19712837

Does this always work? (check)

``` r
#error = TRUE means run the RMD even if there are errors
z_scores(x = 3) #cannot take a SD of one value
```

    ## Error in z_scores(x = 3): you need at least five nunmbers to compute the z score

``` r
z_scores(x = c("my", "name", "is", "jeff")) #cannot take mean of characters
```

    ## Error in z_scores(x = c("my", "name", "is", "jeff")): x needs to be numeric
