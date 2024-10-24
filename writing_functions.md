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
library(readxl)
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

    ##  [1] -1.40705222  0.65117758 -1.78527753 -1.57651931  0.33142542  1.31315613
    ##  [7]  0.13751122  1.31516304 -0.39170363  0.80665109 -1.33614074 -0.76922026
    ## [13] -0.09347642 -0.01581053  1.66498249 -1.07346726  0.15108253 -0.53704075
    ## [19]  0.11248354  0.69411197  1.70536332 -0.21624775 -0.38164743  1.08321098
    ## [25] -0.38271549

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

    ##  [1] -1.40705222  0.65117758 -1.78527753 -1.57651931  0.33142542  1.31315613
    ##  [7]  0.13751122  1.31516304 -0.39170363  0.80665109 -1.33614074 -0.76922026
    ## [13] -0.09347642 -0.01581053  1.66498249 -1.07346726  0.15108253 -0.53704075
    ## [19]  0.11248354  0.69411197  1.70536332 -0.21624775 -0.38164743  1.08321098
    ## [25] -0.38271549

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

## A new function

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

mean_and_sd(x_vec)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  11.3  4.07

## Check stuff using a simulation

``` r
sim_df = 
  tibble(
    x = rnorm(30, 10, 5) #n=30, mean 10, sd 5
  )

sim_df |> 
  summarize(
    mean = mean(x),
    sd = sd(x)
  )
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  9.97  3.89

Simulation function to check sample mean and sd.

``` r
sim_mean_sd = function(samp_size = 30, true_mean = 10, true_sd = 5) {

    sim_df = 
    tibble(
      x = rnorm(samp_size, true_mean, true_sd)
    )
  
  out_df = 
    sim_df |> 
    summarize(
      mean = mean(x),
      sd = sd(x)
    )
  
  return(out_df)
}


sim_mean_sd(samp_size = 30, true_mean = 4, true_sd = 12)
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  2.94  14.1

``` r
sim_mean_sd(true_mean = 4, true_sd = 12, samp_size = 30) #this works too
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  4.82  13.0

``` r
sim_mean_sd(30, 16, 2) #this also works,assumes the order of size, mean, sd
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  16.6  2.01

``` r
sim_mean_sd() #can do this if default values are defined within function, can always be overridden 
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  9.85  4.98

## Revisit LoTR word

``` r
fellowship_df =
  read_excel("data/LotR_Words.xlsx", range = "B3:D6") |> 
  mutate(movie = "fellowship") |> 
  janitor::clean_names()

two_towers_df =
  read_excel("data/LotR_Words.xlsx", range = "F3:H6") |> 
  mutate(movie = "two_towers") |> 
  janitor::clean_names()

return_king_df =
  read_excel("data/LotR_Words.xlsx", range = "J3:L6") |> 
  mutate(movie = "return_king") |> 
  janitor::clean_names()
```

\#inputs = stuff that changed (movie name and cell ranges) \#body =
stuff that happened within the function \#output = what you want

Let’s do this using a function instead

``` r
lotr_import = function(cell_range, movie_title) {
  
  movie_df =
    read_excel("data/LotR_Words.xlsx", range = cell_range) |> 
    mutate(movie = movie_title) |> 
    janitor::clean_names() |> 
    pivot_longer(
      female:male,
      names_to = "sex",
      values_to = "words"
    ) |> 
    select(movie, everything())
    
  return(movie_df)
  
}


lotr_import(cell_range = "B3:D6", movie_title = "fellowship")  #one example
```

    ## # A tibble: 6 × 4
    ##   movie      race   sex    words
    ##   <chr>      <chr>  <chr>  <dbl>
    ## 1 fellowship Elf    female  1229
    ## 2 fellowship Elf    male     971
    ## 3 fellowship Hobbit female    14
    ## 4 fellowship Hobbit male    3644
    ## 5 fellowship Man    female     0
    ## 6 fellowship Man    male    1995

``` r
lotr_df = 
  bind_rows(
    lotr_import("B3:D6", "fellowship"), #another example how to use this new function
    lotr_import("F3:H6", "two_towers"),
    lotr_import("J3:L6", "return_king"),
  )
```

## NSDUH

``` r
nsduh_url = "http://samhda.s3-us-gov-west-1.amazonaws.com/s3fs-public/field-uploads/2k15StateFiles/NSDUHsaeShortTermCHG2015.htm"

nsduh_html = read_html(nsduh_url)

marj_table =
  nsduh_html |> 
  html_table() |> 
  nth(1) |> #take the first element(table) 
  slice(-1) |> 
  mutate(drug = "marj")

cocaine_table =
  nsduh_html |> 
  html_table() |> 
  nth(4) |> #take the 4th element(table) 
  slice(-1) |> 
  mutate(drug = "cocaine")

heroin_table =
  nsduh_html |> 
  html_table() |> 
  nth(5) |> #take the 5th element(table) 
  slice(-1) |> 
  mutate(drug = "heroin")
```

Let’s do this using a function instead

``` r
nsduh_table_format = function(html, table_num, table_name) {
  
  out_table =
    html |> 
    html_table() |> 
    nth(table_num) |> 
    slice(-1) |> 
    mutate(drug = table_name) |> 
    select(-contains("P Value"))
  
  return(out_table)
  
}

bind_rows(
  nsduh_table_format(html = nsduh_html, 1, "marj"),
  nsduh_table_format(html = nsduh_html, 4, "cocaine"),
  nsduh_table_format(html = nsduh_html, 5, "heroin")
)
```

    ## # A tibble: 168 × 12
    ##    State `12+(2013-2014)` `12+(2014-2015)` `12-17(2013-2014)` `12-17(2014-2015)`
    ##    <chr> <chr>            <chr>            <chr>              <chr>             
    ##  1 Tota… 12.90a           13.36            13.28b             12.86             
    ##  2 Nort… 13.88a           14.66            13.98              13.51             
    ##  3 Midw… 12.40b           12.76            12.45              12.33             
    ##  4 South 11.24a           11.64            12.02              11.88             
    ##  5 West  15.27            15.62            15.53a             14.43             
    ##  6 Alab… 9.98             9.60             9.90               9.71              
    ##  7 Alas… 19.60a           21.92            17.30              18.44             
    ##  8 Ariz… 13.69            13.12            15.12              13.45             
    ##  9 Arka… 11.37            11.59            12.79              12.14             
    ## 10 Cali… 14.49            15.25            15.03              14.11             
    ## # ℹ 158 more rows
    ## # ℹ 7 more variables: `18-25(2013-2014)` <chr>, `18-25(2014-2015)` <chr>,
    ## #   `26+(2013-2014)` <chr>, `26+(2014-2015)` <chr>, `18+(2013-2014)` <chr>,
    ## #   `18+(2014-2015)` <chr>, drug <chr>
