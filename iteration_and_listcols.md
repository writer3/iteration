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
    ##   [1] 0.65214512 0.80851314 0.19166511 0.92326354 0.52733675 0.56372918
    ##   [7] 0.94646228 0.58841281 0.80172494 0.93209355 0.73040028 0.07748368
    ##  [13] 0.32944273 0.91166126 0.78633698 0.72568661 0.91353494 0.80752717
    ##  [19] 0.90587022 0.21581246 0.54261216 0.69837641 0.97332499 0.68305545
    ##  [25] 0.35401212 0.45353475 0.10980450 0.29243761 0.45275121 0.47783053
    ##  [31] 0.42032601 0.98256238 0.61665388 0.79048907 0.25993648 0.82528017
    ##  [37] 0.20516720 0.08301680 0.18172892 0.49897025 0.39507834 0.36155326
    ##  [43] 0.04026967 0.52670660 0.80069596 0.35532216 0.74259434 0.53249950
    ##  [49] 0.35042338 0.27096166 0.34403004 0.09704454 0.75083475 0.75241172
    ##  [55] 0.90674012 0.45883927 0.53514862 0.28333829 0.88535926 0.22985306
    ##  [61] 0.89709881 0.21174821 0.22398873 0.92192349 0.94407337 0.86437765
    ##  [67] 0.12546361 0.39389185 0.60952669 0.12482073 0.06888921 0.65460390
    ##  [73] 0.30930541 0.04959462 0.76673640 0.88548804 0.02741130 0.49133372
    ##  [79] 0.73120745 0.24260274 0.58330778 0.14981363 0.60311060 0.35235574
    ##  [85] 0.27322476 0.63670652 0.41579516 0.01245878 0.81815972 0.39211504
    ##  [91] 0.62312944 0.07533711 0.43081763 0.84375956 0.82835007 0.42612371
    ##  [97] 0.77794890 0.44765223 0.27586016 0.91062405
    ## 
    ## $mat
    ##      [,1] [,2] [,3] [,4]
    ## [1,]    1    2    3    4
    ## [2,]    5    6    7    8
    ## 
    ## $summary
    ##     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
    ## -3.01175 -0.62164  0.01077  0.01908  0.65948  3.17421

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
    ## -3.01175 -0.62164  0.01077  0.01908  0.65948  3.17421

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

    ##  [1]  6.9957740  1.2228439  1.4336591  5.5385582  9.2311750 -0.1286056
    ##  [7] -1.2997259 -7.1354954  8.6781316  4.1379702  9.4634088 -0.5535114
    ## [13] -1.1059175  8.9767870  3.0690197  3.8281352  8.2425438 -3.1198029
    ## [19]  6.0916317  3.5553757

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
    ## 1 -1.39  5.73

``` r
mean_and_sd(list_norm[["b"]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.36  4.65

``` r
mean_and_sd(list_norm[["c"]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  1.02  11.3

``` r
mean_and_sd(list_norm[["d"]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  1.86  8.59

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
    ## 1 -1.39  5.73
    ## 
    ## [[2]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.36  4.65
    ## 
    ## [[3]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  1.02  11.3
    ## 
    ## [[4]]
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  1.86  8.59

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
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -1.39  5.73

``` r
mean_and_sd(listcol_df[["samp"]][["b"]])
```

    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.36  4.65

``` r
#use map to do the same thing
map(listcol_df[["samp"]], mean_and_sd) #mapping across samp column that was extracted from listcol_df and calculate mean_and_sd
```

    ## $a
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1 -1.39  5.73
    ## 
    ## $b
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  3.36  4.65
    ## 
    ## $c
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  1.02  11.3
    ## 
    ## $d
    ## # A tibble: 1 × 2
    ##    mean    sd
    ##   <dbl> <dbl>
    ## 1  1.86  8.59

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
    ## 1 a     <dbl [20]>   <tibble [1 × 2]>  5.84
    ## 2 b     <dbl [20]>   <tibble [1 × 2]>  7.54
    ## 3 c     <dbl [20]>   <tibble [1 × 2]> 17.8 
    ## 4 d     <dbl [20]>   <tibble [1 × 2]> 12.3

``` r
listcol_df |> 
  mutate(output = map(samp, mean_and_sd),
  iqr = map_dbl(samp, IQR)) |> 
  select(-samp) |> 
  unnest(output) #similar to bindrows fx to df -- taking a nested  df and expanding it
```

    ## # A tibble: 4 × 4
    ##   name   mean    sd   iqr
    ##   <chr> <dbl> <dbl> <dbl>
    ## 1 a     -1.39  5.73  5.84
    ## 2 b      3.36  4.65  7.54
    ## 3 c      1.02 11.3  17.8 
    ## 4 d      1.86  8.59 12.3

### NSDUH

``` r
nsduh_table_format = function(html, table_num) {
  
  out_table =
    html |> 
    html_table() |> 
    nth(table_num) |> 
    slice(-1) |> 
    select(-contains("P Value"))
  
  return(out_table)
  
}
```

We need to import the html, and then extract the correct tables.

``` r
nsduh_url = "http://samhda.s3-us-gov-west-1.amazonaws.com/s3fs-public/field-uploads/2k15StateFiles/NSDUHsaeShortTermCHG2015.htm"

nsduh_html = read_html(nsduh_url)
```

``` r
nsduh_table_format(html = nsduh_html, table_num = 1)
```

    ## # A tibble: 56 × 11
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
    ## # ℹ 46 more rows
    ## # ℹ 6 more variables: `18-25(2013-2014)` <chr>, `18-25(2014-2015)` <chr>,
    ## #   `26+(2013-2014)` <chr>, `26+(2014-2015)` <chr>, `18+(2013-2014)` <chr>,
    ## #   `18+(2014-2015)` <chr>

``` r
nsduh_table_format(html = nsduh_html, table_num = 4)
```

    ## # A tibble: 56 × 11
    ##    State `12+(2013-2014)` `12+(2014-2015)` `12-17(2013-2014)` `12-17(2014-2015)`
    ##    <chr> <chr>            <chr>            <chr>              <chr>             
    ##  1 Tota… 1.66a            1.76             0.60               0.64              
    ##  2 Nort… 1.94a            2.18             0.60               0.66              
    ##  3 Midw… 1.37             1.43             0.48               0.54              
    ##  4 South 1.45b            1.56             0.53               0.57              
    ##  5 West  2.03             2.05             0.82               0.85              
    ##  6 Alab… 1.23             1.22             0.42               0.41              
    ##  7 Alas… 1.54a            2.00             0.51               0.65              
    ##  8 Ariz… 2.25             2.29             1.01               0.85              
    ##  9 Arka… 0.93             1.07             0.41               0.48              
    ## 10 Cali… 2.14             2.16             0.89               0.94              
    ## # ℹ 46 more rows
    ## # ℹ 6 more variables: `18-25(2013-2014)` <chr>, `18-25(2014-2015)` <chr>,
    ## #   `26+(2013-2014)` <chr>, `26+(2014-2015)` <chr>, `18+(2013-2014)` <chr>,
    ## #   `18+(2014-2015)` <chr>

``` r
nsduh_table_format(html = nsduh_html, table_num = 5)
```

    ## # A tibble: 56 × 11
    ##    State `12+(2013-2014)` `12+(2014-2015)` `12-17(2013-2014)` `12-17(2014-2015)`
    ##    <chr> <chr>            <chr>            <chr>              <chr>             
    ##  1 Tota… 0.30             0.33             0.12               0.10              
    ##  2 Nort… 0.43a            0.54             0.13               0.13              
    ##  3 Midw… 0.30             0.31             0.11               0.10              
    ##  4 South 0.27             0.26             0.12               0.08              
    ##  5 West  0.25             0.29             0.13               0.11              
    ##  6 Alab… 0.22             0.27             0.10               0.08              
    ##  7 Alas… 0.70a            1.23             0.11               0.08              
    ##  8 Ariz… 0.32a            0.55             0.17               0.20              
    ##  9 Arka… 0.19             0.17             0.10               0.07              
    ## 10 Cali… 0.20             0.20             0.13               0.09              
    ## # ℹ 46 more rows
    ## # ℹ 6 more variables: `18-25(2013-2014)` <chr>, `18-25(2014-2015)` <chr>,
    ## #   `26+(2013-2014)` <chr>, `26+(2014-2015)` <chr>, `18+(2013-2014)` <chr>,
    ## #   `18+(2014-2015)` <chr>

Learning Assessment Make a table with drug names, table number, and
column with the associated tables

``` r
nsduh_df =
  tibble(
    drug = c("marijuana", "cocaine", "heroin"),
    table_n = c(1, 4, 5)
  ) |> 
  mutate(table = map(table_n, nsduh_table_format, html = nsduh_html)) |> 
  unnest(table)
```
