Linear Models
================
Lectured by Jeff Goldsmith
2022-11-10

``` r
knitr::opts_chunk$set(message = FALSE, warning = FALSE)

library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
    ## ✔ ggplot2 3.3.6      ✔ purrr   0.3.4 
    ## ✔ tibble  3.1.8      ✔ dplyr   1.0.10
    ## ✔ tidyr   1.2.1      ✔ stringr 1.4.1 
    ## ✔ readr   2.1.2      ✔ forcats 0.5.2 
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(p8105.datasets)

set.seed(1)
```

**Check this [page](https://www.statology.org/t-test-linear-regression/)
for simple explanation of linear regression.**

## *Model Fitting*

-   `lm` = linear models (continuous outcome)

-   `glm` = generalized linear models (non-continuous outcome)

-   Use ***broom package*** for output

Load and clean `nyc_airbnb` dataset.

``` r
data("nyc_airbnb")

nyc_airbnb = 
  nyc_airbnb %>% 
  mutate(stars = review_scores_location / 2) %>% 
  rename(
    borough = neighbourhood_group,
    neighborhood = neighbourhood) %>% 
  filter(borough != "Staten Island") %>% # because Staten Island data is too small. Not meaningful to analyze that.
  select(price, stars, borough, neighborhood, room_type)
```

Fit a linear model to look at how **price** (outcome) changes with
**rating** and **borough**.

``` r
model = lm(price ~ stars + borough, data = nyc_airbnb)
# Regression equation: price = -70.41 + 31.99*stars + 40.50*Brooklyn + 90.25*Manhattan + 13.21*Queens

model 
```

    ## 
    ## Call:
    ## lm(formula = price ~ stars + borough, data = nyc_airbnb)
    ## 
    ## Coefficients:
    ##      (Intercept)             stars   boroughBrooklyn  boroughManhattan  
    ##           -70.41             31.99             40.50             90.25  
    ##    boroughQueens  
    ##            13.21

``` r
# Up to this point, we only get the coefficients of each variable.

summary(model) 
```

    ## 
    ## Call:
    ## lm(formula = price ~ stars + borough, data = nyc_airbnb)
    ## 
    ## Residuals:
    ##    Min     1Q Median     3Q    Max 
    ## -169.8  -64.0  -29.0   20.2 9870.0 
    ## 
    ## Coefficients:
    ##                  Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)       -70.414     14.021  -5.022 5.14e-07 ***
    ## stars              31.990      2.527  12.657  < 2e-16 ***
    ## boroughBrooklyn    40.500      8.559   4.732 2.23e-06 ***
    ## boroughManhattan   90.254      8.567  10.534  < 2e-16 ***
    ## boroughQueens      13.206      9.065   1.457    0.145    
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 181.5 on 30525 degrees of freedom
    ##   (9962 observations deleted due to missingness)
    ## Multiple R-squared:  0.03423,    Adjusted R-squared:  0.03411 
    ## F-statistic: 270.5 on 4 and 30525 DF,  p-value: < 2.2e-16

``` r
# Summary will give us a lot of information, such as standard error, t-value, p-value, r-squared, etc.

# To output the results in a tidy way, use broom::tidy.
model %>% 
  broom::tidy() %>% 
  mutate(
    term = str_replace(term, "borough", "Borough: ")) %>% 
  select(term, estimate, p.value) %>% 
  knitr::kable(digits = 2)
```

| term               | estimate | p.value |
|:-------------------|---------:|--------:|
| (Intercept)        |   -70.41 |    0.00 |
| stars              |    31.99 |    0.00 |
| Borough: Brooklyn  |    40.50 |    0.00 |
| Borough: Manhattan |    90.25 |    0.00 |
| Borough: Queens    |    13.21 |    0.15 |

In the example above, Bronx is the reference category according to
alphabetical order.  
What if we want to change the reference category?

``` r
model_chng_ref = 
  nyc_airbnb %>% 
  mutate(
    borough = fct_infreq(borough)) %>% # Now the most common category would be the reference group.
  lm(price ~ stars + borough, data = .)

model_chng_ref %>% 
  broom::tidy() %>% 
  mutate(
    term = str_replace(term, "borough", "Borough: ")) %>% 
  select(term, estimate, p.value) %>% 
  knitr::kable(digits = 2)
```

| term              | estimate | p.value |
|:------------------|---------:|--------:|
| (Intercept)       |    19.84 |     0.1 |
| stars             |    31.99 |     0.0 |
| Borough: Brooklyn |   -49.75 |     0.0 |
| Borough: Queens   |   -77.05 |     0.0 |
| Borough: Bronx    |   -90.25 |     0.0 |

##### `broom::glance`

-   `broom::tidy` gives us estimate, std.error, statistics (t-test?),
    and p-value.

-   `broom::glance` gives us r-squared, adj.r-squared, sigma, statistic,
    p-value, df, logLik, AIC, BIC, deviance, df.residual, n_obs

``` r
model %>% 
  broom::glance() %>%
  select(r.squared, p.value, AIC)
```

    ## # A tibble: 1 × 3
    ##   r.squared   p.value     AIC
    ##       <dbl>     <dbl>   <dbl>
    ## 1    0.0342 6.73e-229 404237.

## **Diagnostics**

The **fitted** (or predicted) values are the y-values that you would
expect for the given x-values according to the built regression model
(or visually, the best-fitting straight regression line).

To use linear regression, the following assumptions must be met:  
1) **Linearity**: The relationship between the independent and dependent
variables is linear. \* Check this assumption by examining a scatterplot
of x and y.

2)  **Homoscedasticity**: The variance of residual is the same for any
    value of X.

-   Check this assumption by examining the scatterplot of “residuals
    versus fits”; the variance of the residuals should be the same
    across all values of the x-axis. If the plot shows a pattern (e.g.,
    bowtie or megaphone shape), then variances are not consistent, and
    this assumption has not been met.

3)  **Independence**: Observations are independent of each other (not
    repeated by the same person, don’t live in same household, not
    siblings, etc.) There is not a relationship between the residuals
    and the variable; in other words, is independent of errors.

-   Check this assumption by examining a scatterplot of “residuals
    versus fits”; the correlation should be approximately 0. In other
    words, there should not look like there is a relationship.

4)  **Normality**: The residuals must be approximately normally
    distributed.

-   Check this assumption by examining a normal probability plot; the
    observations should be near the line. You can also examine a
    histogram of the residuals; it should be approximately normally
    distributed.

``` r
modelr::add_residuals(nyc_airbnb, model) %>% 
  ggplot(aes(x = stars, y = resid)) +
  geom_point()
```

![](Linear-Models_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
nyc_airbnb %>% 
  modelr::add_residuals(model) %>% 
  ggplot(aes(x = borough, y = resid)) +
  geom_violin() +
  ylim(-250, 250)
```

![](Linear-Models_files/figure-gfm/unnamed-chunk-5-2.png)<!-- -->

``` r
# From the plot, we can see that the variances are not constant. 
# Linear regression assumes constant residuals.. So this assumption is violated.
```

## Hypothesis Testing

one coefficient

``` r
model_chng_ref %>% 
  broom::tidy()
```

    ## # A tibble: 5 × 5
    ##   term            estimate std.error statistic   p.value
    ##   <chr>              <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)         19.8     12.2       1.63 1.04e-  1
    ## 2 stars               32.0      2.53     12.7  1.27e- 36
    ## 3 boroughBrooklyn    -49.8      2.23    -22.3  6.32e-109
    ## 4 boroughQueens      -77.0      3.73    -20.7  2.58e- 94
    ## 5 boroughBronx       -90.3      8.57    -10.5  6.64e- 26

``` r
H_null = lm(price ~ stars, data = nyc_airbnb)
H_alt = lm(price ~ stars + borough, data = nyc_airbnb)

anova(H_null, H_alt) %>% 
  broom::tidy()
```

    ## # A tibble: 2 × 7
    ##   term                    df.residual       rss    df   sumsq stati…¹    p.value
    ##   <chr>                         <dbl>     <dbl> <dbl>   <dbl>   <dbl>      <dbl>
    ## 1 price ~ stars                 30528    1.03e9    NA NA          NA  NA        
    ## 2 price ~ stars + borough       30525    1.01e9     3  2.53e7    256.  7.84e-164
    ## # … with abbreviated variable name ¹​statistic

## Room type by borough

Interactions?

``` r
model_int = 
  nyc_airbnb %>% 
  lm(price ~ stars + borough * room_type, data = .)

model_int %>% 
  broom::tidy() %>% 
  knitr::kable(digits = 3)
```

| term                                   | estimate | std.error | statistic | p.value |
|:---------------------------------------|---------:|----------:|----------:|--------:|
| (Intercept)                            |   13.130 |    18.282 |     0.718 |   0.473 |
| stars                                  |   21.752 |     2.424 |     8.972 |   0.000 |
| boroughBrooklyn                        |   52.844 |    14.944 |     3.536 |   0.000 |
| boroughManhattan                       |  108.362 |    14.913 |     7.266 |   0.000 |
| boroughQueens                          |   21.709 |    15.710 |     1.382 |   0.167 |
| room_typePrivate room                  |  -53.018 |    17.752 |    -2.987 |   0.003 |
| room_typeShared room                   |  -68.459 |    41.506 |    -1.649 |   0.099 |
| boroughBrooklyn:room_typePrivate room  |  -39.161 |    18.024 |    -2.173 |   0.030 |
| boroughManhattan:room_typePrivate room |  -71.567 |    18.001 |    -3.976 |   0.000 |
| boroughQueens:room_typePrivate room    |  -15.595 |    19.017 |    -0.820 |   0.412 |
| boroughBrooklyn:room_typeShared room   |  -37.255 |    42.894 |    -0.869 |   0.385 |
| boroughManhattan:room_typeShared room  |  -85.354 |    42.405 |    -2.013 |   0.044 |
| boroughQueens:room_typeShared room     |  -24.607 |    44.339 |    -0.555 |   0.579 |

Can we fit models by borough?

``` r
nyc_airbnb %>% 
  nest(df = -borough) %>% 
  mutate(
    models = map(.x = df, ~lm(price ~ stars + room_type, data = .x)),
    results = map(models, broom::tidy)) %>% 
  pull(results)
```

    ## [[1]]
    ## # A tibble: 4 × 5
    ##   term                  estimate std.error statistic  p.value
    ##   <chr>                    <dbl>     <dbl>     <dbl>    <dbl>
    ## 1 (Intercept)              90.1      15.2       5.94 5.73e- 9
    ## 2 stars                     4.45      3.35      1.33 1.85e- 1
    ## 3 room_typePrivate room   -52.9       3.57    -14.8  6.21e-41
    ## 4 room_typeShared room    -70.5       8.36     -8.44 4.16e-16
    ## 
    ## [[2]]
    ## # A tibble: 4 × 5
    ##   term                  estimate std.error statistic  p.value
    ##   <chr>                    <dbl>     <dbl>     <dbl>    <dbl>
    ## 1 (Intercept)              91.6      25.8       3.54 4.00e- 4
    ## 2 stars                     9.65      5.45      1.77 7.65e- 2
    ## 3 room_typePrivate room   -69.3       4.92    -14.1  1.48e-43
    ## 4 room_typeShared room    -95.0      11.3      -8.43 5.52e-17
    ## 
    ## [[3]]
    ## # A tibble: 4 × 5
    ##   term                  estimate std.error statistic   p.value
    ##   <chr>                    <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)               69.6     14.0       4.96 7.27e-  7
    ## 2 stars                     21.0      2.98      7.05 1.90e- 12
    ## 3 room_typePrivate room    -92.2      2.72    -34.0  6.40e-242
    ## 4 room_typeShared room    -106.       9.43    -11.2  4.15e- 29
    ## 
    ## [[4]]
    ## # A tibble: 4 × 5
    ##   term                  estimate std.error statistic   p.value
    ##   <chr>                    <dbl>     <dbl>     <dbl>     <dbl>
    ## 1 (Intercept)               95.7     22.2       4.31 1.62e-  5
    ## 2 stars                     27.1      4.59      5.91 3.45e-  9
    ## 3 room_typePrivate room   -124.       3.46    -35.8  9.40e-270
    ## 4 room_typeShared room    -154.      10.1     -15.3  2.47e- 52

``` r
nyc_airbnb %>% 
  nest(df = -borough) %>% 
  mutate(
    models = map(.x = df, ~lm(price ~ stars + room_type, data = .x)),
    results = map(models, broom::tidy)) %>% 
  select(borough, results) %>% 
  unnest(results)
```

    ## # A tibble: 16 × 6
    ##    borough   term                  estimate std.error statistic   p.value
    ##    <chr>     <chr>                    <dbl>     <dbl>     <dbl>     <dbl>
    ##  1 Bronx     (Intercept)              90.1      15.2       5.94 5.73e-  9
    ##  2 Bronx     stars                     4.45      3.35      1.33 1.85e-  1
    ##  3 Bronx     room_typePrivate room   -52.9       3.57    -14.8  6.21e- 41
    ##  4 Bronx     room_typeShared room    -70.5       8.36     -8.44 4.16e- 16
    ##  5 Queens    (Intercept)              91.6      25.8       3.54 4.00e-  4
    ##  6 Queens    stars                     9.65      5.45      1.77 7.65e-  2
    ##  7 Queens    room_typePrivate room   -69.3       4.92    -14.1  1.48e- 43
    ##  8 Queens    room_typeShared room    -95.0      11.3      -8.43 5.52e- 17
    ##  9 Brooklyn  (Intercept)              69.6      14.0       4.96 7.27e-  7
    ## 10 Brooklyn  stars                    21.0       2.98      7.05 1.90e- 12
    ## 11 Brooklyn  room_typePrivate room   -92.2       2.72    -34.0  6.40e-242
    ## 12 Brooklyn  room_typeShared room   -106.        9.43    -11.2  4.15e- 29
    ## 13 Manhattan (Intercept)              95.7      22.2       4.31 1.62e-  5
    ## 14 Manhattan stars                    27.1       4.59      5.91 3.45e-  9
    ## 15 Manhattan room_typePrivate room  -124.        3.46    -35.8  9.40e-270
    ## 16 Manhattan room_typeShared room   -154.       10.1     -15.3  2.47e- 52

``` r
nyc_airbnb %>% 
  filter(borough == "Bronx") %>% 
  lm(price ~ stars + room_type, data = .) %>% 
  broom::tidy()
```

    ## # A tibble: 4 × 5
    ##   term                  estimate std.error statistic  p.value
    ##   <chr>                    <dbl>     <dbl>     <dbl>    <dbl>
    ## 1 (Intercept)              90.1      15.2       5.94 5.73e- 9
    ## 2 stars                     4.45      3.35      1.33 1.85e- 1
    ## 3 room_typePrivate room   -52.9       3.57    -14.8  6.21e-41
    ## 4 room_typeShared room    -70.5       8.36     -8.44 4.16e-16
