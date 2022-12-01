Linear Models
================
Lectured by Jeff Goldsmith
2022-11-10

``` r
knitr::opts_chunk$set(message = FALSE, warning = FALSE)

library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.2 ──
    ## ✔ ggplot2 3.4.0      ✔ purrr   0.3.5 
    ## ✔ tibble  3.1.8      ✔ dplyr   1.0.10
    ## ✔ tidyr   1.2.1      ✔ stringr 1.4.1 
    ## ✔ readr   2.1.3      ✔ forcats 0.5.2 
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()

``` r
library(p8105.datasets)

set.seed(1) # to ensure reproducibility
```

**Check this [page](https://www.statology.org/t-test-linear-regression/)
for simple explanation of linear regression.**

### Predictors in a linear model

- **Outcome** is **continuous**.

- **Continuous exposure** can be added to the model directly, but
  **categorical exposure** must create dummy variables.

## *Model Fitting*

- `lm` = linear models (continuous outcome)

- `glm` = generalized linear models (non-continuous outcome)

- Use ***broom package*** for output

Load and clean `nyc_airbnb` dataset.

``` r
data("nyc_airbnb")

nyc_airbnb = 
  nyc_airbnb %>% 
  # make 10 stars rating to 5 stars.
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
model_change_ref = 
  nyc_airbnb %>% 
  mutate(
    borough = fct_infreq(borough)) %>% # Now the most common category would be the reference group.
  lm(price ~ stars + borough, data = .)

model_change_ref %>% 
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

#### `broom::glance`

- `broom::tidy` gives us estimate, std.error, statistics
  (t-distribution - correlation coefficient use t-distribution as well),
  and p-value.

- `broom::glance` gives us r-squared, adj.r-squared, sigma, statistic,
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

**AIC** tests how well a model fits the data…

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

- Check this assumption by examining the scatterplot of “residuals
  versus fits”; the variance of the residuals should be the same across
  all values of the x-axis. If the plot shows a pattern (e.g., bowtie or
  megaphone shape), then variances are not consistent, and this
  assumption has not been met.

3)  **Independence**: Observations are independent of each other (not
    repeated by the same person, don’t live in same household, not
    siblings, etc.) There is no relationship between the residuals and
    the predictors; in other words, is independent of errors.

- Check this assumption by examining a scatterplot of “residuals versus
  fits”; the correlation should be approximately 0. In other words,
  there should not look like there is a relationship.

4)  **Normality**: The residuals must be approximately normally
    distributed.

- Check this assumption by examining a normal probability plot; the
  observations should be near the line. You can also examine a histogram
  of the residuals; it should be approximately normally distributed.

``` r
# Get residuals and look at them:
modelr::add_residuals(nyc_airbnb, model) %>% # in the () indicates which datsset and which model
  ggplot(aes(x = stars, y = resid)) +
  geom_point()
```

![](Linear-Models_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

``` r
# We should see mean zero & constant variance.
# From the plot, it seems like stars 4-5 have more variances. So constant variance assumption is violated here.

nyc_airbnb %>% 
  modelr::add_residuals(model) %>% 
  ggplot(aes(x = borough, y = resid)) +
  geom_violin() +
  # zoom in a little bit..
  ylim(-250, 250)
```

![](Linear-Models_files/figure-gfm/unnamed-chunk-5-2.png)<!-- -->

``` r
# From the plot, we can see that the variances are not constant. Distribution seems skewed.
# Linear regression assumes constant residuals.. So this assumption is violated.
```

## Hypothesis Testing

#### One coefficient: Example (stars)

``` r
model_change_ref %>% 
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
model_null = lm(price ~ stars, data = nyc_airbnb)
model_alt = lm(price ~ stars + borough, data = nyc_airbnb)

anova(model_null, model_alt) %>% 
  broom::tidy()
```

    ## # A tibble: 2 × 7
    ##   term                    df.residual       rss    df   sumsq stati…¹    p.value
    ##   <chr>                         <dbl>     <dbl> <dbl>   <dbl>   <dbl>      <dbl>
    ## 1 price ~ stars                 30528    1.03e9    NA NA          NA  NA        
    ## 2 price ~ stars + borough       30525    1.01e9     3  2.53e7    256.  7.84e-164
    ## # … with abbreviated variable name ¹​statistic

``` r
# p-value for H_alt is super small, so we should put "borough" in our model.
```

## Interaction

In the airbnb data, we might think that star ratings and room type
affects price differently in each borough. One way to allow this kind of
effect modification is through interaction terms:

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

This works, but the output takes time to think through. What does the
output mean? What is the effect of room type in each borough?

Alternatively, we can nest within boroughs and fit borough-specific
models associating price with rating and room type: The advantage of
doing this is that it makes the interpretation easier.

Fit models by borough:

``` r
# I can nest everything inside the airbnb dataset according to borough:
nest_df = 
  nyc_airbnb %>% 
    nest(df = -borough) %>% 
    mutate(
    # Create a new variable that map the linear regression function to each of the borough.
      models = map(.x = df, ~lm(price ~ stars + room_type, data = .x)),
    # Then, create another variable that shows the tidy results of the linear regression.
      results = map(models, broom::tidy)) %>% 
    pull(results)

# From the results, we can see that in Queens, private room is $69.25 less than an entire home/apt. 
# In Queens, shared room is $94.97 less than an entire home/apt. 
```

We can also unnest the “results”:

``` r
unnest = 
  nyc_airbnb %>% 
  nest(df = -borough) %>% 
  mutate(
    models = map(.x = df, ~lm(price ~ stars + room_type, data = .x)),
    results = map(models, broom::tidy)) %>% 
  select(borough, results) %>% 
  unnest(results)
```

After unnesting the results, we can pivot wider to make the table looks
nicer.

``` r
unnest %>% 
  select(borough, term, estimate) %>% 
  mutate(term = fct_inorder(term)) %>% 
  pivot_wider(
    names_from = term, 
    values_from = estimate) %>% 
  knitr::kable(digits = 3)
```

| borough   | (Intercept) |  stars | room_typePrivate room | room_typeShared room |
|:----------|------------:|-------:|----------------------:|---------------------:|
| Bronx     |      90.067 |  4.446 |               -52.915 |              -70.547 |
| Queens    |      91.575 |  9.654 |               -69.255 |              -94.973 |
| Brooklyn  |      69.627 | 20.971 |               -92.223 |             -105.839 |
| Manhattan |      95.694 | 27.110 |              -124.188 |             -153.635 |

A quick double check:

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

Yes!! It looks like everything looks neat!

## Binary Outcomes

Import, clean, and wrangle the *Washington Post* `50 cities homicide`
data for analysis.

``` r
baltimore_df = 
  read_csv("Data/homicide-data.csv") %>% 
  filter(city == "Baltimore") %>% 
  mutate(
    resolved = as.numeric(disposition == "Closed by arrest"), # make a new variable "resolved", if "closed by arrest" then resolved = 1.
    victim_age = as.numeric(victim_age),
    victim_race = fct_relevel(victim_race, "White")) %>% # make the race variable a factor and move "White" to the front of all races.
  select(resolved, victim_age, victim_race, victim_sex)
```

Using these data, we can fit a logistic regression for the binary
“resolved” outcome and victim demographics as predictors.

``` r
fit_logistic = 
  baltimore_df %>% 
  glm(resolved ~ victim_age + victim_race + victim_sex, data = . , family = binomial()) 

# Binomial distribution can be thought of as simply the probability of a SUCCESS or FAILURE outcome in an experiment or survey that is
# repeated multiple times. The binomial is a type of distribution that has two possible outcomes (the prefix “bi” means two, or twice).

# We use broom::tidy to make the output human readable:
fit_logistic %>% 
  broom::tidy() %>% 
  # create a new variable that shows the ORs
  mutate(OR = exp(estimate)) %>%
  select(term, log_OR = estimate, OR, p.value) %>% 
  knitr::kable(digits = 3)
```

| term                | log_OR |    OR | p.value |
|:--------------------|-------:|------:|--------:|
| (Intercept)         |  1.190 | 3.287 |   0.000 |
| victim_age          | -0.007 | 0.993 |   0.027 |
| victim_raceAsian    |  0.296 | 1.345 |   0.653 |
| victim_raceBlack    | -0.842 | 0.431 |   0.000 |
| victim_raceHispanic | -0.265 | 0.767 |   0.402 |
| victim_raceOther    | -0.768 | 0.464 |   0.385 |
| victim_sexMale      | -0.880 | 0.415 |   0.000 |

Homicides in which the victim is Black are substantially less likely to
be resolved that those in which the victim is white; for other races the
effects are not significant, possible due to small sample sizes.
Homicides in which the victim is male are significantly less like to be
resolved than those in which the victim is female. The effect of age is
statistically significant, but careful data inspections should be
conducted before interpreting too deeply.
