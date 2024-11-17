Homework 4
================

# Problem 1

A new device has been developed which allows patients to evaluate their
blood sugar levels. The most widely device currently on the market
yields widely variable results. The new device is evaluated by 25
patients having nearly the same distribution of blood sugar levels
yielding the following data:

125 123 117 123 115 112 128 118 124 111 116 109 125 120 113 123 112 118
121 118 122 115 105 118 131

1)  Is there significant ($\alpha = 0.05$) evidence that median blood
    sugar readings was less than 120 in the population from which the 25
    patients were selected? Use the sign test and report the test
    statistic and p-value.

``` r
blood_sugar = c(125, 123, 117, 123, 115, 112, 128, 118, 124,
111, 116, 109, 125, 120, 113, 123, 112, 118,
121, 118, 122, 115, 105, 118, 131)
```

``` r
median_hypothesis = 120
```

``` r
positive_sign = sum(blood_sugar > median_hypothesis)
negative_sign = sum(blood_sugar < median_hypothesis)
equal_sign = sum(blood_sugar == median_hypothesis)
```

``` r
n = positive_sign + negative_sign
```

``` r
S = negative_sign
```

``` r
p_value = pbinom(S, n, 0.5)
```

``` r
list(
  test_statistic_a = S,
  total_observations_a = n,
  p_value_a = p_value
)
```

    ## $test_statistic_a
    ## [1] 14
    ## 
    ## $total_observations_a
    ## [1] 24
    ## 
    ## $p_value_a
    ## [1] 0.8462719

2)  Is there significant ($\alpha = 0.05$) evidence that median blood
    sugar readings was less than 120 in the population from which the 25
    patients were selected? Use the Wilcoxon signed-rank test and report
    the test statistic and p-value.

``` r
difference = blood_sugar - median_hypothesis
```

``` r
wilcox_test = wilcox.test(blood_sugar, mu = median_hypothesis, alternative = "less", exact = FALSE)
```

``` r
list(
  test_statistic_b = wilcox_test$statistic,
  p_value_b = wilcox_test$p.value
)
```

    ## $test_statistic_b
    ##     V 
    ## 112.5 
    ## 
    ## $p_value_b
    ## [1] 0.1446559

# Problem 2

``` r
brain_df = readxl::read_excel("./data/brain.xlsx")
```

Human brains have a large frontal cortex with excessive metabolic
demands compared with the brains of other primates. However, the human
brain is also three or more times the size of the brains of other
primates. Is it possible that the metabolic demands of the human frontal
cortex are just an expected consequence of greater brain size? A data
file containing the measurements of glia-neuron ratio (an indirect
measure of the metabolic requirements of brain neurons) and the
log-transformed brain mass in nonhuman primates was provided to you
along with the following graph.

1)  Fit a regression model for the nonhuman data using
    $\ln{(\textrm{brain mass})}$ as a predictor. (Hint: Humans are “homo
    sapiens”.)

2)  Using the nonhuman primate relationship, what is the predicted
    glia-neuron ratio for humans, given their brain mass?

3)  Determine the most plausible range of values for the prediction.
    Which is more relevant for your prediction of human glia-neuron
    ratio: an interval for the predicted mean glia-neuron ratio at the
    given brain mass, or an interval for the prediction of a single new
    observation?

4)  Construct the 95% interval chosen in part (c). On the basis of your
    result, does the human brain have an excessive glia-neuron ratio for
    its mass compared with other primates?

5)  Considering the position of the human data point relative to those
    data used to generate the regression line (see graph above), what
    additional caution is warranted?

# Problem 3

For this problem, you will be using data `HeartDisease.csv`. The
investigator is mainly interested if there is an association between
‘total cost’ (in dollars) of patients diagnosed with heart disease and
the ‘number of emergency room (ER) visits’. Further, the model will need
to be adjusted for other factors, including ‘age’, ‘gender’, ‘number of
complications’ that arose during treatment, and ‘duration of treatment
condition’.

``` r
heart_disease_df = read_csv(file = "./data/heart_disease.csv")
```

    ## Rows: 788 Columns: 10
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (10): id, totalcost, age, gender, interventions, drugs, ERvisits, compli...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

1)  Provide a short description of the data set: what is the main
    outcome, main predictor and other important covariates. Also,
    generate appropriate descriptive statistics for all variables of
    interest (continuous and categorical) – no test required.

The dataset contains variables such as main outcome, main predictors,
and other important covariates. Under main outcome, total cost
represents the total cost incurred by patients diagnosed with heart
disease in dollars. Under main predictor, ERvisits represents the number
of emergency room visits by each patients. Lastly, under other important
covariates, id assigns a specific number for each patients, age
represents patient age, gender represents patient gender, complications
represents the number of complications during treatment, and duration
represents the duration for which the patient was treated for their
condition.

2)  Investigate the shape of the distribution for variable `totalcost`
    and try different transformations, if needed.

3)  Create a new variable called `comp_bin` by dichotomizing
    ‘complications’: 0 if no complications, and 1 otherwise.

4)  Based on your decision in part (b), fit a simple linear regression
    (SLR) between the original or transformed `totalcost` and predictor
    `ERvisits`. This includes a scatterplot and results of the
    regression, with appropriate comments on significance and
    interpretation of the slope.

5)  Fit a multiple linear regression (MLR) with `comp_bin` and
    `ERvisits` as predictors.

    1)  Test if `comp_bin` is an effect modifier of the relationship
        between `totalcost` and `ERvisits`. Comment.

    2)  Test if `comp_bin` is a confounder of the relationship between
        `totalcost` and `ERvisits`. Comment.

    3)  Decide if `comp_bin` should be included along with `ERvisits`.
        Why or why not?

6)  Use your choice of model in part (e) and add additional covariates
    (age, gender, and duration of treatment).

    1)  Fit a MLR, show the regression results and comment.

    2)  Compare the SLR and MLR models. Which model would you use to
        address the investigator’s objective and why?
