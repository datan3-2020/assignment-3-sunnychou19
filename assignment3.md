Statistical assignment 3
================
Sunny Chou (119105)
12/02/2020

In this assignment we will explore political interest (*vote6*) and how
it changes over time.

## Read data

First we want to read and join the data for the first 7 waves of the
Understanding Society. (Wave 8 does not have a variable for political
interest). We only want five variables: personal identifier, sample
origin, sex, age and political interest. It is tedious to join all the
seven waves manually, and it makes sense to use a loop in this case.
Since you don’t yet know about iteration I’ll provide the code for you;
please see the explanation of the code here:
<http://abessudnov.net/dataanalysis3/iteration.html>.

The only thing you need to do for this code to work on your computer is
to provide a path to the directory where the data are stored on your
computer.

``` r
library(tidyverse)
library(data.table)
library(dplyr)
library(reshape2)

# data.table is faster compared to readr so we'll use it in this case (the function fread()). You need to install this package first to be able to run this code.

# create a vector with the file names and paths

files <- dir(
             # Select the folder where the files are stored.
             "C:/Users/user/Desktop/data/UKDA-6614-tab/tab",
             # Tell R which pattern you want present in the files it will display.
             pattern = "indresp",
             # We want this process to repeat through the entire folder.
             recursive = TRUE,
             # And finally want R to show us the entire file path, rather than just
             # the names of the individual files.
             full.names = TRUE)

# Select only files from the UKHLS.
files <- files[stringr::str_detect(files, "ukhls")]
files
```

    ## [1] "C:/Users/user/Desktop/data/UKDA-6614-tab/tab/ukhls_w1/a_indresp.tab"
    ## [2] "C:/Users/user/Desktop/data/UKDA-6614-tab/tab/ukhls_w2/b_indresp.tab"
    ## [3] "C:/Users/user/Desktop/data/UKDA-6614-tab/tab/ukhls_w3/c_indresp.tab"
    ## [4] "C:/Users/user/Desktop/data/UKDA-6614-tab/tab/ukhls_w4/d_indresp.tab"
    ## [5] "C:/Users/user/Desktop/data/UKDA-6614-tab/tab/ukhls_w5/e_indresp.tab"
    ## [6] "C:/Users/user/Desktop/data/UKDA-6614-tab/tab/ukhls_w6/f_indresp.tab"
    ## [7] "C:/Users/user/Desktop/data/UKDA-6614-tab/tab/ukhls_w7/g_indresp.tab"
    ## [8] "C:/Users/user/Desktop/data/UKDA-6614-tab/tab/ukhls_w8/h_indresp.tab"
    ## [9] "C:/Users/user/Desktop/data/UKDA-6614-tab/tab/ukhls_w9/i_indresp.tab"

``` r
# create a vector of variable names
vars <- c("memorig", "sex_dv", "age_dv", "vote6")

for (i in 1:7) {varsToSelect <- paste(letters[i], vars, sep = "_")  
# Create a vector of the variables with the correct prefix.
       varsToSelect <- c("pidp", varsToSelect) # Add pidp to this vector (no prefix for pidp)
                data <- fread(files[i], select = varsToSelect)
        if (i == 1) {all7 <- data  }
        else {all7 <- full_join(all7, data, by = "pidp") }
        rm(data)}   # Now we can remove data to free up the memory.
```

## Reshape data (20 points)

Now we have got the data from all 7 waves in the same data frame
**all7** in the wide format. Note that the panel is unbalanced, i.e. we
included all people who participated in at least one wave of the survey.
Reshape the data to the long format. The resulting data frame should
have six columns for six variables.

``` r
Long <- all7 %>%
gather(a_memorig:g_vote6, key = "variable", value = "value") %>%
separate(variable, into = c("wave", "variable"), sep = "_", extra = "merge") %>%
spread(key = variable, value = value)
```

## Filter and recode (20 points)

Now we want to filter the data keeping only respondents from the
original UKHLS sample for Great Britain (memorig == 1). We also want to
clean the variables for sex (recoding it to “male” or “female”) and
political interest (keeping the values from 1 to 4 and coding all
negative values as missing). Tabulate *sex* and *vote6* to make sure
your recodings were correct.

``` r
Long <- Long %>%
        filter(memorig == 1) %>%
        mutate(sex = case_when(sex_dv == 1 ~ 'Male', sex_dv == 2 ~ 'Female') ) %>%
        mutate(vote6 = case_when(between(vote6, -10,0) ~ 'missing', TRUE ~ as.character(vote6))) %>% na.omit

Long$vote6 <-as.numeric(Long$vote6)
```

    ## Warning: NAs introduced by coercion

``` r
Long  %>% count(sex)
```

    ## # A tibble: 2 x 2
    ##   sex         n
    ##   <chr>   <int>
    ## 1 Female 117665
    ## 2 Male   100342

``` r
Long %>% count(vote6)
```

    ## # A tibble: 5 x 2
    ##   vote6     n
    ##   <dbl> <int>
    ## 1     1 21660
    ## 2     2 70952
    ## 3     3 56133
    ## 4     4 52138
    ## 5    NA 17124

## Calculate mean political interest by sex and wave (10 points)

Political interest is an ordinal variable, but we will treat it as
interval and calculate mean political interest for men and women in each
wave.

``` r
meanVote6 <- Long %>% group_by(sex, wave) %>%
  summarise(MeanVote = mean(vote6, na.rm = TRUE))
        
meanVote6
```

    ## # A tibble: 14 x 3
    ## # Groups:   sex [2]
    ##    sex    wave  MeanVote
    ##    <chr>  <chr>    <dbl>
    ##  1 Female a         2.84
    ##  2 Female b         2.82
    ##  3 Female c         2.87
    ##  4 Female d         2.89
    ##  5 Female e         2.87
    ##  6 Female f         2.81
    ##  7 Female g         2.73
    ##  8 Male   a         2.53
    ##  9 Male   b         2.51
    ## 10 Male   c         2.54
    ## 11 Male   d         2.55
    ## 12 Male   e         2.51
    ## 13 Male   f         2.47
    ## 14 Male   g         2.42

## Reshape the data frame with summary statistics (20 points)

Your resulting data frame with the means is in the long format. Reshape
it to the wide format. It should look like this:

| sex\_dv | a | b | c | d | e | f | g |
| ------- | - | - | - | - | - | - | - |
| female  |   |   |   |   |   |   |   |
| male    |   |   |   |   |   |   |   |

In the cells of this table you should have mean political interest by
sex and wave.

Write a short interpretation of your findings.

``` r
meanvote <- meanVote6 %>% dcast(sex ~ wave) %>% na.omit()

meanvote <- meanvote %>% filter(!is.na(sex))

meanvote
```

    ##      sex        a        b        c        d        e        f        g
    ## 1 Female 2.839437 2.816370 2.874985 2.887006 2.865092 2.807873 2.728400
    ## 2   Male 2.527112 2.512143 2.544448 2.551704 2.507875 2.472188 2.415998

Political interests by sex have been consistent across waves, both sex
reaching a peak of disinterest with a general decreasing trend during
wave 3 and 4 and regressed thereafter.

## Estimate stability of political interest (30 points)

Political scientists have been arguing how stable the level of political
interest is over the life course. Imagine someone who is not interested
in politics at all so that their value of *vote6* is always 4. Their
level of political interest is very stable over time, as stable as the
level of political interest of someone who is always very interested in
politics (*vote6* = 1). On the other hand, imagine someone who changes
their value of *votes6* from 1 to 4 and back every other wave. Their
level of political interest is very unstable.

Let us introduce a measure of stability of political interest that is
going to be equal to the sum of the absolute values of changes in
political interest from wave to wave. Let us call this measure Delta. It
is difficult for me to typeset a mathematical formula in Markdown, but
I’ll explain this informally.

Imagine a person with the level of political interest that is constant
over time: {1, 1, 1, 1, 1, 1, 1}. For this person, Delta is zero.

Now imagine a person who changes once from “very interested in politics”
to “fairly interested in politics”: {1, 1, 1, 1, 2, 2, 2}. For them,
Delta = (1 - 1) + (1 - 1) + (1 - 1) + (2 - 1) + (2 - 2) + (2 - 2) = 1.

Now imagine someone who changes from “very interested in politics” to
“not at all interested” every other wave: {1, 4, 1, 4, 1, 4, 1}. Delta
= (4 - 1) + abs(1 - 4) + (4 - 1) + abs(1 - 4) + (4 - 1) + abs(1 - 4) = 3
\* 6 = 18.

Large Delta indicates unstable political interest. Delta = 0 indicates a
constant level of political interest.

Write the R code that does the following.

1.  To simplify interpretation, keep only the respondents with
    non-missing values for political interest in all seven waves.

<!-- end list -->

``` r
all7 <- all7 %>% na.omit()

Long2 <- all7 %>%
gather(a_memorig:g_vote6, key = "variable", value = "value") %>%
separate(variable, into = c("wave", "variable"), sep = "_", extra = "merge") %>%
  spread(key = variable, value = value)

Long2 <-Long2 %>% mutate(sex = case_when(sex_dv == 1 ~ 'male', sex_dv == 2 ~ 'female' )) %>% na.omit()
```

2.  Calculate Delta for each person in the data set.

<!-- end list -->

``` r
Long2 <- Long2 %>% group_by(pidp) %>%  mutate(prevvote = lag(vote6)) 

Long2 <- Long2 %>% replace(., is.na(.), 1)

Long2<-Long2 %>% group_by(pidp) %>% mutate(delta = abs(vote6 - prevvote))

longtable<- Long2 %>% group_by(pidp) %>% summarise(delta = sum(delta))

longtable
```

    ## # A tibble: 18,731 x 2
    ##        pidp delta
    ##       <int> <dbl>
    ##  1 68004087     3
    ##  2 68006127     3
    ##  3 68006807     4
    ##  4 68008847     7
    ##  5 68009527     3
    ##  6 68010887     7
    ##  7 68020407     6
    ##  8 68025847    11
    ##  9 68029927     6
    ## 10 68029931    19
    ## # ... with 18,721 more rows

3.  Calculate mean Delta for men and women.

<!-- end list -->

``` r
Longtable2 <-Long2 %>% mutate(sex = case_when(sex_dv == 1 ~ 'male', sex_dv == 2 ~ 'female' )) %>% group_by(sex) %>% summarise( 'Delta Mean' = mean (delta)) 

Longtable2
```

    ## # A tibble: 2 x 2
    ##   sex    `Delta Mean`
    ##   <chr>         <dbl>
    ## 1 female        0.782
    ## 2 male          0.882

4.1 Calculate mean Delta by age (at wave 1)

``` r
longtable3<- Long2 %>% group_by(pidp, age = (age_dv[wave == 'a']) ) %>% summarise('delta' = sum(delta))

longtable3
```

    ## # A tibble: 18,731 x 3
    ## # Groups:   pidp [18,731]
    ##        pidp   age delta
    ##       <int> <int> <dbl>
    ##  1 68004087    59     3
    ##  2 68006127    39     3
    ##  3 68006807    72     4
    ##  4 68008847    51     7
    ##  5 68009527    31     3
    ##  6 68010887    45     7
    ##  7 68020407    72     6
    ##  8 68025847    73    11
    ##  9 68029927    36     6
    ## 10 68029931    40    19
    ## # ... with 18,721 more rows

4.2 Plot the local polynomial curve showing the association between age
at wave 1 and mean Delta. You can use either **ggplot2** or the
*scatter.smooth()* function from base R.

``` r
library(ggplot2)
ggplot( longtable3, aes(x = age, y = delta)) + geom_smooth() + ggtitle('Delta by Age') + scale_x_continuous(breaks=c(15,25,35,45,55,65,75,85,95))
```

![](assignment3_files/figure-gfm/unnamed-chunk-11-1.png)<!-- -->

5.  Write a short interpretation of your findings.

Delta, denoting instability in political interests, regresses sharply
from the highest towards a stable period during 25 to 35. It will then
gradually regress until 60, and gradually increase until 85 where there
is a sharp increase from 85 to 95. However, it has to be noted that 85
to 95 accounts for a very limited portion of the population. See table
below.

``` r
table(longtable3$age)
```

    ## 
    ##  15  16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31  32  33  34 
    ##   7 233 148 143 143 145 127 149 156 184 200 214 240 261 339 278 297 321 292 304 
    ##  35  36  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54 
    ## 346 355 334 363 409 423 411 426 372 379 378 381 403 386 381 347 390 376 351 349 
    ##  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72  73  74 
    ## 327 359 338 376 308 320 403 398 410 357 304 323 262 239 238 247 219 212 181 160 
    ##  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90  91  92  93  95 
    ## 142 129 120 114  94  92  76  61  45  37  23  30  15  12   4   8   4   1   1   1
