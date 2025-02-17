README
================
Ramzan Kamoto
2025-01-31

# Risk Parity Portfolio construction in SA: README

## Purpose

This is the host platform for my Fin metrics essay. Initially I wanted
to work on active share and fund flows but i was scrambling for ideas so
I switched to something where code was available. The essay was written
using rticles elsevier template. This is because after many attempts,
Texevier would just not work for me. I try to mimic the texevier format
as far as possible. I trust this is okay.

In this essay I attempt to construct a risk parity portfolio and compare
it with a tangency portfolio as done by Palomar and T.Souza. Essentially
I undertake a conceptual replication in that I take their ideas, code
and packages but apply it to SA data. The data used was the J200 datset
from fmxdat. In the Palomar vignette, they use FAANG companies to
illustrate risk parity construction. Similarly, I take the top 10
tickers on the last date and use those for the rest of my analysis.

Some things I was able to do well and others I struggled. First, I was
able to get nice plots showing the weight and risk contribution under
risk parity portfolio conditions. I was also able to compare risk parity
and tangency portfolio weights side by side. constructing the tangency
portfolio proved difficult. I initially tried to follow the Palomar
vignette but my portfolio failed to optimize. Then, I opted for CVXR
which worked but then I struggled to get the rolling weights stacked bar
chart. In the end I abandoned the tangency portfolio at this stage. In
the essay, I discuss based on what I have seen in the literature but not
with a plot of my own.

## Code issue

Very close to the submission time as I was tying my work together my
function to load data started to produce an error (Error in
`>.default`(date, start_date) : comparison (\>) is possible only for
atomic and list types)

I tried stack and google, some suggestions were that it is because of
lubridates date function being confused but I am not sure. It could just
be my machine, a few days ago i had the same issue and it worked again
on its own without any debugging. Please do review the code and the
plots that are unfortunately missing from my essay.

## Initializing

``` r
suppressPackageStartupMessages({
  pacman::p_load(tidyverse, RiskPortfolios, fitHeavyTail)
  library(lubridate)
  library(tbl2xts)
  library(riskParityPortfolio)
  library(portfolioBacktest)
  library(xts)
  library(fPortfolio)
  library(PerformanceAnalytics)
  library(fmxdat)
})
```

# Load Data

``` r
# Start_date <- ymd(20100103)
# J200 <- load_data(Start_date)
# return_mat <- prepare_return_matrix(J200)
# return_mat <- impute_missing_returns(return_mat, method = "Drawn_Distribution_Collective") %>% tbl_xts()
# 
```

# Table of statistics

``` r
# generate_table1(return_mat)
```

# Risk Contribution plot

``` r
# plot_risk_parity(return_mat)
```

# Parity vs Tangency weights

``` r
# plot_portfolio_weights(return_mat)
```

# Rolling risk parity

``` r
# plot_rolling_risk_parity(return_mat)
```

# Performance chart for risk parity index

``` r
# generate_performance_summary(return_mat)
```
