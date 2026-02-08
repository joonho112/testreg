# testreg

Bayesian meta-regression in R with **Stan** and **NIMBLE** backends.

## Overview

`testreg` fits random-effects meta-regression models of the form:

```
y_i = X_i * beta + u_i + e_i
u_i ~ N(0, tau^2)
e_i ~ N(0, sigma_i^2)   (known sampling variances)
```

Both backends return the same S3 object (`metareg_fit`), so you can switch between Stan and NIMBLE without changing downstream code.

## Installation

```r
# Install from GitHub
devtools::install_github("joonho112/test_reg_R_package")
```

### Backend setup

At least one backend is required:

```r
# Stan (recommended)
install.packages("cmdstanr", repos = c("https://stan-dev.r-universe.dev", getOption("repos")))
cmdstanr::install_cmdstan()

# NIMBLE
install.packages("nimble")
```

## Quick start

```r
library(testreg)
data(example_meta)

# Fit with Stan
fit <- meta_reg_stan(yi = "yi", vi = "vi", mods = ~ dosage, data = example_meta)

# Results
summary(fit)
diagnose(fit)
plot(fit, type = "forest")
pp_check(fit)
```

## Main functions

| Function | Description |
|---|---|
| `meta_reg_stan()` | Fit model via Stan (cmdstanr or rstan) |
| `meta_reg_nimble()` | Fit model via NIMBLE |
| `summary()` | Posterior means, medians, SD, credible intervals |
| `diagnose()` | Rhat, bulk ESS, tail ESS with convergence flags |
| `pp_check()` | Posterior predictive checks (density, scatter, stat) |
| `plot()` | Forest, caterpillar, trace, and density plots |

## Example dataset

`example_meta` contains 18 simulated studies with effect sizes (`yi`), sampling variances (`vi`), and two moderators (`dosage`, `duration`).

```r
data(example_meta)
head(example_meta)
#>      study     yi     vi    sei dosage duration
#> 1 Study_01 0.8484 0.0110 0.1049   13.5        9
#> 2 Study_02 0.0741 0.0391 0.1977   77.4       13
#> 3 Study_03 0.4453 0.1369 0.3700   71.0       11
```

## Package structure

```
testreg/
├── R/
│   ├── fit_stan.R
│   ├── fit_nimble.R
│   ├── diagnostics.R
│   ├── posterior_summary.R
│   ├── model_check.R
│   ├── plot_methods.R
│   ├── data.R
│   └── utils.R
├── inst/stan/
│   └── meta_regression.stan
├── data/
│   └── example_meta.rda
├── tests/testthat/
├── vignettes/
│   └── bayesian_meta_regression.Rmd
└── scripts/
    └── interactive_demo.R
```

## License

MIT
