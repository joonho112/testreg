## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  eval = FALSE
)


## ----load---------------------------------------------------------------------
library(testreg)
data(example_meta)
head(example_meta)
str(example_meta)


## ----fit-stan-----------------------------------------------------------------
fit_stan <- meta_reg_stan(
  yi   = "yi",
  vi   = "vi",
  mods = ~ dosage,
  data = example_meta,
  chains = 4,
  iter   = 2000,
  warmup = 1000
)
fit_stan


## ----fit-nimble---------------------------------------------------------------
fit_nimble <- meta_reg_nimble(
  yi   = "yi",
  vi   = "vi",
  mods = ~ dosage,
  data = example_meta,
  chains = 4,
  iter   = 2000,
  warmup = 1000
)
fit_nimble


## ----summaries----------------------------------------------------------------
cat("=== Stan Backend ===\n")
summary(fit_stan)

cat("\n=== NIMBLE Backend ===\n")
summary(fit_nimble)


## ----diagnostics--------------------------------------------------------------
diagnose(fit_stan)
diagnose(fit_nimble)


## ----ppc----------------------------------------------------------------------
pp_check(fit_stan, type = "density")
pp_check(fit_stan, type = "scatter")
pp_check(fit_stan, type = "stat")


## ----forest-------------------------------------------------------------------
plot(fit_stan, type = "forest")


## ----caterpillar--------------------------------------------------------------
plot(fit_stan, type = "caterpillar")


## ----trace--------------------------------------------------------------------
plot(fit_stan, type = "trace")


## ----density------------------------------------------------------------------
plot(fit_stan, type = "density")

