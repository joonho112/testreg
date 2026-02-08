# ==============================================================================
# Interactive Demo: testreg
# Purpose: Step-by-step verification of all package functions
# Instructions: Run each section one at a time in RStudio
# ==============================================================================

# --- Section 1: Setup and Data ------------------------------------------------

library(testreg)

# Load the built-in example dataset
data(example_meta)
cat("Dataset dimensions:", nrow(example_meta), "studies x",
    ncol(example_meta), "variables\n\n")
print(example_meta)

cat("\nColumn summaries:\n")
print(summary(example_meta[, c("yi", "vi", "dosage", "duration")]))

# --- Section 2: Fit with Stan ------------------------------------------------

cat("\n=== Fitting intercept-only model with Stan ===\n")

fit_stan_int <- meta_reg_stan(
  yi   = "yi",
  vi   = "vi",
  mods = NULL,
  data = example_meta,
  chains = 2,
  iter   = 2000,
  warmup = 1000
)

print(fit_stan_int)

cat("\n=== Fitting model with dosage moderator (Stan) ===\n")

fit_stan_mod <- meta_reg_stan(
  yi   = "yi",
  vi   = "vi",
  mods = ~ dosage,
  data = example_meta,
  chains = 2,
  iter   = 2000,
  warmup = 1000
)

print(fit_stan_mod)

# --- Section 3: Fit with NIMBLE -----------------------------------------------

cat("\n=== Fitting intercept-only model with NIMBLE ===\n")

fit_nimble_int <- meta_reg_nimble(
  yi   = "yi",
  vi   = "vi",
  mods = NULL,
  data = example_meta,
  chains = 2,
  iter   = 2000,
  warmup = 1000
)

print(fit_nimble_int)

cat("\n=== Fitting model with dosage moderator (NIMBLE) ===\n")

fit_nimble_mod <- meta_reg_nimble(
  yi   = "yi",
  vi   = "vi",
  mods = ~ dosage,
  data = example_meta,
  chains = 2,
  iter   = 2000,
  warmup = 1000
)
print(fit_nimble_mod)

# --- Section 4: Compare Backends ---------------------------------------------

cat("\n=== Posterior Summary: Stan (dosage model) ===\n")
smry_stan <- summary(fit_stan_mod)

cat("\n=== Posterior Summary: NIMBLE (dosage model) ===\n")
smry_nimble <- summary(fit_nimble_mod)

cat("\nBoth summaries should show similar estimates for beta and tau.\n")

# --- Section 5: Diagnostics --------------------------------------------------

cat("\n=== Diagnostics: Stan ===\n")
diag_stan <- diagnose(fit_stan_mod)

cat("\n=== Diagnostics: NIMBLE ===\n")
diag_nimble <- diagnose(fit_nimble_mod)

# --- Section 6: Posterior Predictive Checks -----------------------------------

cat("\n=== PPC: Density overlay (Stan) ===\n")
print(pp_check(fit_stan_mod, type = "density"))

cat("\n=== PPC: Scatter plot (Stan) ===\n")
print(pp_check(fit_stan_mod, type = "scatter"))

cat("\n=== PPC: Test statistic (Stan) ===\n")
print(pp_check(fit_stan_mod, type = "stat"))

# --- Section 7: Visualization ------------------------------------------------

cat("\n=== Forest Plot ===\n")
print(plot(fit_stan_mod, type = "forest"))

cat("\n=== Caterpillar Plot ===\n")
print(plot(fit_stan_mod, type = "caterpillar"))

cat("\n=== Trace Plots ===\n")
print(plot(fit_stan_mod, type = "trace"))

cat("\n=== Posterior Density Plots ===\n")
print(plot(fit_stan_mod, type = "density"))

# --- Section 8: Edge Cases and Robustness -------------------------------------

# Test with multiple moderators
cat("\n=== Model with two moderators ===\n")
fit_multi <- meta_reg_stan(
  yi   = "yi",
  vi   = "vi",
  mods = ~ dosage + duration,
  data = example_meta,
  chains = 2,
  iter   = 2000,
  warmup = 1000
)
summary(fit_multi)
diagnose(fit_multi)

# Test summary with different credible interval widths
cat("\n=== 90% credible intervals ===\n")
summary(fit_stan_mod, prob = 0.90)

cat("\n=== 80% credible intervals ===\n")
summary(fit_stan_mod, prob = 0.80)

# Test summary with all parameters
cat("\n=== All parameters ===\n")
summary(fit_stan_mod, pars = "all")

cat("\n=== Demo complete! ===\n")
