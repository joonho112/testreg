data {
  int<lower=1> N;           // number of studies
  int<lower=1> K;           // number of predictors (including intercept)
  vector[N] y;              // observed effect sizes
  vector<lower=0>[N] se;    // known standard errors
  matrix[N, K] X;           // design matrix
}

parameters {
  vector[K] beta;           // regression coefficients
  real<lower=0> tau;        // between-study SD
  vector[N] u;              // study-level random effects
}

model {
  // Priors
  beta ~ normal(0, 10);
  tau ~ cauchy(0, 1);

  // Random effects
  u ~ normal(0, tau);

  // Likelihood
  y ~ normal(X * beta + u, se);
}

generated quantities {
  vector[N] y_rep;          // posterior predictive replications
  vector[N] log_lik;        // pointwise log-likelihood for LOO-CV

  for (i in 1:N) {
    real mu_i = dot_product(X[i], beta) + u[i];
    y_rep[i] = normal_rng(mu_i, se[i]);
    log_lik[i] = normal_lpdf(y[i] | mu_i, se[i]);
  }
}
