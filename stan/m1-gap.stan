functions {
  matrix GP(int A, real kappa, real tau, real delta) {
    matrix[A, A] omega;

    for (a in 1:(A-1)) {
    for (b in (a+1):A) {
    omega[a, b] = kappa * exp(-tau * ((b-a)^2 / A^2));
    omega[b, a] = omega[a, b];
    }
    }

    for (i in 1:A) {
    omega[i, i] = 1;
    }

    return delta * cholesky_decompose(omega);
  }
}

data {

  int N;
  int A;
  int outcome [N, N];

  int sex [N];
  int age [N];

  vector [N] age_FM;
  vector [N] goods;
  vector [N] edu;

  int n_age_FM_miss;
  int loc_age_FM_miss [n_age_FM_miss];

  int n_edu_miss;
  int loc_edu_miss [n_edu_miss];

  matrix [N, N] age_diff;
  matrix [N, N] same_sex;

  real Z [3];

  int MissingFocal [N];

}

parameters {
  
  vector [A] theta_raw [2];

  real eta [2];
  real <lower = 0, upper = 1> kappa [2];
  real <lower = 0> tau [2];
  real <lower = 0> delta [2];

  vector [3] beta [2];

  real <lower = 0> sigma;

  vector [n_age_FM_miss] age_FM_impute;
  vector [n_edu_miss] edu_impute;

}

transformed parameters {

  vector [A] theta [2];
  vector [N] age_FM_full;
  vector [N] edu_full;

  for (s in 1:2) {
  theta[s] = eta[s] + GP(A, kappa[s], tau[s], delta[s])*theta_raw[s];
  }

  age_FM_full = age_FM;

  for (i in 1:n_age_FM_miss) {
  	age_FM_full[loc_age_FM_miss[i]] = age_FM_impute[i];
  }

  edu_full = edu;

  for (i in 1:n_edu_miss) {
    edu_full[loc_edu_miss[i]] = edu_impute[i];
  }

}

model {

  real mu;

  sigma ~ exponential(1);

  for (s in 1:2){
    theta_raw[s] ~ normal(0, 1);
    beta[s] ~ normal(0, 1);
  }

  age_FM_impute ~ normal(0, 1);
  edu_impute ~ normal(0, 1);

  eta ~ normal(0, 10);
  kappa ~ beta(12, 2);
  tau ~ exponential(1);
  delta ~ exponential(1);

  for (i in 1:N) {
    if(MissingFocal[i] == 0) {

  for (j in 1:N) {
    if (outcome[i, j] == 1) {
    if (i != j) {
        mu = theta[sex[i], age[i]] + beta[sex[i], 1] * age_FM_full[i] * Z[1] 
                                   + beta[sex[i], 2] * edu_full[i] * Z[2]
                                   + beta[sex[i], 3] * goods[i] * Z[3];
        age_diff[i, j] ~ normal(mu, sigma);

      }
      }
  }
  }

}
}

