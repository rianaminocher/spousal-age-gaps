data {

  int N;
  int sex [N];
  int age [N];
  int babies [N];
  real married [N];
  real age_gap [N]; 

}

parameters {

  vector [2] beta [3];

  real <lower = 0> phi;

  real bob;

}

model {

  vector [N] mu;

  phi ~ exponential(1);

  bob ~ normal(0, 1);

  for (i in 1:3) {
    beta[i, ] ~ normal(0, 1);
  }

  for (n in 1:N) {
  if (married[n] == 1) {
  if (age_gap[n] != -99) {

    mu[n] = exp(beta[1, sex[n]] + 
                beta[2, sex[n]] * log(age[n] - 10) +
                beta[3, sex[n]] * age_gap[n]);

  }
  }

  else {

    mu[n] = exp(bob);

  }

  }

  babies ~ neg_binomial(mu * phi, phi);

}

