data {

  int N;
  int sex [N];
  int age [N];
  int born [N];
  int alive [N];
  real married [N];
  real diff [N]; 

}

parameters {

  vector [2] beta [3];

}

model {

  vector [N] mu;


  for (i in 1:3) {
    beta[i, ] ~ normal(0, 1);
  }

  for (n in 1:N) {
  if (married[n] == 1) {
  if (diff[n] != -99) {

    mu[n] = beta[1, sex[n]] + 
            beta[2, sex[n]] * log(age[n] - 10) +
            beta[3, sex[n]] * diff[n];

    alive[n] ~ binomial_logit(born[n], mu[n]);

  }
  }
  }

}

