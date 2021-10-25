# fit fertility model

# compile model

m <- stan_model("stan/m2-fert-simple.stan")

# prep data

data <- list()

list_data <- list(data_PC_Curr_Married, 
                  data_WL_Curr_Married, 
                  data_WH_Curr_Married, 
                  data_IH_Curr_Married)

age_center <- list() # save the mean/sd for the plots
age_scale <- list() 

# (age_gap - center) / scale is what the scale function does

for (s in 1:4) {
  
  d <- list_data[[s]]
  
  age_gap <- rep(NA, d$N)
  married <- rep(NA, d$N)
  
  for (i in 1:d$N) {
    focal <- i
    married[i] <- length(which(d$Outcome[i, ] == 1))
    
    if (married[i] == 1) {
      alt <- which(d$Outcome[i, ] == 1)
      age_gap[i] <- d$AgeDiff[focal, alt]  
    }
  }
  
  age_center[[s]] <- attr(scale(age_gap), "scaled:center")
  age_scale[[s]] <- attr(scale(age_gap), "scaled:scale")
  
  age_gap <- scale(age_gap)[, 1]
  age_gap[is.na(age_gap)] <- -99
  
  data[[s]] <- list(N = d$N, 
                    sex = d$Sex, 
                    age = d$Age,
                    babies = d$ChildrenAlive,
                    married = married,
                    age_gap = age_gap)
  
}

# check that when married is 1, age gap is always known 

any(data[[1]]$age_gap[data[[1]]$married == 1] == -99)
any(data[[2]]$age_gap[data[[2]]$married == 1] == -99)
any(data[[3]]$age_gap[data[[3]]$married == 1] == -99)
any(data[[4]]$age_gap[data[[4]]$married == 1] == -99)

# data is missing on babies for the fourth site, need to drop those individuals

drop <- which(is.na(data[[4]]$babies))

data[[4]]$N <- data[[4]]$N - length(drop)
data[[4]]$sex <- data[[4]]$sex[-c(drop)]
data[[4]]$age <- data[[4]]$age[-c(drop)]
data[[4]]$babies <- data[[4]]$babies[-c(drop)]
data[[4]]$married <- data[[4]]$married[-c(drop)]
data[[4]]$age_gap <- data[[4]]$age_gap[-c(drop)]

# fit model

fit <- mclapply(data, 
                function(d) sampling(m,
                                     data = d, 
                                     iter = iter, 
                                     seed = seed, 
                                     chains = chains, 
                                     control = control, 
                                     cores = 4), 
                mc.cores = 48)

saveRDS(fit, "stanfits/fit-fert.rds")

