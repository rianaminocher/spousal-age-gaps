# fit IRT model to mental health data

# 1. effect of the realized gap, i.e. focal - alter
# 2. effect of the divergence between realized and long
# 3. effect of the divergence between realized and short

# compile model

m <- stan_model("stan/m3-sad.stan")

# prep data

data <- list()

list_data <- list(data_PC_Curr_Married, 
                  data_WL_Curr_Married, 
                  data_WH_Curr_Married,
                  data_IH_Curr_Married)

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
  
  age_gap <- scale(age_gap)[, 1]
  age_gap[is.na(age_gap)] <- -99
  
  K <- cbind(d$Sad, d$Anxious, d$Nervous, d$Hopeless, d$Worthless, d$Tired)
  K[is.na(K)] <- -99
  
  edu <- scale(d$Edu)[, 1]
  
  n_edu_miss <- length(which(is.na(edu)))
  loc_edu_miss <- as.array(which(is.na(edu)))
  
  edu[is.na(edu)] <- -99
  
  data[[s]] <- list(N = d$N, 
                    Q = 6,
                    K = K,
                    A = max(d$Age),
                    sex = d$Sex, 
                    age = d$Age,
                    married = married,
                    diff = age_gap,
                    goods = scale(d$GoodsValues)[, 1],
                    edu = edu,
                    n_edu_miss = n_edu_miss, 
                    loc_edu_miss = loc_edu_miss)
  
}

# fit model
  
fit <- mclapply(data, 
                function(data) sampling(m,
                                        data = data,
                                        iter = iter, 
                                        seed = seed, 
                                        control = control,
                                        chains = chains, 
                                        cores = 4), 
                mc.cores = 48)

saveRDS(fit, "stanfits/fit-sad.rds")

# fit models to the difference between ideal and actualized
# we can only model when we have an overlap in the observations of long/married or short/married

list_data <- list(data_PC_Curr_Married, 
                  data_WL_Curr_Married, 
                  data_WH_Curr_Married, 
                  data_IH_Curr_Married)

list_data[[1]]$long <- data_PC_Long$Outcome
list_data[[2]]$long <- data_WL_Long$Outcome
list_data[[3]]$long <- data_WH_Long$Outcome
list_data[[4]]$long <- data_IH_Long$Outcome

list_data[[1]]$short <- data_PC_Short$Outcome
list_data[[2]]$short <- data_WL_Short$Outcome
list_data[[3]]$short <- data_WH_Short$Outcome
list_data[[4]]$short <- data_IH_Short$Outcome

data <- list()

for (s in 1:4) {
  
  d <- list_data[[s]]
  
  ideal_long <- rep(NA, d$N)
  ideal_short <- rep(NA, d$N)
  realized <- rep(NA, d$N)
  
  for (i in 1:d$N) {
    
    focal <- i
    
    married <- length(which(d$Outcome[i, ] == 1)) # store just for now
    long <- length(which(d$long[i, ] == 1)) # store just for now
    short <- length(which(d$short[i, ] == 1)) # store just for now
    
    if (married == 1) {
      alt <- which(d$Outcome[i, ] == 1)
      realized[i] <- d$AgeDiff[focal, alt]  
    }
    
    if (long > 0) {
      alt <- which(d$long[i, ] == 1)
      ideal_long[i] <- mean(d$AgeDiff[focal, alt])
    }
    
    if (short > 0) {
      alt <- which(d$short[i, ] == 1)
      ideal_short[i] <- mean(d$AgeDiff[focal, alt])
    }
    
  }  
  
  diff_long <- abs(realized - ideal_long)
  diff_long <- scale(diff_long)[, 1]
  diff_long[is.na(diff_long)] <- -99
  
  list_data[[s]]$diff_short <- abs(realized - ideal_short) # save this for the next model fit
  
  K <- cbind(d$Sad, d$Anxious, d$Nervous, d$Hopeless, d$Worthless, d$Tired)
  K[is.na(K)] <- -99
    
  edu <- scale(d$Edu)[, 1]
    
  n_edu_miss <- length(which(is.na(edu)))
  loc_edu_miss <- as.array(which(is.na(edu)))
    
  edu[is.na(edu)] <- -99
    
  data[[s]] <- list(N = d$N, 
                    Q = 6,
                    A = max(d$Age),
                    K = K,
                    sex = d$Sex, 
                    age = d$Age, 
                    diff = diff_long, 
                    goods = scale(d$GoodsValues)[, 1],
                    edu = edu,
                    n_edu_miss = n_edu_miss, 
                    loc_edu_miss = loc_edu_miss)
    
}

fit <- mclapply(data, 
                function(data) sampling(m,
                                        data = data,
                                        iter = iter, 
                                        seed = seed, 
                                        control = control,
                                        chains = chains, 
                                        cores = 4), 
                mc.cores = 48)

saveRDS(fit, "stanfits/fit-sad-long-div.rds")

# fit to short divergence
# keep same "data" 
# swap out the "diff" variable

for (s in 1:4) {
  
  data[[s]]$diff <- list_data[[s]]$diff_short
  data[[s]]$diff <- scale(data[[s]]$diff)[, 1]
  data[[s]]$diff[is.na(data[[s]]$diff)] <- -99
  
}

fit <- mclapply(data, 
                function(data) sampling(m,
                                        data = data,
                                        iter = iter, 
                                        seed = seed, 
                                        control = control,
                                        chains = chains, 
                                        cores = 4), 
                mc.cores = 48)

saveRDS(fit, "stanfits/fit-sad-short-div.rds")
