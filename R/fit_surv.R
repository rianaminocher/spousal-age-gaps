# fit offspring survival model

# 1. spousal age gap (focal - alter)
# 2. absolute divergence long
# 3. absolute divergence short

# compile model

m <- stan_model("stan/m4-surv.stan")

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
  
  data[[s]] <- list(N = d$N, 
                    sex = d$Sex, 
                    age = d$Age,
                    alive = d$ChildrenAlive,
                    born = d$ChildrenBorn,
                    married = married,
                    diff = age_gap)
  
}

# check that when married is 1, age gap is always known 

any(data[[1]]$age_gap[data[[1]]$married == 1] == -99)
any(data[[2]]$age_gap[data[[2]]$married == 1] == -99)
any(data[[3]]$age_gap[data[[3]]$married == 1] == -99)
any(data[[4]]$age_gap[data[[4]]$married == 1] == -99)

# data is missing on babies for the fourth site, need to drop those individuals

drop <- which(is.na(data[[4]]$born))

data[[4]]$N <- data[[4]]$N - length(drop)
data[[4]]$sex <- data[[4]]$sex[-c(drop)]
data[[4]]$age <- data[[4]]$age[-c(drop)]
data[[4]]$born <- data[[4]]$born[-c(drop)]
data[[4]]$alive <- data[[4]]$alive[-c(drop)]
data[[4]]$married <- data[[4]]$married[-c(drop)]
data[[4]]$diff <- data[[4]]$diff[-c(drop)]

range(data[[4]]$diff[!data[[4]]$diff == -99])

# fit model

fit <- mclapply(data, 
                function(d) sampling(m,
                                     data = d, 
                                     iter = iter, 
                                     seed = seed, 
                                     chains = chains, 
                                     cores = 4, 
                                     control = control), 
                mc.cores = 48)

saveRDS(fit, "stanfits/fit-surv.rds")

# fit the divergence models

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

div_long_center <- list()
div_long_scale <- list()

for (s in 1:4) {
  
  d <- list_data[[s]]
  
  married <- rep(NA, d$N)
  ideal_long <- rep(NA, d$N)
  ideal_short <- rep(NA, d$N)
  realized <- rep(NA, d$N)
  
  for (i in 1:d$N) {
    focal <- i
    married[i] <- length(which(d$Outcome[i, ] == 1)) # store for the model
    
    long <- length(which(d$long[i, ] == 1)) # store just for now
    short <- length(which(d$short[i, ] == 1)) # store just for now
    
    if (married[i] == 1) {
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
  
  div_long_center[[s]] <- attr(scale(diff_long), "scaled:center")
  div_long_scale[[s]] <- attr(scale(diff_long), "scaled:scale")
  
  diff_long <- scale(diff_long)[, 1]
  diff_long[is.na(diff_long)] <- -99
  
  list_data[[s]]$diff_short <- abs(realized - ideal_short) # save it for next model fit
  
  data[[s]] <- list(N = d$N, 
                    sex = d$Sex, 
                    age = d$Age,
                    alive = d$ChildrenAlive,
                    born = d$ChildrenBorn,
                    married = married,
                    diff = diff_long)
  
}

# drop missing babies from site 4

drop <- which(is.na(data[[4]]$born))

data[[4]]$N <- data[[4]]$N - length(drop)
data[[4]]$sex <- data[[4]]$sex[-c(drop)]
data[[4]]$age <- data[[4]]$age[-c(drop)]
data[[4]]$born <- data[[4]]$born[-c(drop)]
data[[4]]$alive <- data[[4]]$alive[-c(drop)]
data[[4]]$married <- data[[4]]$married[-c(drop)]
data[[4]]$diff <- data[[4]]$diff[-c(drop)]

fit <- mclapply(data, 
                function(d) sampling(m,
                                     data = d, 
                                     iter = iter, 
                                     seed = seed, 
                                     chains = chains, 
                                     cores = 4, 
                                     control = control), 
                mc.cores = 48)

saveRDS(fit, "stanfits/fit-surv-long-div.rds")

# swap out diff variable to "short" for next model

# remove the missing observation from site 4
list_data[[4]]$diff_short <- list_data[[4]]$diff_short[-c(drop)] 

div_short_center <- list()
div_short_scale <- list()

for (s in 1:4) {
  
  data[[s]]$diff <- list_data[[s]]$diff_short
  
  div_short_center[[s]] <- attr(scale(data[[s]]$diff), "scaled:center")
  div_short_scale[[s]] <- attr(scale(data[[s]]$diff), "scaled:scale")

  data[[s]]$diff <- scale(data[[s]]$diff)[, 1]
  data[[s]]$diff[is.na(data[[s]]$diff)] <- -99
  
}

fit <- mclapply(data, 
                function(d) sampling(m,
                                     data = d, 
                                     iter = iter, 
                                     seed = seed, 
                                     chains = chains, 
                                     cores = 4, 
                                     control = control), 
                mc.cores = 48)

saveRDS(fit, "stanfits/fit-surv-short-div.rds")
