# fit age gap models

# compile model

m <- stan_model("stan/m1-gap.stan")

# 1: model without predictors

# prep data

data <- list()

d_list <- list(data_PC_Long, data_WL_Long, data_WH_Long, data_IH_Long, 
               data_PC_Short, data_WL_Short, data_WH_Short, data_IH_Short, 
               data_PC_Married, data_WL_Married, data_WH_Married, data_IH_Married)

for (i in 1:length(d_list)) {
  
  d <- d_list[[i]]
  
  goods <- scale(d$GoodsValues)[, 1]
  edu <- scale(d$Edu)[, 1]
  age_FM <- scale(d$AgeAtFM)[, 1]
  
  n_age_FM_miss <- length(which(is.na(d$AgeAtFM)))
  loc_age_FM_miss <- as.array(which(is.na(d$AgeAtFM)))
  
  n_edu_miss <- length(which(is.na(edu)))
  loc_edu_miss <- as.array(which(is.na(edu)))
  
  age_FM[is.na(age_FM)] <- -99
  edu[is.na(edu)] <- -99
  
  data[[i]] <- list(N = d$N,
                    A = max(d$Age), 
                    outcome = d$Outcome, 
                    sex = d$Sex, 
                    age = d$Age, 
                    age_diff = d$AgeDiff,
                    same_sex = d$SameSex, 
                    MissingFocal = d$MissingFocal, 
                    goods = goods, 
                    edu = edu, 
                    age_FM = age_FM, 
                    n_age_FM_miss = n_age_FM_miss,
                    loc_age_FM_miss = loc_age_FM_miss,
                    n_edu_miss = n_edu_miss,
                    loc_edu_miss = loc_edu_miss,
                    Z = rep(0, 3)) # don't estimate the beta params
  
}

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

saveRDS(fit, "stanfits/fit-1.rds")

# 2: model with predictors 

# prep data

data <- list()

d_list <- list(data_PC_Long, data_WL_Long, data_WH_Long, data_IH_Long,
               data_PC_Short, data_WL_Short, data_WH_Short, data_IH_Short)

for (i in 1:length(d_list)) {
  
  d <- d_list[[i]]
  
  goods <- scale(d$GoodsValues)[, 1]
  edu <- scale(d$Edu)[, 1]
  age_FM <- scale(d$AgeAtFM)[, 1]
  
  n_age_FM_miss <- length(which(is.na(d$AgeAtFM)))
  loc_age_FM_miss <- as.array(which(is.na(d$AgeAtFM)))
  
  n_edu_miss <- length(which(is.na(edu)))
  loc_edu_miss <- as.array(which(is.na(edu)))
  
  age_FM[is.na(age_FM)] <- -99
  edu[is.na(edu)] <- -99
  
  data[[i]] <- list(N = d$N,
                    A = max(d$Age), 
                    outcome = d$Outcome, 
                    sex = d$Sex, 
                    age = d$Age, 
                    age_diff = d$AgeDiff,
                    same_sex = d$SameSex, 
                    MissingFocal = d$MissingFocal, 
                    goods = goods, 
                    edu = edu, 
                    age_FM = age_FM,
                    n_age_FM_miss = n_age_FM_miss,
                    loc_age_FM_miss = loc_age_FM_miss,
                    n_edu_miss = n_edu_miss,
                    loc_edu_miss = loc_edu_miss,
                    Z = rep(1, 3)) # estimate the beta params
  
}

# fit model

fit <- mclapply(data, 
                function(d) sampling(m, 
                                     data = d, 
                                     iter = iter, 
                                     seed = seed, 
                                     chains = chains,
                                     control = control,
                                     cores = 4), 
                mc.cores = 32)

saveRDS(fit, "stanfits/fit-1-gap-multi.rds")
