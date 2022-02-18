# plot gap results

# 1: model without predictors

fit <- readRDS("stanfits/fit-1.rds")

# 1:4 is "long" by site
# 5:8 is "short" by site
# 9:12 is "married" by site

# check fit

precis(fit[[1]], 3)
precis(fit[[2]], 3)
precis(fit[[3]], 3)
precis(fit[[4]], 3)

traceplot(fit[[1]], 3, pars = "eta")
traceplot(fit[[2]], 3, pars = "eta")
traceplot(fit[[3]], 3, pars = "eta")
traceplot(fit[[4]], 3, pars = "eta")
traceplot(fit[[1]], 3, pars = "tau")
traceplot(fit[[2]], 3, pars = "tau")
traceplot(fit[[3]], 3, pars = "tau")
traceplot(fit[[4]], 3, pars = "tau")
traceplot(fit[[1]], 3, pars = "kappa")
traceplot(fit[[2]], 3, pars = "kappa")
traceplot(fit[[3]], 3, pars = "kappa")
traceplot(fit[[4]], 3, pars = "kappa")
traceplot(fit[[1]], 3, pars = "delta")
traceplot(fit[[2]], 3, pars = "delta")
traceplot(fit[[3]], 3, pars = "delta")
traceplot(fit[[4]], 3, pars = "delta")

precis(fit[[5]], 3)
precis(fit[[6]], 3)
precis(fit[[7]], 3)
precis(fit[[8]], 3)

traceplot(fit[[5]], 3, pars = "eta")
traceplot(fit[[6]], 3, pars = "eta")
traceplot(fit[[7]], 3, pars = "eta")
traceplot(fit[[8]], 3, pars = "eta")
traceplot(fit[[5]], 3, pars = "tau")
traceplot(fit[[6]], 3, pars = "tau")
traceplot(fit[[7]], 3, pars = "tau")
traceplot(fit[[8]], 3, pars = "tau")
traceplot(fit[[5]], 3, pars = "kappa")
traceplot(fit[[6]], 3, pars = "kappa")
traceplot(fit[[7]], 3, pars = "kappa")
traceplot(fit[[8]], 3, pars = "kappa")
traceplot(fit[[5]], 3, pars = "delta")
traceplot(fit[[6]], 3, pars = "delta")
traceplot(fit[[7]], 3, pars = "delta")
traceplot(fit[[8]], 3, pars = "delta")

precis(fit[[9]], 3)
precis(fit[[10]], 3)
precis(fit[[11]], 3)
precis(fit[[12]], 3)

traceplot(fit[[9]], 3, pars = "eta")
traceplot(fit[[10]], 3, pars = "eta")
traceplot(fit[[11]], 3, pars = "eta")
traceplot(fit[[12]], 3, pars = "eta")
traceplot(fit[[9]], 3, pars = "tau")
traceplot(fit[[10]], 3, pars = "tau")
traceplot(fit[[11]], 3, pars = "tau")
traceplot(fit[[12]], 3, pars = "tau")
traceplot(fit[[9]], 3, pars = "kappa")
traceplot(fit[[10]], 3, pars = "kappa")
traceplot(fit[[11]], 3, pars = "kappa")
traceplot(fit[[12]], 3, pars = "kappa")
traceplot(fit[[9]], 3, pars = "delta")
traceplot(fit[[10]], 3, pars = "delta")
traceplot(fit[[11]], 3, pars = "delta")
traceplot(fit[[12]], 3, pars = "delta")

# save parameter summaries

# save means across ages

sum <- list()
sum_sex <- list()

for (i in 1:12) {
  
  post <- extract.samples(fit[[i]])
  
  for (s in 1:2) {
    tmp <- quantile(post$theta[, s, ], c(0.5, 0.05, 0.95))
    tmp <- round(tmp, 1)
    tmp <- paste0(tmp, collapse = c(""), sep = c(" (", ", ", ")")) 
    sum_sex[[s]] <- tmp
  }
  
  sum[[i]] <- do.call(rbind, sum_sex)
}

sum_long <- do.call(rbind, sum[1:4])
sum_short <- do.call(rbind, sum[5:8])
sum_marr <- do.call(rbind, sum[9:12])

sum <- cbind(sum_long, sum_short, sum_marr)

rownames(sum) <- c("Coastal (F)", "Coastal (M)", 
                   "Lowland (F)", "Lowland (M)",
                   "Highland (F)", "Highland (M)",
                   "Altiplano (F)", "Altiplano (M)")

colnames(sum) <- c("long-term (years)", "short-term (years)", "married (years)")

print(xtable(sum), 
      file = "output/tables/m_gap_means.txt",
      only.contents = TRUE, 
      sanitize.rownames.function = function(x) x)

# save means at age 20

sum <- list()
sum_sex <- list()

for (i in 1:12) {
  
  post <- extract.samples(fit[[i]])
  
  for (s in 1:2) {
    tmp <- quantile(post$theta[, s, 20], c(0.5, 0.05, 0.95)) # select only age 20!
    tmp <- round(tmp, 2)
    tmp <- paste0(tmp, collapse = c(""), sep = c(" (", ", ", ")")) 
    sum_sex[[s]] <- tmp
  }
  
  sum[[i]] <- do.call(rbind, sum_sex)
  
}

sum_long <- do.call(rbind, sum[1:4])
sum_short <- do.call(rbind, sum[5:8])
sum_marr <- do.call(rbind, sum[9:12])

sum <- cbind(sum_long, sum_short, sum_marr)

rownames(sum) <- c("PC (F)", "PC (M)", 
                   "WL (F)", "WL (M)",
                   "WH (F)", "WH (M)",
                   "IH (F)", "IH (M)")

colnames(sum) <- c("long", "short", "married")

print(xtable(sum), 
      file = "output/tables/m_gap_means_20.txt",
      only.contents = TRUE, 
      sanitize.rownames.function = function(x) x)

# save means at age 60

sum <- list()
sum_sex <- list()

for (i in 1:12) {
  
  post <- extract.samples(fit[[i]])
  
  for (s in 1:2) {
    tmp <- quantile(post$theta[, s, 60], c(0.5, 0.05, 0.95)) # select age 60!
    tmp <- round(tmp, 2)
    tmp <- paste0(tmp, collapse = c(""), sep = c(" (", ", ", ")")) 
    sum_sex[[s]] <- tmp
  }
  
  sum[[i]] <- do.call(rbind, sum_sex)
  
}

sum_long <- do.call(rbind, sum[1:4])
sum_short <- do.call(rbind, sum[5:8])
sum_marr <- do.call(rbind, sum[9:12])

sum <- cbind(sum_long, sum_short, sum_marr)

rownames(sum) <- c("PC (F)", "PC (M)", 
                   "WL (F)", "WL (M)",
                   "WH (F)", "WH (M)",
                   "IH (F)", "IH (M)")

colnames(sum) <- c("long", "short", "married")

print(xtable(sum), 
      file = "output/tables/m_gap_means_60.txt",
      only.contents = TRUE, 
      sanitize.rownames.function = function(x) x)

# save full parameter summaries

sum <- list()
model <- c("long1", "long2", "long3", "long4", 
           "short1", "short2", "short3", "short4",
           "marr1", "marr2", "marr3", "marr4")

for (i in 1:12) {
  
  sum[[i]] <- precis(fit[[i]], 3, pars = c("eta", "kappa", "tau", "delta", "sigma"))
  
  rownames(sum[[i]]) <- c("$\\eta_{1}$", "$\\eta_{2}$", 
                          "$\\kappa_{1}$", "$\\kappa_{2}$", 
                          "$\\tau_{1}$", "$\\tau_{2}$",
                          "$\\delta_{1}$", "$\\delta_{2}$",
                          "$\\sigma$")
  
    print(xtable(sum[[i]]), 
          file = paste0("output/tables/par_summary_gap_", 
                        model[i],
                        ".txt"), 
          only.contents = TRUE, 
          sanitize.rownames.function = function(x) {x}) 
    
}

# plot results

site_names <- rep(c("coastal", "lowland", "highland", "altiplano"), 3)
sex <- c("women", "men")

d <- list()
d_sex <- list()

for (i in 1:12) {
  
  post <- extract.samples(fit[[i]])
  
for (s in 1:2) {
  
  d_sex[[s]] <- data.frame(age = (1:ncol(post$theta[, s, ])), 
                           group = sex[s], 
                           type = NA, 
                           site = site_names[i], 
                           low = apply(post$theta[, s, ], 2, function(x) quantile(x, 0.05)), 
                           mid = apply(post$theta[, s, ], 2, mean), 
                           upp = apply(post$theta[, s, ], 2, function(x) quantile(x, 0.95)))  
}
  
  d[[i]] <- do.call(rbind, d_sex)
  
}

d_long <- do.call(rbind, d[1:4])
d_short <- do.call(rbind, d[5:8])
d_marr <- do.call(rbind, d[9:12])

d_long$type <- "long-term"
d_short$type <- "short-term"
d_marr$type <- "married"
  
plot_data <- rbind(d_long, d_short, d_marr)

# reorder sites
plot_data$site <- factor(plot_data$site)
plot_data$site <- factor(plot_data$site, levels = c("coastal", "lowland", "highland", "altiplano"))

ggplot(plot_data, 
       aes(x = age, 
           y = mid, 
           fill = type)) +
  
  geom_line(aes(color = type)) + 
  
  geom_ribbon(aes(ymin = low, 
                  ymax = upp), 
              alpha = 0.5) + 
  
  facet_grid(group ~ site) +

  geom_hline(aes(yintercept = 0), 
             color = "black", 
             linetype = "dashed") +
    
  labs(y = "age difference", 
         x = "focal age") +
  
  theme(strip.text.x = element_text(size = 14, face = "bold"),
        strip.text.y = element_text(size = 14, face = "bold"), 
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, face = "bold"), 
        legend.key.size = unit(1, "cm"), 
        legend.text = element_text(size = 12), 
        legend.title = element_blank(), 
        legend.position = "bottom") + 
  
  coord_cartesian(xlim = c(18, 50), ylim = c(-20, 45)) +
  
  scale_color_manual(values = c("married" = "turquoise4",
                                "short-term" ="#b54a31",
                                "long-term" = "goldenrod3")) +
  
  scale_fill_manual(values = c("married" = "turquoise4",
                               "short-term" = "#b54a31",
                               "long-term" = "goldenrod3"))

ggsave(file = "output/figures/m_gap.png", 
       dpi = 300, 
       height = 8, 
       width = 14)

# 2: model with predictors

# 1:4 is long
# 5:8 is short

fit <- readRDS("stanfits/fit-1-gap-multi.rds")

# check fit

precis(fit[[1]], 3)
precis(fit[[2]], 3)
precis(fit[[3]], 3)
precis(fit[[4]], 3)

traceplot(fit[[1]], 3, pars = "eta")
traceplot(fit[[2]], 3, pars = "eta")
traceplot(fit[[3]], 3, pars = "eta")
traceplot(fit[[4]], 3, pars = "eta")
traceplot(fit[[1]], 3, pars = "tau")
traceplot(fit[[2]], 3, pars = "tau")
traceplot(fit[[3]], 3, pars = "tau")
traceplot(fit[[4]], 3, pars = "tau")
traceplot(fit[[1]], 3, pars = "kappa")
traceplot(fit[[2]], 3, pars = "kappa")
traceplot(fit[[3]], 3, pars = "kappa")
traceplot(fit[[4]], 3, pars = "kappa")
traceplot(fit[[1]], 3, pars = "delta")
traceplot(fit[[2]], 3, pars = "delta")
traceplot(fit[[3]], 3, pars = "delta")
traceplot(fit[[4]], 3, pars = "delta")
traceplot(fit[[1]], 3, pars = "beta")
traceplot(fit[[2]], 3, pars = "beta")
traceplot(fit[[3]], 3, pars = "beta")
traceplot(fit[[4]], 3, pars = "beta")

precis(fit[[5]], 3)
precis(fit[[6]], 3)
precis(fit[[7]], 3)
precis(fit[[8]], 3)

traceplot(fit[[5]], 3, pars = "eta")
traceplot(fit[[6]], 3, pars = "eta")
traceplot(fit[[7]], 3, pars = "eta")
traceplot(fit[[8]], 3, pars = "eta")
traceplot(fit[[5]], 3, pars = "tau")
traceplot(fit[[6]], 3, pars = "tau")
traceplot(fit[[7]], 3, pars = "tau")
traceplot(fit[[8]], 3, pars = "tau")
traceplot(fit[[5]], 3, pars = "kappa")
traceplot(fit[[6]], 3, pars = "kappa")
traceplot(fit[[7]], 3, pars = "kappa")
traceplot(fit[[8]], 3, pars = "kappa")
traceplot(fit[[5]], 3, pars = "delta")
traceplot(fit[[6]], 3, pars = "delta")
traceplot(fit[[7]], 3, pars = "delta")
traceplot(fit[[8]], 3, pars = "delta")
traceplot(fit[[5]], 3, pars = "beta")
traceplot(fit[[6]], 3, pars = "beta")
traceplot(fit[[7]], 3, pars = "beta")
traceplot(fit[[8]], 3, pars = "beta")

# save full parameter summaries

sum <- list()
model <- c("long1", "long2", "long3", "long4", 
           "short1", "short2", "short3", "short4")

for (i in 1:8) {
  
  sum[[i]] <- precis(fit[[i]], 3, pars = c("eta", "kappa", "tau", "delta", "beta", "sigma"))
  
  rownames(sum[[i]]) <- c("$\\eta_{1}$", "$\\eta_{2}$", 
                          "$\\kappa_{1}$", "$\\kappa_{2}$", 
                          "$\\tau_{1}$", "$\\tau_{2}$",
                          "$\\delta_{1}$", "$\\delta_{2}$",
                          "$\\beta_{1, 1}$", "$\\beta_{1, 2}$",
                          "$\\beta_{1, 3}$", "$\\beta_{2, 1}$",
                          "$\\beta_{2, 2}$", "$\\beta_{2, 3}$",
                          "$\\sigma$")
  
  print(xtable(sum[[i]]), 
        file = paste0("output/tables/par_summary_gap_multi_", 
                      model[i],
                      ".txt"), 
        only.contents = TRUE, 
        sanitize.rownames.function = function(x) {x}) 
  
}

# plot results

site_names <- rep(c("coastal", "lowland", "highland", "altiplano"), 2)
term <- c("age at \n first marriage", "education", "wealth")

est <- list()
est_site <- list()
est_sex <- list()

for (i in 1:8) { # model
  
  post <- extract.samples(fit[[i]]) 
  
  for (s in 1:2) { # sex
  for (b in 1:3) { # est
      
    est[[b]] <- data.frame(sex = sex[s], 
                           type = NA, 
                           site = site_names[i], 
                           term = term[b],
                           est = quantile(post$beta[ , s, b], 0.5), 
                           ymin = quantile(post$beta[ , s, b], 0.05), 
                           ymax = quantile(post$beta[ , s, b], 0.95)) 
      
    }
    est_sex[[s]] <- do.call(rbind, est) 
    
    }
    est_site[[i]] <- do.call(rbind, est_sex)

}

long <- do.call(rbind, est_site[1:4])
short <- do.call(rbind, est_site[5:8])

long$type <- "long-term"
short$type <- "short-term"

plot_data <- rbind(long, short)

# reorder sites
plot_data$site <- factor(plot_data$site)
plot_data$site <- factor(plot_data$site, levels = c("coastal", "lowland", "highland", "altiplano"))

# plot

ggplot(plot_data, 
       aes(x = term, 
           y = est, 
           color = sex)) +
  
  coord_flip() +
  
  facet_grid(type ~ site) +
  
  geom_hline(aes(yintercept = 0), linetype = "dashed") +
  
  geom_linerange(aes(ymin = ymin, ymax = ymax), 
                 position = position_dodge(width = 0.35), size=1.25) +
  
  geom_point(position = position_dodge(width = 0.35),size=2) +

  scale_color_manual(values = c("women" = "turquoise4",
                                "men" = "goldenrod3"))+
  
  theme(strip.text.x = element_text(size = 14, face = "bold"),
        strip.text.y = element_text(size = 14, face = "bold"), 
        axis.text = element_text(size = 12),
        axis.title = element_blank(), 
        legend.key.size = unit(1, "cm"), 
        legend.text = element_text(size = 12), 
        legend.title = element_blank(), 
        legend.position = "bottom")

ggsave(file = "output/figures/m_gap_coef.png", 
       dpi = 300, 
       height = 6, 
       width = 12)

# plot age distribution by community

age <- c(data_PC_Long$Age, 
         data_WL_Long$Age, 
         data_WH_Long$Age,
         data_IH_Long$Age)

sex <- c(data_PC_Long$Sex, 
         data_WL_Long$Sex, 
         data_WH_Long$Sex,
         data_IH_Long$Sex)

site <- c(rep("coastal", data_PC_Long$N), 
          rep("lowland", data_WL_Long$N), 
          rep("highland", data_WH_Long$N),
          rep("altiplano", data_IH_Long$N))

dens_dat <- data.frame(age = age,
                       sex = sex, 
                       site = site)

dens_dat$sex <-ifelse(dens_dat$sex == 1, 
                      "women",
                      "men")

dens_dat$site <- factor(dens_dat$site)
dens_dat$site <- factor(dens_dat$site, levels = c("coastal", "lowland", "highland", "altiplano"))

ggplot(dens_dat, 
       aes(x = age,
           col = sex,
           fill = sex)) +
  
  geom_histogram(alpha = 0.5) +
  
  facet_grid(sex ~ site) +
  
  scale_color_manual(values = c("women" = "turquoise4",
                                "men" = "goldenrod3")) +  
  
  scale_fill_manual(values = c("women" = "turquoise4",
                                "men" = "goldenrod3")) + 
  
  theme(legend.position = "none",
        strip.text.x = element_text(size = 14,
                                    face = "bold"),
        strip.text.y = element_text(size = 14, 
                                    face = "bold"), 
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, 
                                  face = "bold")) 

ggsave(file = "output/figures/comm_age_sex.png", 
       dpi = 300, 
       height = 6, 
       width = 12)

# plot the empirical distribution of deviation between individual's average age of nominee and actual gap
# goal was to restrict to individuals under the age of 30, i.e. when entering/entered marriage market
# but there are very few data points for some sites, e.g. 3 men long-term nom under 30 at altiplano, or one man highland long-term

site_names <- c("coastal", "lowland", "highland", "altiplano")
sex <- c("women", "men")

list_data <- list(list(data_PC_Curr_Married, data_PC_Long, data_PC_Short), 
                  list(data_WL_Curr_Married, data_WL_Long, data_WL_Short),
                  list(data_WH_Curr_Married, data_WH_Long, data_WH_Short),
                  list(data_IH_Curr_Married, data_IH_Long, data_IH_Short))

# calculate the age-gap means

plot_data <- list()

for (j in 1:4) { # sites
  
  data <- list_data[[j]][[1]]
  spouse_diff <- rep(NA, data$N)
  
  for (i in 1:data$N) {
    
    if(any(data$Outcome[i, ] == 1)) {
      spouse_id <- which(data$Outcome[i, ] == 1)
      spouse_diff[i] <- data$AgeDiff[i, spouse_id]
    }
    
  }
  
  data <- list_data[[j]][[2]]
  long_diff <- rep(NA, data$N)
  
  for (i in 1:data$N) {
    
    if(any(data$Outcome[i, ] == 1)) {
      nom_id <- which(data$Outcome[i, ] == 1)
      long_diff[i] <- mean(data$AgeDiff[i, nom_id])
    }
    
  }
  
  data <- list_data[[j]][[3]]
  short_diff <- rep(NA, data$N)
  
  for (i in 1:data$N) {
    
    if(any(data$Outcome[i, ] == 1)) {
      nom_id <- which(data$Outcome[i, ] == 1)
      short_diff[i] <- mean(data$AgeDiff[i, nom_id])
    }
    
  }
  
  long <- list()
  short <- list()
  
  for (s in 1:2) {
    
    x <- spouse_diff[data$Sex == s] - long_diff[data$Sex == s]
    
    long[[s]] <- data.frame(diff = x[!is.na(x)], 
                            sex = sex[s], 
                            type = "long-term", 
                            site = site_names[j])
    
    x <- spouse_diff[data$Sex == s] - short_diff[data$Sex == s]
    
    short[[s]] <- data.frame(diff = x[!is.na(x)], 
                             sex = sex[s], 
                             type = "short-term", 
                             site = site_names[j])
    
  }
  
  long <- do.call(rbind, long)
  short <- do.call(rbind, short)
  plot_data[[j]] <- rbind(long, short)
  
}

plot_data <- do.call(rbind, plot_data)

plot_data$site <- factor(plot_data$site)
plot_data$site <- factor(plot_data$site, levels = c("coastal", "lowland", "highland", "altiplano"))

ggplot(plot_data, 
       aes(x = diff, 
           fill = sex,
           color = sex)) +
  
  geom_density(alpha = 0.5) + 
  
  facet_grid(site ~ type,
             scales = "free_y") +
  
  geom_vline(aes(xintercept = 0), 
             color = "black", 
             linetype = "dashed") +
  
  labs(y = "", 
       x = "difference between spousal \n and preferred age gap") +
  
  theme(strip.text.x = element_text(size = 14,
                                    face = "bold"),
        strip.text.y = element_text(size = 14,
                                    face = "bold"), 
        axis.text = element_text(size = 12),
        axis.title = element_text(size = 14, 
                                  face = "bold"), 
        legend.key.size = unit(0.5, "cm"), 
        legend.text = element_text(size = 12), 
        legend.title = element_blank(),
        legend.position = "bottom") +

  scale_color_manual(values = c("women" = "turquoise4",
                                "men" = "goldenrod3")) +  
  scale_fill_manual(values = c("women" = "turquoise4",
                                "men" = "goldenrod3")) +

  coord_cartesian(xlim = c(-40, 20)) 

ggsave(file = "output/figures/gap_empirical_diff.png", 
       dpi = 300, 
       height = 10, 
       width = 10)
