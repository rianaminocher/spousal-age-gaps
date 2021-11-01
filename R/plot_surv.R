# plot offspring survival results

# 1: realized age difference

# check fit

fit <- readRDS("stanfits/fit-surv.rds")

precis(fit[[1]], 3)
precis(fit[[2]], 3)
precis(fit[[3]], 3)
precis(fit[[4]], 3)

traceplot(fit[[1]], 3, pars = "beta")
traceplot(fit[[2]], 3, pars = "beta")
traceplot(fit[[3]], 3, pars = "beta")
traceplot(fit[[4]], 3, pars = "beta")

# save parameter summaries

sum <- list()

for (i in 1:4) {
  
  sum[[i]] <- precis(fit[[i]], 3, pars = c("beta"))
  rownames(sum[[i]]) <- c("$\\chi_{1, 1}$", "$\\chi_{1, 2}$", 
                          "$\\chi_{2, 1}$", "$\\chi_{2, 2}$", 
                          "$\\chi_{3, 1}$", "$\\chi_{3, 2}$")
  
  print(xtable(sum[[i]]), 
        file = paste0("output/tables/par_summary_surv_diff", i, ".txt"), 
        only.contents = TRUE, 
        sanitize.rownames.function = function(x) {x})
  
}

# store data for ggplot

site_names <- c("coastal", "lowland", "highland", "altiplano")
sex <- c("women", "men")

site_est <- list()
est <- list()

for (i in 1:4) { # sites
for (s in 1:2) { # sexes
      
      post <- extract.samples(fit[[i]])
      est[[s]] <- data.frame(sex = sex[s], 
                             site = site_names[i], 
                             est = quantile(post$beta[ , 3, s], 0.5), 
                             ymin = quantile(post$beta[ , 3, s], 0.05),
                             ymax = quantile(post$beta[ , 3, s], 0.95), 
                             term = "age \n difference")
      
  }
  
  site_est[[i]] <- do.call(rbind, est)
    
}

plot_data <- do.call(rbind, site_est)

# reorder sites
plot_data$site <- factor(plot_data$site)
plot_data$site <- factor(plot_data$site, levels = c("coastal", "lowland", "highland", "altiplano"))

plot_real <- plot_data
plot_real$model <- "realized gap"

# 2: divergence, long

fit <- readRDS("stanfits/fit-surv-long-div.rds")

# check fit

precis(fit[[1]], 3)
precis(fit[[2]], 3)
precis(fit[[3]], 3)
precis(fit[[4]], 3)

traceplot(fit[[1]], 3, pars = "beta")
traceplot(fit[[2]], 3, pars = "beta")
traceplot(fit[[3]], 3, pars = "beta")
traceplot(fit[[4]], 3, pars = "beta")

# save parameter summaries

sum <- list()

for (i in 1:4) {
  
  sum[[i]] <- precis(fit[[i]], 3, pars = c("beta"))
  rownames(sum[[i]]) <- c("$\\chi_{1, 1}$", "$\\chi_{1, 2}$", 
                          "$\\chi_{2, 1}$", "$\\chi_{2, 2}$", 
                          "$\\chi_{3, 1}$", "$\\chi_{3, 2}$")
  
  print(xtable(sum[[i]]), 
        file = paste0("output/tables/par_summary_surv_div_long", i, ".txt"), 
        only.contents = TRUE, 
        sanitize.rownames.function = function(x) {x})
  
}

# store data for ggplot

site_est <- list()
est <- list()

for (i in 1:4) { # sites
for (s in 1:2) { # sexes
    
    post <- extract.samples(fit[[i]])
    est[[s]] <- data.frame(sex = sex[s], 
                           site = site_names[i], 
                           est = mean(post$beta[ , 3, s]), 
                           ymin = quantile(post$beta[ , 3, s], 0.05),
                           ymax = quantile(post$beta[ , 3, s], 0.95), 
                           term = "divergence \n actual-ideal")
    
  }
  
  site_est[[i]] <- do.call(rbind, est)
  
}

plot_data <- do.call(rbind, site_est)

# reorder sites
plot_data$site <- factor(plot_data$site)
plot_data$site <- factor(plot_data$site, levels = c("coastal", "lowland", "highland", "altiplano"))

plot_long <- plot_data
plot_long$model <- "divergence, long"

# 3: divergence, short

fit <- readRDS("stanfits/fit-surv-short-div.rds")

# check fit 

precis(fit[[1]], 3)
precis(fit[[2]], 3)
precis(fit[[3]], 3)
precis(fit[[4]], 3)

traceplot(fit[[1]], 3, pars = "beta")
traceplot(fit[[2]], 3, pars = "beta")
traceplot(fit[[3]], 3, pars = "beta")
traceplot(fit[[4]], 3, pars = "beta")

# save parameter summaries

sum <- list()

for (i in 1:4) {
  
  sum[[i]] <- precis(fit[[i]], 3, pars = c("beta"))
  rownames(sum[[i]]) <- c("$\\chi_{1, 1}$", "$\\chi_{1, 2}$", 
                          "$\\chi_{2, 1}$", "$\\chi_{2, 2}$", 
                          "$\\chi_{3, 1}$", "$\\chi_{3, 2}$")
  
  print(xtable(sum[[i]]), 
        file = paste0("output/tables/par_summary_surv_div_short", i, ".txt"), 
        only.contents = TRUE, 
        sanitize.rownames.function = function(x) {x})
  
}

# store data for ggplot

site_est <- list()
est <- list()

for (i in 1:4) { # sites
for (s in 1:2) { # sexes
    
    post <- extract.samples(fit[[i]])
    est[[s]] <- data.frame(sex = sex[s], 
                           site = site_names[i], 
                           est = mean(post$beta[ , 3, s]), 
                           ymin = quantile(post$beta[ , 3, s], 0.05),
                           ymax = quantile(post$beta[ , 3, s], 0.95), 
                           term = "divergence \n actual-ideal")
    
  }
  
  site_est[[i]] <- do.call(rbind, est)
  
}

plot_data <- do.call(rbind, site_est)

# reorder sites
plot_data$site <- factor(plot_data$site)
plot_data$site <- factor(plot_data$site, levels = c("coastal", "lowland", "highland", "altiplano"))

plot_short <- plot_data
plot_short$model <- "divergence, short"

# combine data for ggplot

plot_all <- rbind(plot_real, plot_long, plot_short)

# plot results

ggplot(plot_all, 
       aes(x = model, 
           y = est, 
           color = sex)) +
  
  coord_flip() +
  
  facet_grid(. ~ site) +
  
  geom_hline(yintercept = 0, linetype = "dashed") +
  
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

ggsave(file = "output/figures/m_surv_coef.png",
       dpi = 300, 
       height = 3.5, 
       width = 12)

# produce numbers reported in text

# Here, women married to men 10 years older than themselves benefit from an offspring survival rate that is about 19\% () larger than women married to men the same age as themselves.

fit <- readRDS("stanfits/fit-surv.rds")
post <- extract.samples(fit[[1]])

# we need the age gap in standard units to make predictions
# can use same scale/center from fert models, because the outcome is similar 

focal_age <- 40
mu_pred <- array(NA, dim = c(8e3, focal_age))
age_gap <- (-10 - age_center[[1]]) / age_scale[[1]]

for (q in 1:8e3) {
for (a in 11:focal_age) {
    
    mu_pred[q, a] <- inv_logit(post$beta[q, 1, 1] + 
                               post$beta[q, 2, 1] * log(a - 10) +
                               post$beta[q, 3, 1] * (age_gap))
    
}
}
mu_pred <- mu_pred[, 11:focal_age]
pred1 <- mu_pred[, ncol(mu_pred)]

focal_age <- 40
mu_pred <- array(NA, dim = c(8e3, focal_age))
age_gap <- (0 - age_center[[1]]) / age_scale[[1]]

for (q in 1:8e3) {
  for (a in 11:focal_age) {
    
    mu_pred[q, a] <- inv_logit(post$beta[q, 1, 1] + 
                               post$beta[q, 2, 1] * log(a - 10) +
                               post$beta[q, 3, 1] * (age_gap))
    
  }
}
mu_pred <- mu_pred[, 11:focal_age]
pred2 <- mu_pred[, ncol(mu_pred)]

mean(pred1 - pred2) # 0.06
quantile(pred1 - pred2, c(0.05, 0.95)) # 0.04 - 0.08

# Considering the divergence between preferred age-gaps and realized spousal gaps, we observe that women suffer decreased offspring survival when the divergence between their preferences and realized gaps increases; an individual that nominates a long-term partner that differs in age from their current partner by 5 years suffers a decreased offspring survival rate of xx\% compared with an individual who nominates a partner that is the same age as their current partner.

fit <- readRDS("stanfits/fit-surv-long-div.rds")
post <- extract.samples(fit[[1]])

focal_age <- 40
mu_pred <- array(NA, dim = c(8e3, focal_age))
age_div <- (5 - div_short_center[[1]]) / div_short_scale[[1]]

for (q in 1:8e3) {
  for (a in 11:focal_age) {
    
    mu_pred[q, a] <- inv_logit(post$beta[q, 1, 1] + 
                               post$beta[q, 2, 1] * log(a - 10) +
                               post$beta[q, 3, 1] * (age_div))
    
  }
}
mu_pred <- mu_pred[, 11:focal_age]
pred1 <- mu_pred[, ncol(mu_pred)]

focal_age <- 40
mu_pred <- array(NA, dim = c(8e3, focal_age))
age_div <- (0 - div_short_center[[1]]) / div_short_scale[[1]]

for (q in 1:8e3) {
  for (a in 11:focal_age) {
    
    mu_pred[q, a] <- inv_logit(post$beta[q, 1, 1] + 
                               post$beta[q, 2, 1] * log(a - 10) +
                               post$beta[q, 3, 1] * (age_div))
    
  }
}
mu_pred <- mu_pred[, 11:focal_age]
pred2 <- mu_pred[, ncol(mu_pred)]

mean(pred2 - pred1) # 0.02
quantile(pred2 - pred1, c(0.05, 0.95)) # -0.01 - 0.04

# (equivalently, for the divergence between realized gap and preferred short-term gap, offspring survival is decreased by xx\%).

fit <- readRDS("stanfits/fit-surv-short-div.rds")
post <- extract.samples(fit[[1]])

focal_age <- 40
mu_pred <- array(NA, dim = c(8e3, focal_age))
age_div <- (5 - div_short_center[[1]]) / div_short_scale[[1]]

for (q in 1:8e3) {
  for (a in 11:focal_age) {
    
    mu_pred[q, a] <- inv_logit(post$beta[q, 1, 1] + 
                                 post$beta[q, 2, 1] * log(a - 10) +
                                 post$beta[q, 3, 1] * (age_div))
    
  }
}
mu_pred <- mu_pred[, 11:focal_age]
pred1 <- mu_pred[, ncol(mu_pred)]

focal_age <- 40
mu_pred <- array(NA, dim = c(8e3, focal_age))
age_div <- (0 - div_short_center[[1]]) / div_short_scale[[1]]

for (q in 1:8e3) {
  for (a in 11:focal_age) {
    
    mu_pred[q, a] <- inv_logit(post$beta[q, 1, 1] + 
                                 post$beta[q, 2, 1] * log(a - 10) +
                                 post$beta[q, 3, 1] * (age_div))
    
  }
}
mu_pred <- mu_pred[, 11:focal_age]
pred2 <- mu_pred[, ncol(mu_pred)]

mean(pred2 - pred1) # 0.04
quantile(pred2 - pred1, c(0.05, 0.95)) # 0.008 - 0.06
