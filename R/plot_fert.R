# plot fertility results

fit <- readRDS("stanfits/fit-fert.rds")

# check fit

precis(fit[[1]], 3)
precis(fit[[2]], 3)
precis(fit[[3]], 3)
precis(fit[[4]], 3)

traceplot(fit[[1]], 2, pars = "beta")
traceplot(fit[[2]], 2, pars = "beta")
traceplot(fit[[3]], 2, pars = "beta")
traceplot(fit[[4]], 2, pars = "beta")

# babies ~ negbin(mu*phi, phi)
# mu = exp(beta[1, sex]) + beta[2, sex] * log(age-10) + beta[3, sex] * babies[n]

# save parameter summaries

sum <- list()

for (i in 1:4) {
  
  sum[[i]] <- precis(fit[[i]], 3, pars = c("beta", "phi"))
  rownames(sum[[i]]) <- c("$\\pi_{1, 1}$", "$\\pi_{1, 2}$", 
                          "$\\pi_{2, 1}$", "$\\pi_{2, 2}$", 
                          "$\\pi_{3, 1}$", "$\\pi_{3, 2}$",
                          "$\\phi$")
  
  print(xtable(sum[[i]]), 
        file = paste0("output/tables/par_summary_fert_", i, ".txt"), 
        only.contents = TRUE, 
        sanitize.rownames.function = function(x) {x})
  
}

# plot results

site_names <- c("coastal", "lowland", "highland", "altiplano")
sex <- c("women", "men")

plot_data <- list()
plot_data_women <- list() # list of length b
plot_data_men <- list() # list of length b
plot_data_site <- list()

type <- c("focal 10y older", "same age", "focal 10y younger")

focal_age <- 60

# age gap is standardized
# we need to translate 10, 0, -10 into the standard units

age_gap <- c(10, 0, -10)

for (i in 1:4) {
  
  post <- extract.samples(fit[[i]])
  
  for (b in 1:3) {
    
    # take the site- specific scale & convert 10 years to std units
    gap_pred <- (age_gap[b] - age_center[[i]]) / age_scale[[i]] 
    
    mu_pred_women <- array(NA, dim = c(8e3, focal_age))
      
    for (q in 1:8e3) {
    for (a in 11:focal_age) {
          
      mu_pred_women[q, a] <- exp(post$beta[q, 1, 1] + 
                                 post$beta[q, 2, 1] * log(a - 10) +
                                 post$beta[q, 3, 1] * gap_pred)
          
      }
      }
        
      mu_pred_women <- mu_pred_women[, 11:focal_age]
      
      mu_pred_men <- array(NA, dim = c(8e3, focal_age))
      
      for (q in 1:8e3) {
        for (a in 11:focal_age) {
          
          mu_pred_men[q, a] <- exp(post$beta[q, 1, 2] + 
                                   post$beta[q, 2, 2] * log(a - 10) +
                                   post$beta[q, 3, 2] * gap_pred)
          
        }
      }
      
      mu_pred_men <- mu_pred_men[, 11:focal_age]
      
      plot_data_women[[b]] <- data.frame(age = 11:focal_age,
                                         sex = "women", 
                                         type = type[b],
                                         site = site_names[i], 
                                         mean = apply(mu_pred_women, 2, mean), 
                                         low = apply(mu_pred_women, 2, function(x) quantile(x, 0.05)), 
                                         upp = apply(mu_pred_women, 2, function(x) quantile(x, 0.95)))
      
      plot_data_men[[b]] <- data.frame(age = 11:focal_age,
                                       sex = "men", 
                                       type = type[b],
                                       site = site_names[i], 
                                       mean = apply(mu_pred_men, 2, mean), 
                                       low = apply(mu_pred_men, 2, function(x) quantile(x, 0.05)), 
                                       upp = apply(mu_pred_men, 2, function(x) quantile(x, 0.95)))
      
    }
    
    plot_data_site[[i]] <- rbind(do.call(rbind, plot_data_men), 
                                 do.call(rbind, plot_data_women))
    
}

plot_data <- do.call(rbind, plot_data_site)

# reorder sites
plot_data$site <- factor(plot_data$site)
plot_data$site <- factor(plot_data$site, levels = c("coastal", "lowland", "highland", "altiplano"))

ggplot(plot_data, 
       aes(x = age, y = mean, fill = sex)) +
  
  geom_line(aes(colour = sex)) +
  
  geom_ribbon(aes(ymin = low, ymax = upp), alpha = 0.5) +
  
  facet_grid(type ~ site)  + 
  
  labs(y = "children alive", 
       x = "focal age") +

  theme(strip.text.x = element_text(size = 14, face = "bold"), 
        strip.text.y = element_text(size = 12, face = "bold"), 
        axis.text = element_text(size = 12), 
        axis.title = element_text(size = 14, face = "bold"),
        legend.key.size = unit(0.5, "cm"), 
        legend.text = element_text(size = 12), 
        legend.position = "bottom") +

    scale_color_manual(values = c("women" = "turquoise4",
                                  "men" = "goldenrod3")) +
  
    scale_fill_manual(values = c("women" = "turquoise4",
                                 "men" = "goldenrod3"))

ggsave("output/figures/m_fert_gap.png", 
       height = 6, 
       width = 10,
       dpi = 300)

# produce numbers reported in text
# For example, a 40-year old man married to a 30-year old woman is predicted to have xx children more than a 40-year old man married to a 50-year old woman, and a 40-year old woman married to a 50-year old man is predicted to have xx children more than a 40-year old woman married to a 30-year old man.

post <- extract.samples(fit[[1]])

# 40 year old man married to 30 year old woman

focal_age <- 40
mu_pred <- array(NA, dim = c(8e3, focal_age))
age_gap <- (10 - age_center[[1]]) / age_scale[[1]]

for (q in 1:8e3) {
for (a in 11:focal_age) {
    
    mu_pred[q, a] <- exp(post$beta[q, 1, 2] + 
                         post$beta[q, 2, 2] * log(a - 10) +
                         post$beta[q, 3, 2] * (age_gap))
    
  }
}
mu_pred <- mu_pred[, 11:focal_age]
pred1 <- mu_pred[, ncol(mu_pred)]

# 40 year old man married to 50 year old woman

focal_age <- 40
mu_pred <- array(NA, dim = c(8e3, focal_age))
age_gap <- (-10 - age_center[[1]]) / age_scale[[1]]

for (q in 1:8e3) {
for (a in 11:focal_age) {
    
    mu_pred[q, a] <- exp(post$beta[q, 1, 2] + 
                         post$beta[q, 2, 2] * log(a - 10) +
                         post$beta[q, 3, 2] * (age_gap))
    
  }
}
mu_pred <- mu_pred[, 11:focal_age]
pred2 <- mu_pred[, ncol(mu_pred)]

mean(pred1 - pred2) # 1.53
quantile(pred1 - pred2, c(0.05, 0.95)) # -0.37 - 3.6

# 40 year old woman married to 50 year old man

focal_age <- 40
mu_pred <- array(NA, dim = c(8e3, focal_age))
age_gap <- (-10 - age_center[[1]]) / age_scale[[1]]

for (q in 1:8e3) {
for (a in 11:focal_age) {
    
    mu_pred[q, a] <- exp(post$beta[q, 1, 1] + 
                         post$beta[q, 2, 1] * log(a - 10) +
                         post$beta[q, 3, 1] * (age_gap))
    
}
}
mu_pred <- mu_pred[, 11:focal_age]
pred1 <- mu_pred[, ncol(mu_pred)]

# 40 year old woman married to 30 year old man

focal_age <- 40
mu_pred <- array(NA, dim = c(8e3, focal_age))
age_gap <- (10 - age_center[[1]]) / age_scale[[1]]

for (q in 1:8e3) {
for (a in 11:focal_age) {
    
    mu_pred[q, a] <- exp(post$beta[q, 1, 1] + 
                         post$beta[q, 2, 1] * log(a - 10) +
                         post$beta[q, 3, 1] * (age_gap))
    
}
}
mu_pred <- mu_pred[, 11:focal_age]
pred2 <- mu_pred[, ncol(mu_pred)]

mean(pred1 - pred2) # 2.6
quantile(pred1 - pred2, c(0.05, 0.95)) # 0.8 - 4.5

# 40 year old woman married to 40 year old man

focal_age <- 40
mu_pred <- array(NA, dim = c(8e3, focal_age))
age_gap <- (0 - age_center[[1]]) / age_scale[[1]]

for (q in 1:8e3) {
  for (a in 11:focal_age) {
    
    mu_pred[q, a] <- exp(post$beta[q, 1, 1] + 
                         post$beta[q, 2, 1] * log(a - 10) +
                         post$beta[q, 3, 1] * (age_gap))
    
  }
}
mu_pred <- mu_pred[, 11:focal_age]
pred3 <- mu_pred[, ncol(mu_pred)]

mean(pred3 - pred2) # diff 0 to 10
mean(pred3 - pred1) # diff 0 to -10
mean(pred1 - pred2) # diff -10 to 10
