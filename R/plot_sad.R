# plot sad model results

# 1: realized age difference

fit <- readRDS("stanfits/fit-sad.rds")

# check fit

pars <- c("c", "beta", "alpha", "gamma", "tau", "kappa", "delta")

precis(fit[[1]], 3, pars = pars)
precis(fit[[2]], 3, pars = pars)
precis(fit[[3]], 3, pars = pars)
precis(fit[[4]], 3, pars = pars)

traceplot(fit[[1]], 3, pars = "beta")
traceplot(fit[[2]], 3, pars = "beta")
traceplot(fit[[3]], 3, pars = "beta")
traceplot(fit[[4]], 3, pars = "beta")

traceplot(fit[[1]], 3, pars = "alpha")
traceplot(fit[[2]], 3, pars = "alpha")
traceplot(fit[[3]], 3, pars = "alpha")
traceplot(fit[[4]], 3, pars = "alpha")

traceplot(fit[[1]], 3, pars = "gamma")
traceplot(fit[[2]], 3, pars = "gamma")
traceplot(fit[[3]], 3, pars = "gamma")
traceplot(fit[[4]], 3, pars = "gamma")

traceplot(fit[[1]], 3, pars = "c")
traceplot(fit[[2]], 3, pars = "c")
traceplot(fit[[3]], 3, pars = "c")
traceplot(fit[[4]], 3, pars = "c")

# save an example for the supplement

png("output/figures/trace_gamma.png", 
    height = 2000, 
    width = 3000, 
    res = 250)

traceplot(fit[[1]], 3, pars = "gamma")

dev.off()

# save parameter summaries

sum <- list()

for (i in 1:4) {
  
  sum[[i]] <- precis(fit[[i]], 3, pars = c("beta", "gamma", "alpha", "eta", "kappa", "tau", "delta", "sigma"))
  rownames(sum[[i]]) <- c("$\\phi_{1, 1}$", "$\\phi_{1, 2}$", "$\\phi_{1, 3}$", 
                          "$\\phi_{2, 1}$", "$\\phi_{2, 2}$", "$\\phi_{2, 3}$", 
                          "$\\gamma_{1}$", "$\\gamma_{2}$", "$\\gamma_{3}$", 
                          "$\\gamma_{4}$", "$\\gamma_{5}$", "$\\gamma_{6}$", 
                          "$\\alpha_{1}$", "$\\alpha_{2}$", "$\\alpha_{3}$", 
                          "$\\alpha_{4}$", "$\\alpha_{5}$", "$\\alpha_{6}$", 
                          "$\\eta_{1}$", "$\\eta_{2}$", 
                          "$\\kappa_{1}$", "$\\kappa_{2}$", 
                          "$\\tau_{1}$", "$\\tau_{2}$", 
                          "$\\delta_{1}$", "$\\delta_{2}$",
                          "$\\sigma$")
  
  print(xtable(sum[[i]]), 
        file = paste0("output/tables/par_summary_sad_diff", i, ".txt"), 
        only.contents = TRUE, 
        sanitize.rownames.function = function(x) {x})
  
}

# gamma caterpillar plot

labels <- c("sad", "anxious", "nervous", "hopeless", "worthless", "tired")
site_names <- c("coastal", "lowland", "highland", "altiplano")

png("output/figures/irt_gamma_caterpillar_diff.png", 
    res = 250,
    height = 1000, 
    width = 4000)

par(mfrow = c(1, 4), mar = c(5, 5.5, 4, 2)) 

for (i in 1:4) {
  
  post <- extract.samples(fit[[i]])
  
  plot(NULL, 
       xlim = c(-4, 4), 
       ylim = c(1, 7), 
       xlab = "", 
       ylab = "", 
       yaxt = "n", 
       cex.axis = 1.5)
  
  mtext(site_names[i])
  
  points(apply(post$gamma[, 2:6], 2, mean), 2:6, lwd = 2)
  arrows(x0 = apply(post$gamma[, 2:6], 2, function(x) quantile(x, 0.05)), 
         x1 = apply(post$gamma[, 2:6], 2, function(x) quantile(x, 0.95)),
         y0 = 2:6, 
         y1 = 2:6, 
         length = 0, 
         lwd = 2)
  
  abline(v = 0, lty = 2, lwd = 2) 
  mtext(side = 2, at = 2:6, text = labels[2:6], las = 2)
  
}

dev.off()

# alpha caterpillar plot

png("output/figures/irt_alpha_caterpillar_diff.png", 
    res = 250, 
    height = 1000, 
    width = 4000)

par(mfrow = c(1, 4), mar = c(5, 5.5, 4, 2)) 

for (i in 1:4) {
  
  post <- extract.samples(fit[[i]])
  
  plot(NULL, 
       xlim = c(0, 3), 
       ylim = c(1, 7), 
       xlab = "", 
       ylab = "", 
       yaxt = "n", 
       cex.axis = 1.5)
  
  mtext(site_names[i])
  
  points(apply(post$alpha[, 2:6], 2, mean), 2:6, lwd = 2)
  arrows(x0 = apply(post$alpha[, 2:6], 2, function(x) quantile(x, 0.05)), 
         x1 = apply(post$alpha[, 2:6], 2, function(x) quantile(x, 0.95)),
         y0 = 2:6, 
         y1 = 2:6, 
         length = 0, 
         lwd = 2)
  
  abline(v = 1, lty = 2, lwd = 2) 
  mtext(side = 2, at = 2:6, text = labels[2:6], las = 2)
  
}

dev.off()

# curve plot

# plot the probability an individual answers 4/5, given position on the latent axis

cols <- c("#440154FF", "#414487FF", "#2A788EFF", "#22A884FF", "#7AD151FF", "#FDE725FF")

png("output/figures/irt_curve_diff.png", 
    res = 250, 
    height = 1000, 
    width = 4000)

par(mfrow = c(1, 4))

for (i in 1:4) {
  
  post <- extract.samples(fit[[i]])
  
  plot(NULL, 
       xlim = c(-6, 6), 
       ylim = c(0, 1), 
       ylab = "", 
       xlab = "latent mental health score")
  
  mtext(site_names[i])
  
  x <- seq(-6, 6, length.out = 200) # from -6 to 6 on a latent axis
  
  p <- matrix(NA, nrow = 6, ncol = 200)
  
  for (j in 1:6) {
  for (q in 1:200) {
     
      p[j, q] = mean(inv_logit(post$alpha[, j]*(x[q] - post$gamma[, j]) - post$c[, 3]))
  }
  }
  
  for (z in 1:6) {
   
    lines(p[z, ] ~ x, 
          col = cols[z], 
          lwd = 2)
    
    abline(v = 0, lty = 2, lwd = 2)
    
    legend("topleft", 
           legend = c("sad", 
                      "anxious", 
                      "nervous",
                      "hopeless", 
                      "worthless", 
                      "tired"), 
           fill = cols, 
           bty = "n") 
  }
  
}

dev.off()

# check that question sum loads positive against the linear model/ xi params

list_data <- list(data_PC_Curr_Married, 
                  data_WL_Curr_Married, 
                  data_WH_Curr_Married, 
                  data_IH_Curr_Married)

png("output/figures/irt_question_sums_diff.png", 
    res = 250, 
    height = 1000, 
    width = 4000)

par(mfrow = c(1, 4))

for (i in 1:4) {
  
  post <- extract.samples(fit[[i]])
  
  d <- list_data[[i]]
  K <- cbind(d$Sad, d$Anxious, d$Nervous, d$Hopeless, d$Worthless, d$Tired)

  plot(apply(post$zeta, 2, mean), 
       apply(K, 1, sum),
       xlab = "position on latent axis", 
       ylab = "question sum", 
       cex.lab = 1.5)
  
  mtext(site_names[i])

}

dev.off()

# make data for ggplot

site_names <- c("coastal", "lowland", "highland", "altiplano")
sex <- c("women", "men")
effect <- c("wealth", "education", "difference")

est <- list()
site_est <- list()
sex_est <- list()

for (i in 1:4) { # sites
  
  post <- extract.samples(fit[[i]])

for (s in 1:2) { # sexes
for (b in 1:3) { # parameters

    est[[b]] <- data.frame(sex = sex[s], 
                           site = site_names[i], 
                           est = quantile(post$beta[ , s, b], 0.5), 
                           ymin = quantile(post$beta[ , s, b], 0.05),
                           ymax = quantile(post$beta[ , s, b], 0.95), 
                           term = effect[b])
  }
  sex_est[[s]] <- do.call(rbind, est)
}
  site_est[[i]] <- do.call(rbind, sex_est)
}

plot_data <- do.call(rbind, site_est)

plot_real <- plot_data
plot_real$model <- "realized gap"

# 2: divergence model, long

fit <- readRDS("stanfits/fit-sad-long-div.rds")

# check fit

precis(fit[[1]], 3, pars = pars)
precis(fit[[2]], 3, pars = pars)
precis(fit[[3]], 3, pars = pars)
precis(fit[[4]], 3, pars = pars)

traceplot(fit[[1]], 3, pars = "beta")
traceplot(fit[[2]], 3, pars = "beta")
traceplot(fit[[3]], 3, pars = "beta")
traceplot(fit[[4]], 3, pars = "beta")

traceplot(fit[[1]], 3, pars = "alpha")
traceplot(fit[[2]], 3, pars = "alpha")
traceplot(fit[[3]], 3, pars = "alpha")
traceplot(fit[[4]], 3, pars = "alpha")

traceplot(fit[[1]], 3, pars = "gamma")
traceplot(fit[[2]], 3, pars = "gamma")
traceplot(fit[[3]], 3, pars = "gamma")
traceplot(fit[[4]], 3, pars = "gamma")

traceplot(fit[[1]], 3, pars = "c")
traceplot(fit[[2]], 3, pars = "c")
traceplot(fit[[3]], 3, pars = "c")
traceplot(fit[[4]], 3, pars = "c")

# save parameter summaries

sum <- list()

for (i in 1:4) {
  
  sum[[i]] <- precis(fit[[i]], 3, pars = c("beta", "gamma", "alpha", "eta", "kappa", "tau", "delta", "sigma"))
  rownames(sum[[i]]) <- c("$\\phi_{1, 1}$", "$\\phi_{1, 2}$", "$\\phi_{1, 3}$", 
                          "$\\phi_{2, 1}$", "$\\phi_{2, 2}$", "$\\phi_{2, 3}$", 
                          "$\\gamma_{1}$", "$\\gamma_{2}$", "$\\gamma_{3}$", 
                          "$\\gamma_{4}$", "$\\gamma_{5}$", "$\\gamma_{6}$", 
                          "$\\alpha_{1}$", "$\\alpha_{2}$", "$\\alpha_{3}$", 
                          "$\\alpha_{4}$", "$\\alpha_{5}$", "$\\alpha_{6}$", 
                          "$\\eta_{1}$", "$\\eta_{2}$", 
                          "$\\kappa_{1}$", "$\\kappa_{2}$", 
                          "$\\tau_{1}$", "$\\tau_{2}$", 
                          "$\\delta_{1}$", "$\\delta_{2}$",
                          "$\\sigma$")
  
  print(xtable(sum[[i]]), 
        file = paste0("output/tables/par_summary_sad_div_long", i, ".txt"), 
        only.contents = TRUE, 
        sanitize.rownames.function = function(x) {x})
  
}

# gamma caterpillar plot

labels <- c("sad", "anxious", "nervous", "hopeless", "worthless", "tired")
site_names <- c("coastal", "lowland", "highland", "altiplano")

png("output/figures/irt_gamma_caterpillar_div_long.png", 
    res = 250, 
    height = 1000, 
    width = 4000)

par(mfrow = c(1, 4), mar = c(5, 5.5, 4, 2)) 

for (i in 1:4) {
  
  post <- extract.samples(fit[[i]])
  
  plot(NULL, 
       xlim = c(-4, 4), 
       ylim = c(1, 7), 
       xlab = "", 
       ylab = "", 
       yaxt = "n", 
       cex.axis = 1.5)
  
  mtext(site_names[i])
  
  points(apply(post$gamma[, 2:6], 2, mean), 2:6, lwd = 2)
  arrows(x0 = apply(post$gamma[, 2:6], 2, function(x) quantile(x, 0.05)), 
         x1 = apply(post$gamma[, 2:6], 2, function(x) quantile(x, 0.95)),
         y0 = 2:6, 
         y1 = 2:6, 
         length = 0, 
         lwd = 2)
  
  abline(v = 0, lty = 2, lwd = 2) 
  mtext(side = 2, at = 2:6, text = labels[2:6], las = 2)
  
}

dev.off()

# alpha caterpillar plot

png("output/figures/irt_alpha_caterpillar_div_long.png", 
    res = 250, 
    height = 1000, 
    width = 4000)

par(mfrow = c(1, 4), mar = c(5, 5.5, 4, 2)) 

for (i in 1:4) {
  
  post <- extract.samples(fit[[i]])
  
  plot(NULL, 
       xlim = c(0, 3), 
       ylim = c(1, 7), 
       xlab = "", 
       ylab = "", 
       yaxt = "n", 
       cex.axis = 1.5)
  
  mtext(site_names[i])
  
  points(apply(post$alpha[, 2:6], 2, mean), 2:6, lwd = 2)
  arrows(x0 = apply(post$alpha[, 2:6], 2, function(x) quantile(x, 0.05)), 
         x1 = apply(post$alpha[, 2:6], 2, function(x) quantile(x, 0.95)),
         y0 = 2:6, 
         y1 = 2:6, 
         length = 0, 
         lwd = 2)
  
  abline(v = 1, lty = 2, lwd = 2) 
  mtext(side = 2, at = 2:6, text = labels[2:6], las = 2)
  
}

dev.off()

# curve plot

# plot the probability an individual answers 4/5, given position on the latent axis

cols <- c("#440154FF", "#414487FF", "#2A788EFF", "#22A884FF", "#7AD151FF", "#FDE725FF")

png("output/figures/irt_curve_div_long.png", 
    res = 250, 
    height = 1000, 
    width = 4000)

par(mfrow = c(1, 4))

for (i in 1:4) {
  
  post <- extract.samples(fit[[i]])
  
  plot(NULL, 
       xlim = c(-6, 6), 
       ylim = c(0, 1), 
       ylab = "", 
       xlab = "latent mental health score")
  
  mtext(site_names[i])
  
  x <- seq(-6, 6, length.out = 200) # from -6 to 6 on a latent axis
  
  p <- matrix(NA, nrow = 6, ncol = 200)
  
  for (j in 1:6) {
    for (q in 1:200) {
      
      p[j, q] = mean(inv_logit(post$alpha[, j]*(x[q] - post$gamma[, j]) - post$c[, 3]))
    }
  }
  
  for (z in 1:6) {
    
    lines(p[z, ] ~ x, 
          col = cols[z], 
          lwd = 2)
    
    abline(v = 0, lty = 2, lwd = 2)
    
    legend("topleft", 
           legend = c("sad", 
                      "anxious", 
                      "nervous",
                      "hopeless", 
                      "worthless", 
                      "tired"), 
           fill = cols, 
           bty = "n") 
  }
  
}

dev.off()

# question sum plot

png("output/figures/irt_question_sums_div_long.png", 
    res = 250, 
    height = 1000, 
    width = 4000)

par(mfrow = c(1, 4))

for (i in 1:4) {
  
  post <- extract.samples(fit[[i]])
  
  d <- list_data[[i]]
  K <- cbind(d$Sad, d$Anxious, d$Nervous, d$Hopeless, d$Worthless, d$Tired)
  
  plot(apply(post$zeta, 2, mean), 
       apply(K, 1, sum),
       xlab = "position on latent axis", 
       ylab = "question sum", 
       cex.lab = 1.5)
  
  mtext(site_names[i])
  
}

dev.off()

# make data for ggplot

est <- list()
site_est <- list()
sex_est <- list()

for (i in 1:4) { # sites
  
  post <- extract.samples(fit[[i]])
  
for (s in 1:2) { # sexes
for (b in 1:3) { # parameters
  
      est[[b]] <- data.frame(sex = sex[s], 
                             site = site_names[i], 
                             est = quantile(post$beta[ , s, b], 0.5), 
                             ymin = quantile(post$beta[ , s, b], 0.05),
                             ymax = quantile(post$beta[ , s, b], 0.95), 
                             term = effect[b])
  }
    sex_est[[s]] <- do.call(rbind, est)
  }
    site_est[[i]] <- do.call(rbind, sex_est)
}

plot_data <- do.call(rbind, site_est)

plot_long <- plot_data
plot_long$model <- "divergence, long"

# 3: divergence model, short

fit <- readRDS("stanfits/fit-sad-short-div.rds")

# check fit
pars <- c("alpha", "beta", "gamma", "delta", "tau", "kappa", "c", "sigma")

precis(fit[[1]], 3, pars = pars)
precis(fit[[2]], 3, pars = pars)
precis(fit[[3]], 3, pars = pars)
precis(fit[[4]], 3, pars = pars)

traceplot(fit[[1]], 3, pars = "beta")
traceplot(fit[[2]], 3, pars = "beta")
traceplot(fit[[3]], 3, pars = "beta")
traceplot(fit[[4]], 3, pars = "beta")

traceplot(fit[[1]], 3, pars = "alpha")
traceplot(fit[[2]], 3, pars = "alpha")
traceplot(fit[[3]], 3, pars = "alpha")
traceplot(fit[[4]], 3, pars = "alpha")

traceplot(fit[[1]], 3, pars = "gamma")
traceplot(fit[[2]], 3, pars = "gamma")
traceplot(fit[[3]], 3, pars = "gamma")
traceplot(fit[[4]], 3, pars = "gamma")

traceplot(fit[[1]], 3, pars = "c")
traceplot(fit[[2]], 3, pars = "c")
traceplot(fit[[3]], 3, pars = "c")
traceplot(fit[[4]], 3, pars = "c")

traceplot(fit[[1]], 3, pars = "sigma")
traceplot(fit[[2]], 3, pars = "sigma")
traceplot(fit[[3]], 3, pars = "sigma")
traceplot(fit[[4]], 3, pars = "sigma")

# save parameter summaries

sum <- list()

for (i in 1:4) {
  
  sum[[i]] <- precis(fit[[i]], 3, pars = c("beta", "gamma", "alpha", "eta", "kappa", "tau", "delta", "sigma"))
  rownames(sum[[i]]) <- c("$\\phi_{1, 1}$", "$\\phi_{1, 2}$", "$\\phi_{1, 3}$", 
                          "$\\phi_{2, 1}$", "$\\phi_{2, 2}$", "$\\phi_{2, 3}$", 
                          "$\\gamma_{1}$", "$\\gamma_{2}$", "$\\gamma_{3}$", 
                          "$\\gamma_{4}$", "$\\gamma_{5}$", "$\\gamma_{6}$", 
                          "$\\alpha_{1}$", "$\\alpha_{2}$", "$\\alpha_{3}$", 
                          "$\\alpha_{4}$", "$\\alpha_{5}$", "$\\alpha_{6}$", 
                          "$\\eta_{1}$", "$\\eta_{2}$", 
                          "$\\kappa_{1}$", "$\\kappa_{2}$", 
                          "$\\tau_{1}$", "$\\tau_{2}$", 
                          "$\\delta_{1}$", "$\\delta_{2}$",
                          "$\\sigma$")
  
  print(xtable(sum[[i]]), 
        file = paste0("output/tables/par_summary_sad_div_short", i, ".txt"), 
        only.contents = TRUE, 
        sanitize.rownames.function = function(x) {x})
  
}

# gamma caterpillar plot

labels <- c("sad", "anxious", "nervous", "hopeless", "worthless", "tired")
site_names <- c("coastal", "lowland", "highland", "altiplano")

png("output/figures/irt_gamma_caterpillar_div_short.png", 
    res = 250, 
    height = 1000, 
    width = 4000)

par(mfrow = c(1, 4), mar = c(5, 5.5, 4, 2)) 

for (i in 1:4) {
  
  post <- extract.samples(fit[[i]])
  
  plot(NULL, 
       xlim = c(-4, 4), 
       ylim = c(1, 7), 
       xlab = "", 
       ylab = "", 
       yaxt = "n", 
       cex.axis = 1.5)
  
  mtext(site_names[i])
  
  points(apply(post$gamma[, 2:6], 2, mean), 2:6, lwd = 2)
  arrows(x0 = apply(post$gamma[, 2:6], 2, function(x) quantile(x, 0.05)), 
         x1 = apply(post$gamma[, 2:6], 2, function(x) quantile(x, 0.95)),
         y0 = 2:6, 
         y1 = 2:6, 
         length = 0, 
         lwd = 2)
  
  abline(v = 0, lty = 2, lwd = 2) 
  mtext(side = 2, at = 2:6, text = labels[2:6], las = 2)
  
}

dev.off()

# alpha caterpillar plot

png("output/figures/irt_alpha_caterpillar_div_short.png", 
    res = 250, 
    height = 1000, 
    width = 4000)

par(mfrow = c(1, 4), mar = c(5, 5.5, 4, 2)) 

for (i in 1:4) {
  
  post <- extract.samples(fit[[i]])
  
  plot(NULL, 
       xlim = c(0, 3), 
       ylim = c(1, 7), 
       xlab = "", 
       ylab = "", 
       yaxt = "n", 
       cex.axis = 1.5)
  
  mtext(site_names[i])
  
  points(apply(post$alpha[, 2:6], 2, mean), 2:6, lwd = 2)
  arrows(x0 = apply(post$alpha[, 2:6], 2, function(x) quantile(x, 0.05)), 
         x1 = apply(post$alpha[, 2:6], 2, function(x) quantile(x, 0.95)),
         y0 = 2:6, 
         y1 = 2:6, 
         length = 0, 
         lwd = 2)
  
  abline(v = 1, lty = 2, lwd = 2) 
  mtext(side = 2, at = 2:6, text = labels[2:6], las = 2)
  
}

dev.off()

# curve plot

# plot the probability an individual answers 4/5, given position on the latent axis

cols <- c("#440154FF", "#414487FF", "#2A788EFF", "#22A884FF", "#7AD151FF", "#FDE725FF")

png("output/figures/irt_curve_div_short.png", 
    res = 250, 
    height = 1000, 
    width = 4000)

par(mfrow = c(1, 4))

for (i in 1:4) {
  
  post <- extract.samples(fit[[i]])
  
  plot(NULL, 
       xlim = c(-6, 6), 
       ylim = c(0, 1), 
       ylab = "", 
       xlab = "latent mental health score")
  
  mtext(site_names[i])
  
  x <- seq(-6, 6, length.out = 200) # from -6 to 6 on a latent axis
  
  p <- matrix(NA, nrow = 6, ncol = 200)
  
  for (j in 1:6) {
    for (q in 1:200) {
      
      p[j, q] = mean(inv_logit(post$alpha[, j]*(x[q] - post$gamma[, j]) - post$c[, 3]))
    }
  }
  
  for (z in 1:6) {
    
    lines(p[z, ] ~ x, 
          col = cols[z], 
          lwd = 2)
    
    abline(v = 0, lty = 2, lwd = 2)
    
    legend("topleft", 
           legend = c("sad", 
                      "anxious", 
                      "nervous",
                      "hopeless", 
                      "worthless", 
                      "tired"), 
           fill = cols, 
           bty = "n") 
  }
  
}

dev.off()

# question sum plot

png("output/figures/irt_question_sums_div_short.png", 
    res = 250, 
    height = 1000, 
    width = 4000)

par(mfrow = c(1, 4))

for (i in 1:4) {
  
  post <- extract.samples(fit[[i]])
  
  d <- list_data[[i]]
  K <- cbind(d$Sad, d$Anxious, d$Nervous, d$Hopeless, d$Worthless, d$Tired)
  
  plot(apply(post$zeta, 2, mean), 
       apply(K, 1, sum),
       xlab = "position on latent axis", 
       ylab = "question sum", 
       cex.lab = 1.5)
  
  mtext(site_names[i])
  
}

dev.off()

# make data for ggplot

est <- list()
site_est <- list()
sex_est <- list()

for (i in 1:4) { # sites
  
  post <- extract.samples(fit[[i]])
  
for (s in 1:2) { # sexes
for (b in 1:3) { # parameters
      
  est[[b]] <- data.frame(sex = sex[s], 
                         site = site_names[i], 
                         est = quantile(post$beta[ , s, b], 0.5), 
                         ymin = quantile(post$beta[ , s, b], 0.05),
                         ymax = quantile(post$beta[ , s, b], 0.95), 
                         term = effect[b])
  }
    sex_est[[s]] <- do.call(rbind, est)
  }
    site_est[[i]] <- do.call(rbind, sex_est)
}

plot_data <- do.call(rbind, site_est)

plot_short <- plot_data
plot_short$model <- "divergence, short"

# bind them all together
plot_all <- rbind(plot_real, plot_long, plot_short)

# reorder sites
plot_all$site <- factor(plot_all$site)
plot_all$site <- factor(plot_all$site, levels = c("coastal", "lowland", "highland", "altiplano"))

# reorder coef
plot_all$term <- factor(plot_all$term)
plot_all$term <- factor(plot_all$term, levels = c("difference", "education", "wealth"))

# reorder model
plot_all$model <- factor(plot_all$model)
plot_all$model <- factor(plot_all$model, levels = c("realized gap", "divergence, long", "divergence, short"))

ggplot(plot_all, 
       aes(x = term, 
           y = est, 
           color = sex)) +
  
  coord_flip() +
  
  facet_grid(model ~ site) +
  
  geom_hline(yintercept = 0, linetype = "dashed") +
  
  geom_linerange(aes(ymin = ymin, ymax = ymax), 
                 position = position_dodge(width = 0.35), size=1.25) +
  
  geom_point(position = position_dodge(width = 0.35),size=2) +

  scale_color_manual(values = c("women" = "turquoise4",
                                "men" = "goldenrod3"))+
  
  theme(strip.text.x = element_text(size = 14, face = "bold"),
        strip.text.y = element_text(size = 14, face = "bold"), 
        axis.text = element_text(size = 12),
        legend.key.size = unit(1, "cm"), 
        legend.text = element_text(size = 12), 
        legend.title = element_blank(), 
        legend.position = "bottom") + 
  
  xlab("") + 
  ylab("estimated parameter value (log odds)")

ggsave(file = "output/figures/m_sad_coef.png",
       dpi = 300, 
       height = 8, 
       width = 12)

# Coastal site:
# For women, as the divergence between actual and ideal increases, mental health worsens
# Also, increasing wealth of women negatively impacts mental health
# For men, no effect on MH

# Lowland site:
# No effect of divergence, but as wealth of women increases, so does mental health
# No effect for men

# Highland site:
# No effect on women of any factors on mental health
# Men's mental health worsens as education improves, and men's mental health improves as the divergence increases, which is odd

# Altiplano site:
# For women, no effect
# For men, as the divergence increases mental health worsens, and as wealth increases, mental health improves

# Coastal site:
# No effects

# Lowland site: 
# Wealth improves mental health of women
# Education improves mental health of men 

# Highland site
# Divergence decreases mental health of women (tentatively)
# Education decreases mental health of men

# Altiplano site
# No effects on womens mental health
# Wealth improves mental health of men 
