# database
mcycle <- MASS::mcycle
head(mcycle)
plot(mcycle$times, mcycle$accel)


# linear model
lm_mod <- lm(accel~times, data=mcycle)
termplot(lm_mod, partial.resid = T, se=T)

# gam model
library(mgcv)
gam_mod <- gam(accel ~ s(times), data=mcycle )
plot(gam_mod, residuals = T, pch=1)
summary(gam_mod)

# base functions
coef(gam_mod)

# smoothing parameter
# FIT = LIKEHOOD - (LAMBDA)*WIGGLINESS
gm_auto <- gam(accel ~ s(times), data = mcycle)
gm_sp1  <- gam(accel ~ s(times, sp = 0.42), data = mcycle)
gm_reml <- gam(accel ~ s(times), data = mcycle, method = "REML")

par(mfrow = c(1, 3))
plot(gm_auto, residuals = TRUE, pch = 1)
plot(gm_sp1,  residuals = TRUE, pch = 1)
plot(gm_reml, residuals = TRUE, pch = 1)

str(gm_sp1)


# number of basis function

# Fit a GAM with 3 basis functions
gam_mod_k3 <- gam(accel ~ s(times, k = 3), data = mcycle)

# Fit with 20 basis functions
gam_mod_k20 <- gam(accel ~ s(times, k = 20), data = mcycle)

# Visualize the GAMs
par(mfrow = c(1, 2))
plot(gam_mod_k3, residuals = TRUE, pch = 1)
plot(gam_mod_k20, residuals = TRUE, pch = 1)

# visualizing smooth parameter
gm_reml$smooth[[1]]$sp


# Fit the GAM with 50 base
gam_mod_sk <- gam(accel~s(times, k=50), data=mcycle, sp=0.0001)

# Visualize the model
plot(gam_mod_sk, residuals = TRUE, pch = 1)