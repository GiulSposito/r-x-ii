library(mgcv)
#library(gamair)
data("mpg", package="gamair")

head(mpg)
plot(mpg$weight, mpg$hw.mpg)

# one variable
model <- gam(hw.mpg ~ s(weight), data=mpg, method="REML")
par(mfrow = c(1, 1))
plot(model, residuals = T, pch=1)

# two variable
model2 <- gam(hw.mpg ~ s(weight) + s(length), data=mpg, method="REML")
par(mfrow = c(1, 2))
plot(model2, residuals = T, pch=1)

# two variable (one linear)
model3 <- gam(hw.mpg ~ length + s(weight), data=mpg, method="REML")
par(mfrow = c(1, 1))
plot(model3, residuals = T, pch=1)

# categorical variables
str(mpg)

model3 <- gam(hw.mpg ~ s(weight) + fuel, data = mpg, method = "REML")
par(mfrow = c(1, 2))
plot(model3, residuals = T, pch=1)
termplot(model3)

model4 <- gam(hw.mpg ~ s(weight, by = fuel), data = mpg, method = "REML")
plot(model4, residuals = T, pch=1)
termplot(model3, se = T)

model4b <- gam(hw.mpg ~ s(weight, by = fuel) + fuel, data = mpg, method = "REML")
par(mfrow = c(1, 3))
plot(model4b, residuals = T, pch=1)
termplot(model4b, se = T)

## City

# Examine the data
head(mpg)
str(mpg)

# Fit the model
mod_city <- gam(city.mpg ~ s(weight) + s(length) + s(price), 
                data = mpg, method = "REML")

# Plot the model
#par(mfrow = c(2, 2))
plot(mod_city, pages = 1)


# Fit the model
mod_city3 <- gam(city.mpg ~ s(weight, by=drive) + s(length, by=drive) + s(price, by=drive) + drive, 
                 data = mpg, method = "REML")

# Plot the model
plot(mod_city3, pages = 1)


### Interpreting

mod_hwy <- gam(hw.mpg ~ s(weight) + s(rpm) +  s(price) + s(comp.ratio) + s(width) + fuel + cylinders,
               data = mpg, method = "REML")

summary(mod_hwy)
plot(mod_hwy, pages=1)
