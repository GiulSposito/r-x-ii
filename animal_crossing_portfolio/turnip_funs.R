#for all init like
#initial_prices <- c(rep(sample(90:110, 1), 2), rep(0, 12))

#price goes up and down over week
#x <- sim_fluctuating(prices = initial_prices, first_epochs = c(sample(0:6, 1), sample(2:3, 1)))
sim_fluctuating <- function(epochs = 12, prices, first_epochs = c(3, 3)) {
  high_epochs <- c(first_epochs[1], sample(1:(7-first_epochs[1]), 1))
  high_epochs[3] <- 7 - sum(high_epochs)

  low_epochs <- c(first_epochs[2], 5 - first_epochs[2])
  
  epochs <- c(2, high_epochs[1], low_epochs[1], high_epochs[2], low_epochs[2], high_epochs[3])
  e <<- epochs
  epoch_day <- 2
  
  if(epochs[2] > 0) {
    for(phase_day in seq(epochs[2])) {
      epoch_day <- epoch_day + 1
      rate <- runif(1, 0.9, 1.4)
      prices[epoch_day] <- ceiling(prices[1] * rate)
    }
  }
  
  rate <- runif(1, 0.6, 0.8)
  for(epoch in seq(epochs[3])) {
    epoch_day <- epoch_day + 1
    prices[epoch_day] <- ceiling(prices[1] * rate)
    rate <- rate - 0.04 - runif(1, 0, 0.06)
  }
  
  for(epoch in seq(epochs[4])) {
    epoch_day <- epoch_day + 1
    rate <- runif(1, 0.9, 1.4)
    prices[epoch_day] <- ceiling(prices[1] * rate)
  }
  
  rate <- runif(1, 0.6, 0.8)
  for(epoch in seq(epochs[5])) {
    epoch_day <- epoch_day + 1
    prices[epoch_day] <- ceiling(prices[1] * rate)
    rate <- rate - 0.04 - runif(1, 0, 0.06)
  }
  
  if(epochs[6] > 0) {
    for(epoch in seq(epochs[6])) {
      epoch_day <- epoch_day + 1
      rate <- runif(1, 0.9, 1.4)
      prices[epoch_day] <- ceiling(prices[1] * rate)
    }
  }
  
  return(prices)
}

#price decreases before a large spike midweek
#x <- sim_largespike(prices = initial_prices, rate = runif(1, 0.85, 0.95), first_peak = sample(2:8, 1))
sim_largespike <- function(rate = 0.9, first_peak = 5, prices) {
  epochs <- c(2, 2+first_peak, 14)

  for(epoch in (epochs[1]+1):(epochs[2]-1)) {
    prices[epoch] <- ceiling(prices[1] * rate)
    
    rate <- rate - 0.03
    rate <- rate - runif(1, 0, 0.02)
  }
  
  prices[epochs[2]] <- ceiling(prices[1] * runif(1, 0.9, 1.4))
  prices[epochs[2] + 1] <- ceiling(prices[1] * runif(1, 1.4, 2))
  prices[epochs[2] + 2] <- ceiling(prices[1] * runif(1, 2, 6))
  prices[epochs[2] + 3] <- ceiling(prices[1] * runif(1, 1.4, 2))
  prices[epochs[2] + 4] <- ceiling(prices[1] * runif(1, 0.9, 1.4))
  
  if(epochs[2] + 4 < 14) {
    for(epoch in (epochs[2]+5):epochs[3]) {
      rate <- runif(1, 0.4, 0.9)
      prices[epoch] <- ceiling(prices[1] * rate)
    }
  }
  
  return(prices)
}

#price decreases over the week
#x <- sim_decreasing(prices = initial_prices)
sim_decreasing <- function(rate = 0.9, epochs = 12, prices) {
  extra_decrease <- runif(1, 0, 0.05)
  rate <- rate - extra_decrease
  
  for(epoch in seq(2+epochs)) {
    prices[epoch] <- ceiling(prices[1] * rate)
    
    rate <- rate - 0.03
    rate <- rate - runif(1, 0, 0.02)
  }
  
  return(prices)
}

#prices have a small spike midweek
#x <- sim_smallspike(prices = initial_prices, first_peak = sample(1:8, 1))
sim_smallspike <- function(rate = 0.9, first_peak = 5, prices) {
  epochs <- c(2, 2+first_peak, 14)
  
  if(epochs[2] - epochs[1] > 1) {
    for(epoch in (epochs[1]+1):(epochs[2]-1)) {
      prices[epoch] <- ceiling(prices[1] * rate)
      
      rate <- rate - 0.03
      rate <- rate - runif(1, 0, 0.02)
    }
  }
  
  prices[epochs[2]] <- ceiling(prices[1] * runif(1, 0.9, 1.4))
  prices[epochs[2] + 1] <- ceiling(prices[1] * runif(1, 0.9, 1.4))
  rate <- runif(1, 1.4, 2)
  prices[epochs[2] + 2] <- ceiling(prices[1] * runif(1, 1.4, rate)) - 1
  prices[epochs[2] + 3] <- ceiling(prices[1] * rate)
  prices[epochs[2] + 4] <- ceiling(prices[1] * runif(1, 1.4, rate)) - 1
  
  rate <- runif(1, 0.4, 0.9)
  if(epochs[2] + 4 < 14) {
    for(epoch in (epochs[2]+5):epochs[3]) {
      prices[epoch] <- ceiling(prices[1] * rate)
      rate <- rate - 0.03
      rate <- rate - runif(1, 0, 0.02)
    }
  }
  
  return(prices)
}
