library(knitr)
library(plotrix)

# set the random seed
set.seed(1211)
par(xpd=NA)

# set the assumptions
lambda <- .2 
n <- 40
nosim <- 1000

#Generating 1000 random exponential distributions
ranex <- rexp(1000, lambda)

par(mfrow = c(1,1))
myhist <- hist(ranex , freq = FALSE, xlim = c(0, 20), ylim = c(0, .55), 
               breaks = 25, main = paste("Probability density function for n = 1000"), 
               xlab = "Values")

# calculate the mean and standard deviation of the 1000 samples
meanex <- mean(ranex)
sdex <- sd(ranex)


meanex
sdex

# plot the mean value from the 1000 samples
abline(v = meanex , col = "black", lwd = 2, lty = 1)

#calculate the theoretical mean, standard deviation of the exponential distribution

emean <-1/lambda
esd <- 1/lambda

# clear the vectors
mean_val <- NULL
mean_sd <- NULL

for (i in 1:nosim){
  # calculate the mean & sd of all the sample means
  val <- rexp(n, lambda)
  means <- mean(val); sd <- sd(val)
  mean_val  <- c(mean_val, means); mean_sd <- c(mean_sd, sd)
}

#plot of the means
myhist <- hist(mean_val , freq = TRUE, xlim = c(2, 8), 
               main = paste("Histogram of", nosim, "n = 40 Simulations"), xlab = "Values")

# calculate the mean, standard deviation and variance of the aggregated sample means
smean <- mean(mean_val)
ssd <- sd(mean_val)
svar <- ssd^2

#calculate the theoretical mean, standard deviation and variance of X-bar

tmean <-1/lambda
tsd <- (1/lambda) / sqrt(n)
tvar <- (1/lambda)^2 / n

# Compare Sample Mean and Theoretical Mean of Xbar
smean; tmean

# Compare Sample Variance and Theoretical Variance of Xbar
svar; tvar


# Investigating the Distribution of the Simulated sample means from the Exponential Distribution
# plot the 1000 - histogram of probability density od sample mean
par(mfrow = c(1,1))
myhist <- hist(mean_val , freq = FALSE, xlim = c(2, 8), ylim = c(0, .55), 
               breaks = 25, main = paste("Probability density function for", nosim, "simulations"), 
               xlab = "Values")

# plot the theoretical distribution for simulated sample means
x <- seq(min(mean_val ), max(mean_val ), length = 100) 
y <- dnorm(x, mean = smean, sd = ssd)
curve(dnorm(x, mean = smean, sd = ssd), 
      col = "gray", lwd = 3, lty = 3, add = TRUE)

# plot the mean value from the data set
abline(v = smean , col = "steelblue", lwd = 3, lty = 2)

# plot the expected value of an exponential distribution
abline(v = 5, col = "red", lwd = 3, lty = 9)


legend('topright', c("Theoretical Mean", "Mean of Simulated Sample Means", "Normal Distribution"), 
       lty=1, col=c('red', 'steelblue', 'gray'), bty='n', cex=.75)

# Comparing Sample Variance and Theoretical Variance of Xbar
ssd
tsd

qqnorm(mean_val, col = "lightskyblue1")
qqline(mean_val)
