
rm(list=ls())

getwd()
setwd("C:/Users/cayde/OneDrive/Graduate Studies/Misc/TA/Lab 102/Data Sets")

#################
# Simulating Data
#################



# Histogram of repeated samplings from simulated data
#####################################################

# Create a fake dataset  

  N <- 1000 # Set the number of observations to 1000
  i <- seq(1, N, by = 1) # Create a variable sequenced from 1 to 1000
  i
  
  set.seed(12345) # makes random draws replicable; can be any combination on numbers
  v <- rnorm(N, mean = 75, sd = 7) # rnorm(Number of draws, mean of distribution, sd)
  summary(v)

# Sample 300 respondents and calculate the mean of v for the sample

  s <- 300 #set the sample size
  dat <- sample(v, size = s, replace = FALSE) #sample a certain number of items (s) from a dataset (v) and don't replace (replace = FALSE)
  mean(dat) #get the mean of that sample 

# Repeat this process 10000 times

  times <- 10000 
  means <- NULL # create empty vector for means
  
  for (j in 1:times){
    dat <- sample(v, size = s, replace = FALSE)
    means[j] <-mean(dat)
  }
  
# Create histogram of sample means
  
  hist(means, prob = T, breaks = 20, xlab = "Sample Means", col = "lightblue")
  lines(density(means), col = "darkblue", lwd = 2)
  # add lines for mean of sample means and theoretical mean  
  m1 <- mean(v)
  m2 <- mean(means)
  abline(v = m1, col = "red", lty = 2, lwd = 2)
  abline(v = m2, col = "purple", lty = 2, lwd =2)

# T-test on simulated data
###########################

#Create fake data
  
  set.seed(22418)  #set seed to ensure replicability
  
  g <- rbinom(N, size = 1, prob = 0.50) #rbinom(number of obs., number of trials, probability of 1 in each trial) 
  table(g)
  
  # use if else statement to create variable with different means for each group
  d1 <- rnorm(N, mean = 3, sd = 1) #create data for group 1
  d2 <- rnorm(N, mean = 1, sd = 1) #create data for group 2
  v2 <- ifelse(g == 1, d1, d2) #ifelse(condition, yes, no)
  
  dat <- cbind(i, g, d1, d2,v2) # create matrix to ensure it worked
  View(dat)
  
#Conduct a t test
  
  mean(v2[g == 1])
  mean(v2[g==0])
  t.test(v2~g, var.equal =TRUE)
  # calculate difference in means
  diff <- mean(v2[g==1])-mean(v2[g==0])
  diff

# Sample 300 respondents from the data and get the difference in means
  
  s <- 300 #set sample size
  
  index <- sample(i, size = s, replace = FALSE) # take sample of individuals to index dat
 
  d <- data.frame(dat[index,])  # create new dataframe using sample index
 
  mean(d$v2[d$g==1]) #calculate mean for group 1
  mean(d$v2[d$g==0]) #calculate mean for group 2
  mean(d$v2[d$g==1])-mean(d$v2[d$g==0])  # calculate difference

# Repeat this process 10000 times

  times <- 10000 #set number of times
  s <- 300 #set sample size
  diffs <- NULL #create empty vector for means
  
  for (j in 1:times){
    index <- sample(i, size = s, replace = FALSE)
    d <- data.frame(dat[index,])
    diffs[j] <-mean(d$v2[d$g==1])-mean(d$v2[d$g==0]) #store the difference in means in the j'th position 
  }
  
  # look at summary of diffs
  summary(diffs)
  
# create histogram of differences
  hist(diffs, prob = TRUE, main = "Histogram of Differences in Means",
       xlab = "Differences in Means",
       col = "blanchedalmond")
  lines(density(diffs), col = "mediumorchid", lwd = 2)
  # add mean of diffs and true diff
  d <- mean(diffs)
  abline(v = diff, col = "maroon", lty = 2,lwd = 2)
  abline(v = d, col = "darkorchid", lty = 4, lwd = 2)


# What if we repeat the first section but change the sample size each time?
# Let's write a function that will take the mean and sd and number of times, number of obs,  and sample size
  graph_means <- function(m, sd, times, N, s){
    
    set.seed(12345)
    v <- rnorm(N, mean = m, sd = sd) # create variable from normal distribution 
    means <- NULL  # create empty vector for means
    
    for (j in 1:times){
      dat <- sample(v, size = s, replace = FALSE)
      means[j] <-mean(dat)
    }
    
  # Create histogram of sample means
  hist(means, prob = T, breaks = 20, xlab = "Sample Means", col = "lightblue", xlim = c(45,55)) # standardizing the window of the graph with xlim()
  lines(density(means), col = "darkblue", lwd = 2)
  # add lines for mean of sample means and theoretical mean  
  m1 <- mean(v)
  m2 <- mean(means)
  abline(v = m1, col = "red", lty = 2, lwd = 2)
  abline(v = m2, col = "purple", lty = 2, lwd =2)
}

# create graph with 4 histograms
# have sample get larger
# add , xlim = c(45,55) to histogram function to show changes in distribution
# m, sd, times, N, s
par(mfrow=c(2,2))
graph_means(50, 10, 10000, 1000, 50) 
graph_means(50, 10, 10000, 1000, 150) 
graph_means(50, 10, 10000, 1000, 300) 
graph_means(50, 10, 10000, 1000, 600) 




