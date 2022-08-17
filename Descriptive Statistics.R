rm(list = ls())

getwd()
anes <- read.csv("anes_timeseries_2016.csv")
anes

install.packages("stargazer") #Install the package first
library(stargazer) #load in the package with the library command


stargazer(anes, type = "text") #First argument is data. Set "type" to text for a basic table
stargazer(anes, type = "text", title = "Descriptive Statistics") #Add a custom title

#Define a character vector with variable names
#If the line of code is long, you can go to a new line as long as you end the line with a comma
covariates <- c("Case ID", "Feeling for Democratic Candidate", "Feeling for Republican Candidate", 
                "Feeling for Previous President", "Feeling for Bill Clinton", "Feeling for Democratic Party", 
                "Feeling for Republican Party", "Age")


#Here we used our covariates character vector supply the labels
stargazer(anes, type = "text", title = "Descriptive Statistics", covariate.labels = covariates) 



#We could also change the values that we report with the "summary.stat" argument
stargazer(anes, type = "text", title = "Descriptive Statistics", covariate.labels = covariates,
          summary.stat = c("max", "mean", "sd", "min")) 

#List of stats that can be used in the summary.stat
  # "max"	maximum
  # "mean"	mean
  # "median"	median
  # "min"	minimum
  # "n"	number of observations
  # "p25"	25th percentile
  # "p75"	75th percentile
  # "sd"	standard deviation


#Only show certain variables 
anes_feelings <- select(anes, contains("feeling"))
stargazer(anes_feelings, type = "text")

anes_feelings <- select(anes, c("age", contains("feeling")))
stargazer(anes_feelings, type = "text")

#Provide descriptive stats for a subset of the data 
anes_republican <- filter(anes, party_of_registration == "2. Republican party")
stargazer(anes_republican, type = "text")
