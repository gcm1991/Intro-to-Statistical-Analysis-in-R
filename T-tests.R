# T-Test

rm(list = ls())
setwd("C:/Users/cayde/OneDrive/Graduate Studies/Misc/TA/Lab 102/Data Sets")

# Three Different Types of t-tests:

  # 1: One sample t-test                 | Compare the mean of a single group against a known mean
  # 2: Unpaired (Independent) t-test     | Compare the means of two separate groups
  # 3: Paired (Dependent) t-test         | Comparing the means of the same group from a different period in time

# Two Different ways of conducting a t-test:

  # 1: One-tailed (Directional. Asks if a value is greater than or less than)

      #H0: x = mu (null hypothesis)
      #H1: x > mu (alternative hypothesis)
          #or
      #H1: x < mu (alternative hypothesis)

  # 2: Two-tailed (Doesn't assume a direction. Asks if a value is simply different)

      # H0: x = mu (null hypothesis)
      # H1: x != (alternative hypothesis)



###################
#One sample t-test 
##################
  scores <- c(97,78,89,83,90,92,88,94,80,79) #Our single group 
  mu = 80

    # Two tailed t-test
      t.test(scores, mu = 80)
      t.test(scores, mu = 80, alternative = "two.sided") #two.sided is the default argument for alternative 
      
    # One tailed t-test
      t.test(scores, mu = 80, alternative = "greater") #use the alternative argument to change to one sided and specify direction 
      t.test(scores, mu = 80, alternative = "less")
      
    # calculate t statistic by hand
    # t = (x-mu)/(sd/sqrt(n))
    
      x <- mean(scores)
      s <- sd(scores)
      n <- length(scores)
      t <- (x-mu)/(s/sqrt(n))
      t
      
      #For two-sided tests you don't care whether t is positive or negative, as long as it's greater than 1.64 (90%), 1.96 (95%), 2.8 (99%)
      #For one-sided tests, you want t to be 'signed' in the correct dimension, but your threshold for significance is usually lower 1.64 (95%), 2.5 (99%)
        

###############################
#Unpaired (Independent) Samples
###############################
      
anes <- read.csv("anes_timeseries_2016.csv")
  
  # First method: t.test(outcomes_group_1, outcomes_group_2)     
          
    #Let's get a variable (Feelings for Clinton) from two separate groups (Republicans and Democrats) and store it  
    democrats_feelings_for_Clinton <- anes$feeling_bill_clinton[anes$party_of_registration == "1. Democratic party"] 
    republicans_feelings_for_Clinton <- anes$feeling_bill_clinton[anes$party_of_registration == "2. Republican party"]
    
       
    #Two tailed t-test 
    t.test(democrats_feelings_for_Clinton, republicans_feelings_for_Clinton)
    
    #One tailed t-test
    t.test(democrats_feelings_for_Clinton, republicans_feelings_for_Clinton, alternative = "greater") #this is checking to see if Democrats had warmer feelings for Clinton than Republicans 
    t.test(democrats_feelings_for_Clinton, republicans_feelings_for_Clinton, alternative = "less") #this hypothesis is silly, don't get too invested in interpretting the results
    
  #Second method: t.test(outcomes ~ group)  
    
    #Let's drop non-partisans from the data so we have exactly two groups
    anes.partisan <- anes[anes$party_of_registration == "1. Democratic party" | anes$party_of_registration == "2. Republican party",]
    table(anes.partisan$party_of_registration) #checking to see if we have exactly two groups for our grouping variable
    
    
    #Two tailed t-test
    t.test(anes.partisan$feeling_bill_clinton ~ anes.partisan$party_of_registration) #Put outcome variable first, followed by '~' and the grouping variable
    
    t.test(feeling_bill_clinton ~ party_of_registration, data = anes.partisan) #specify your data set if you don't want to use '$' for every variable
    t.test(feeling_bill_clinton ~ party_of_registration, anes.partisan) #Condense the code even more 
    
    #One tailed t-test
    
    t.test(feeling_bill_clinton ~ party_of_registration, anes.partisan, alternative = "greater")
    t.test(feeling_bill_clinton ~ party_of_registration, anes.partisan, alternative = "less")

######################
#Paired (Dependent) Sample t-test
#####################
  
library(foreign)
qog <- read.dta("qog.dta")

  #First method: t.test(outcomes_t1, outcomes_t2)

  hr1991 <- as.numeric(qog$gd_ptsa[qog$year == 1991])
  hr2017 <- as.numeric(qog$gd_ptsa[qog$year == 2017])

  t.test(hr1991, hr2017, paired = TRUE) #Set the paired argument to TRUE to do a paired t.test (paired defaults to false if not specified)

  #Second Method: t.test(outcome ~ group)
  qog.before.after <- qog[qog$year == 1991 | qog$year == 2017, c("cname", "year", "gd_ptsa")]
  qog.before.after <- na.omit(qog.before.after)
  
  t.test(as.numeric(gd_ptsa) ~ year, qog.before.after, paired = TRUE) 
  
  #Ignore this witchcraft. Had to ensure that there were no NA's in the data and that the number of observations at t1 and t2 were equal 
  countries <- NULL
  for(i in 1:nrow(qog.before.after)){
    country <- qog.before.after$cname[i]
    num <- sum(qog.before.after$cname == country)
    print(paste(c(country, num)))
    if(num == 1){countries <- c(countries, country)}
  }
  
  qog.before.after <- qog.before.after[!qog.before.after$cname %in% countries,]
  #########################################
  
  t.test(as.numeric(gd_ptsa) ~ year, qog.before.after, paired = TRUE) 
  #The code above is what I had to do to make this syntax work
  #Of course if the data was structured in the right way, I could have easily used the above syntax
  #If one method isn't working, try the other, before manipulating your data structure to make it work 

  
  

# Calculate effect size with Cohen's d
install.packages("powerAnalysis")
library(powerAnalysis)

ES.t.one(m = mean(scores, na.rm = T),
         sd = sd(scores, na.rm = T),
         mu = 80)
