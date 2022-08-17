rm(list=ls())
setwd("C:/Users/cayde/OneDrive/Graduate Studies/Misc/TA/Lab 102/Data Sets")

############################
# Chi Square Goodness of Fit
############################

  # Goodness of fit test is used for determining whether the counts in a categorical variable come from a specific distribution

  # In 2016, are countries equally likely to be have free, partly free, or not free press?

#Data Structuring
#################
  
  library(foreign)
  library(dplyr)

  qog <- read.dta("qog.dta") # Load QOG data
  
  qog2016 <- qog[qog$year == 2016,] # Create subset of data that only includes 2016 observations
  
  
  # Look at distribution of variable 
  table(qog2016$fhp_status5)
  class(qog2016$fhp_status5)
  
  
#Data Visualization 
###################

  # Create Barplot
  prop <- prop.table(table(qog2016$fhp_status5))
  
  barplot(prop, col = rainbow(3),
          xlab = "Press Status",
          ylab = "Proportion of Countries",
          names.arg = c("Free","Partly Free","Not Free"))

#Data Analysis
###############
  
  # Run Chi-square goodness of fit with raw data
  mod <- chisq.test(table(qog2016$fhp_status5))
  mod
  
  # Given the p-value, we fail to reject the null hypothesis 
  
  # get observed values
  mod$observed
  
  # get expected values
  mod$expected
  
  # get residuals
  mod$residuals

#Displaying Results
####################
  
  # Create table of observed, expected and residuals
  
  # create vector of values
  vals <- c(mod$observed, mod$expected, mod$residuals)
  
  # create matrix
  t <- matrix(vals, ncol = 3, byrow = FALSE)
  colnames(t) <- c("Observed","Expected","Residuals")
  rownames(t) <- c("Free","Partly Free","Not Free")
  print(t)


##########################
# Chi Square Independence
##########################

  # Use this to explore whether there is a relationship between two categorical variables
  
#Research Question
##################

  # Is there a relationship between political system and political terror score?

  # Null Hypothesis: The political system of a country is not related to its political terror score
  # Alternative Hypothesis: The political system of a country is related to its political terror score
  # Critical Value: 0.05
  
#Data Structuring
##################

  qog2015 <- qog[qog$year == 2015, ] # Create subset of qog data for 2015
  
  table(qog2015$dpi_system) # Political System: 3 Groups
  table(qog2015$gd_ptsa) # Political Terror Score: 5 Groups 
  
  class(qog2015$dpi_system) # We need both variables to be factor variables
  class(qog2015$gd_ptsa)
  
  # Recode political terror variable so names not as long
  qog2015$gd_ptsa <- recode_factor(qog2015$gd_ptsa,
                                   'Countries under a secure rule of law' = "Rule of Law",
                                   'There is a limited amount of imprisonment for nonviolent political activity' = "Limited Imprisonment",
                                   'There is extensive political imprisonment, or a recent history of such' = "Extensive Imprisonment",
                                   'Civil and political rights violations have expanded to large numbers of the pop.' = "Violations",
                                   'Terror has expanded to the whole population' = "Terror")
  
  table(qog2015$gd_ptsa)
  
  
#Data Visualization 
###################
  
  # Create a table of the variables
  counts <- table(qog2015$gd_ptsa, qog2015$dpi_system)
  counts
  
  # Create a proportion table
  prop.table(counts)
  
  # Create mosaic plot
  mosaicplot(~ gd_ptsa + dpi_system, # ~ var1 + var2
             data = qog2015,
             color = c("maroon", "mediumorchid", "magenta4"),
             xlab = "Political Terror Score",
             ylab = "Political System",
             main = "",
             las = 1) # Makes all labels horizontal
  
  
#Data Analysis
##############
  
  model <- chisq.test(qog2015$gd_ptsa, qog2015$dpi_system) # Run Chi Square Independence Test (Ignore the warning)
  model
  
    #p is leass than .05, we can successfully reject the null hypothesis 
  
  # Get Observed Values, Expected Values, and Residuals
  model$observed
  model$expected
  model$residuals
  
  # get number of cases
  sum(model$observed)

  
  # Compare Effect size of political system on two groups:

  sub <- qog2015[qog2015$gd_ptsa == "Rule of Law" | qog2015$gd_ptsa == "Extensive Imprisonment", ] #subsetting our data to include only two outcomes: Rule of Law and Extensive Imprisonment
  
  table(sub$gd_ptsa) #We have unused levels
  sub$gd_ptsa <- droplevels(sub$gd_ptsa) # drop unused levels
  table(sub$gd_ptsa)
  
  
  sub <- sub[sub$dpi_system == "Presidential" | sub$dpi_system == "Parliamentary", ] # Only compare two political systems, Presidential and Parliamentary
  
  table(sub$dpi_system) #We have unused levels
  sub$dpi_system <- droplevels(sub$dpi_system) # drop unused levels
  table(sub$dpi_system)

  # Calculate the Odds Ratio
  
  #install.packages("epitools")
  library(epitools)
  
  # Syntax: oddsratio.wald(groupvar, outcomevar)
  oddsratio.wald(sub$dpi_system, sub$gd_ptsa)

    # Presidential is our baseline group and Parliamentary is our non-baseline group
    # Rule of Law is the baseline outcome and Extensive Imprisonment is the outcome of interest
    
    # The following section of the output gives us the odds ratio of 
    # the non-baseline having the outcome of interest
    
    #$measure
    #odds ratio with 95% C.I.
    #Predictor        estimate      lower     upper
    #Presidential  1.0000000         NA        NA
    #Parliamentary 0.1088435 0.03712413 0.3191164
    
    # Parliamentary systems are 0.11 times more likely to have extensive imprisonment compared to presidential systems
  
    # we really want the outcome of interest to be rule of law
    # add rev = "columns" to commmand to switch outcomes

  oddsratio.wald(sub$dpi_system, sub$gd_ptsa, rev = "columns")


    # From this output, we can see that parliamentary systems are 
    # 9.19 times more likely to have rule of law than presidential systems
    
    # If we wanted to switch the order of the groups, include rev = "rows"
  
  oddsratio.wald(sub$dpi_system, sub$gd_ptsa, rev = "rows")
  
    # If want to switch both the groups and outcomes, include rev = "both" in code

  oddsratio.wald(sub$dpi_system, sub$gd_ptsa, rev = "both")








