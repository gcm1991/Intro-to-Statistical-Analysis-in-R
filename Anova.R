rm(list=ls())
setwd("C:/Users/cayde/OneDrive/Graduate Studies/Misc/TA/Lab 102/Data Sets")

anes <- read.csv("anes_timeseries_2016.csv")

##############
#One-way ANOVA
##############

#Data Restructuring
##################

  # These group labels and the variable name is wordy, so we can recode
  # remember to install.packages("dplyr") if you have not installed the package before 

  library(dplyr) 
  
  anes$parents <- recode_factor(anes$native_status_parents,
                                '1. Both parents born in the U.S.' = "Both born in US",
                                '2. One parent born in the U.S.' = "One born in US",
                                '3. Both parents born in another country' = "None born in US")
  
  table(anes$parents) # Verify that the variable change worked

#Data Inspection
################
  
  #ANOVA's are good for data that has a categorical IV with more than two groups, and a continuous DV
    #(If we just had two groups, we could use a t-test instead)
  
  class(anes$parents) # Check that Group variable is a factor (categorical) variable 
  levels(anes$parents) #Check that the Group variable has more than two levels
  
  class(anes$feeling_rep_party) # Check that Dependent variable is a interger/numeric variable 


  # install.packages("psych")
  library(psych)
  describeBy(anes$feeling_rep_party, anes$parents)
  
  
#Data Visualization
###################
  
  # Create a boxplot of feeling_rep_party by group variable parents
  boxplot(feeling_rep_party ~ parents,
          data = anes,
          xlab = "Native Status of Parents",
          ylab = "Approval of Rep. Party",
          main = "Approval of Republican Party Across Parental Native Status",
          border = c("red", "blue", "green")) 

  # Plot the Means
    # Remember boxplots show the median but not the mean
    # This function plots the means with 95% confidence intervals

  # install.packages("gplots")
  library(gplots)
  
  # Syntax: plotmeans(y ~ x, data = nameofdataset)
  plotmeans(feeling_rep_party~parents, data = anes)

  # Final Product
  plotmeans(feeling_rep_party~parents, data = anes,
            main = "Mean Plot with 95% CI", # title
            xlab = "Native Status of Parents",
            ylab = "Approval of Rep. Party",
            connect = FALSE # FALSE means no line connecting means
  )

#Check Assumptions
##################
  
  #ANOVA tests requires that variances between the groups be equal
  #We have two different packages/commands we can use to test this

  # install.packages("car")
  library(car)
  
  # install.packages("rstatix")
  library(rstatix)

  # Conduct a Leven Test
  
  leveneTest(feeling_rep_party~parents, data = anes) # using car package
  levene_test(feeling_rep_party~parents, data = anes) # using rstatix package

  # The p-value we get is greater than 0.05, so the F statistic is not 
  # statistically significant, thus we can assume that the variances are equal

#Analysis
#########

  
  # Syntax: aov(y ~ x, data = nameofdataset)
  aov(feeling_rep_party~parents, data = anes) #estimate a model (make sure your IV is factor. You can use as.factor() to treat it as a factor)
  
  model1 <- aov(feeling_rep_party~parents, data = anes) #store the model in a variable
  summary(model1) #look at the results 

    # The p-value is less than the alpha of 0.05 so we reject the null hypothesis: the group means are not all equal

  TukeyHSD(model1) #TukeyHSD (Honestly Significant Differences) will tell us which groups are different from each other 
  
    # The output shows the difference in means between two groups being compared. 
    # Look at each line as if it were it's own t-test. Note the estimated difference and the statistical significance 
  
  # install.packages("lsr")
  library(lsr)
  etaSquared(x = model1) #EtaSquared gives us the effect sizes
  
    # only about 0.23% of variance in respondents' level of approval for Rep. Party
    # is associated with the native status of their parents

  
  
  
  
###################
#Two-way ANOVA test
###################

    #If we have two categorical IV's, we can do a two-way ANOVA
      # Dependent Variable: feeling_rep_party
      # Independent Variable 1: white (2 groups)
      # Independent Variable 2: religion_important (2 groups)

  
  # Define Null and Alternative Hypotheses
    # Null Hypothesis: 
    # Ho1: Mean approval equal between whites and non-whites
    # Ho2: Mean approval equal between those that say religion is important and those that do not
    # Ho3: There is no interaction between race and the importance of religion 
    # Alternative Hypothesis: 
    # HA1: Mean approval is different between whites and non-whites
    # HA2: Mean approval different between those that say religion is important and those that do not
    # HA3: There is an interaction between race and the importance of religion
    # Critical Value: 0.05

#Data Restructuring
##################  
  
  # Recode labels for ease of plotting
  library(dplyr) 
  
  anes$race <- recode_factor(anes$white,
                             '0. Not selected' = "Non-White",
                             '1. Selected' = "White")
  
  anes$religion <- recode_factor(anes$religion_important,
                                 '1. Important' = "Important",
                                 '2. Not important' = "Not Important")
  
  # Create subset of data that only includes the variables we need
  ANES <- anes[, c("race", "religion", "feeling_rep_party")]
  ANES <- na.omit(ANES)
  summary(ANES)
  
#Data Inspection
################  
  
  # Check the class of variables
  class(anes$feeling_rep_party)
  class(anes$white)
  class(anes$religion_important)

  
  # install.packages("Rmisc") 
  library(Rmisc)
  
  summarySE(ANES, # data
            measurevar = "feeling_rep_party", # Dependent Var
            groupvars = c("race", "religion"), # Independent Vars
            na.rm = TRUE)

  
#Data Visualization
###################

  #Create a boxplot
  
  boxplot(feeling_rep_party~ race*religion,   # Create boxplot
          data = ANES, 
          boxwex = 0.5, # scale factor applied to the boxes 
          horizontal = TRUE, # makes boxplot horizontal  
          las = 2, # changes direction of labels
          ylab = "",
          xlab = "Approval of Rep. Party",
          col = rainbow(4))
  
  #Graph doesn't fit, let's adjust the margins and rerun the graph 
  par(mar=c(5,10,1,1))   # par(mar = c(bottom_margin, left_margin, top_margin, right_margin))

# Create a two-way interaction plot

  par(mar=c(5.1, 4.1, 4.1, 2.1)) # Reset parameters
  
  interaction.plot(x.factor = ANES$race, # IV1
                   trace.factor = ANES$religion, #IV2,
                   response = ANES$feeling_rep_party, # DV
                   fun = mean, # function to compute, in this case the mean
                   type = "b", # type of plot (b = lines + points)
                   col = c("purple", "blue"), # color
                   pch = c(19,15), # symbols for the points
                   xlab = "Race",
                   ylab = "Mean Approval for Rep. Party",
                   trace.label = "Religion")

#Check Assumptions
###################
  
  # Now moving on to testing the homogeneity of variance assumption

  library(car)
  library(rstatix)
  
  # Syntax: (DV ~ IV1*IV2, data = nameofdata)
  leveneTest(feeling_rep_party ~ race * religion, data = ANES) # using car package
  levene_test(feeling_rep_party~ race*religion, data = ANES) # using rstatix package

    # Based on the p-value, the F statistic is statistically significant
    # so we can not assume the homogeneity of the variances
    # This is one issue with political science, often this assumption is violated
    # As a result, political science does not often use ANOVA
    # Even though this assumption was violated, we will move forward pretending it was not


# Analysis
##########
  
  
  model <- aov(feeling_rep_party~ race*religion, data = ANES)
  summary(model)

  # All of the p-values are less than 0.05, so we can reject all of the null hypotheses

  # Conduct follow up tests of calculating effect sizes and Tukey HSD

  library(lsr)
  etaSquared(model) #Effect Sizes


  TukeyHSD(model)  # Tukey HSD




