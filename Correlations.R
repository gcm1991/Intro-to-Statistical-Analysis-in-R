rm(list = ls())
setwd("D:/Graduate Studies/Misc/TA/Lab 102/Data Sets")

#############
# Correlation
##############

# Correlation allows us to explore relationships between two numeric variables

#Load in Data 

  library(foreign)
  qog <- read.dta("qog.dta")
  qog2000 <- qog[qog$year == 2000, ] # create subset of data that only includes 2000

#Establish Hypotheses
  # Null Hypothesis: There is a positive relationship between settler mortality and GDP per capita
  # Alternative Hypothesis: There is a negative relationship between settler mortality and GDP per capita
  # One-tailed test with alpha = 0.05
  
#Data Inspection

  class(qog2000$ajr_settmort) #verify that the variable is numeric
  class(qog2000$gle_rgdpc)

  summary(qog2000$ajr_settmort) # Look at summary statistics for each variables
  summary(qog2000$gle_rgdpc)

#Data Visualization 

  plot(x = qog2000$ajr_settmort,
       y = qog2000$gle_rgdpc,
       type = "p", # plot type (p = points)
       pch = 1, # point type (1 = open circle)
       xlab = "Settler mortality (log)",# label x axis
       ylab = "GDP per capita (2000)",
       ylim = c(0, 40000)) # adjust y axis
  #add line for the trend (line that best matches the data)
  abline(lm(qog2000$gle_rgdpc ~ qog2000$ajr_settmort), lwd = 2, col = "red")

#Data analysis   
# Calculate Pearson's r (correlation coefficient)
  #cor.test(x,y)
  
  cor.test(qog2000$ajr_settmort, qog2000$gle_rgdpc) #Run a basic correlation 
  
  #You can change the hypothesis with the alternative argument 
  cor.test(qog2000$ajr_settmort, qog2000$gle_rgdpc, alternative = "two.sided") #two.sided is defualt
  cor.test(qog2000$ajr_settmort, qog2000$gle_rgdpc, alternative = "greater")
  cor.test(qog2000$ajr_settmort, qog2000$gle_rgdpc, alternative = "less")
  
  #You can change the stastical method with the method argument
  cor.test(qog2000$ajr_settmort, qog2000$gle_rgdpc, method = "pearson") #Pearson is default, and should get you what you want
  cor.test(qog2000$ajr_settmort, qog2000$gle_rgdpc, method = "kendall")
  cor.test(qog2000$ajr_settmort, qog2000$gle_rgdpc, method = "spearman")
  
  #Final product
  model <- cor.test(qog2000$ajr_settmort, 
                    qog2000$gle_rgdpc, 
                    alternative = "less", 
                    method = "pearson")

  model

# Our correlation coefficient is -0.64, indicating a negative relationship
# The p-value is less than 0.05, so we can reject the null hypothesis of a positive relationship

#############
#Effect Sizes
#############

  # Calculate effect size (r^2)
  # square the coefficient estimate
  effect <- (model$estimate)^2
  effect

  # Approximately 40% of the variation in GDP per capita can be predicted by relationship with settler mortality rates

################
#Correllelogram 
###############
  
  # Visualizing correlations among many variables
  
  #Data Creation 
  
    #Let's get numeric variables that we are interested in the correlations between
    dat <- qog2000[, c("ajr_settmort", "gle_rgdpc", "gle_pop", "gle_imp", "gle_exp")] 
  
  #Data Visualization 
    
    #Correlation Matrix
    mat <- cor(dat, use = "complete.obs", method = "pearson") #set the use argument to complete.obs to handle missing data
    round(mat, 2) #round our data for better visualization 
    
    # each cell in the table is the correlation coefficient between the variable in the row and the variable in the column
    # notice that the diagonal from top left to bottom right is all 1s
    # variables are perfectly correlated with themselves
  
  # Draw a correlogram to visualize correlation matrix
    # install.packages("corrplot") 
    library(corrplot)
    
    corrplot(mat) #create a correllogram 
    corrplot(mat, type = "full") #change the type. 'full' is defualt. But this duplicates the data
    corrplot(mat, type = "upper") #upper
    corrplot(mat, type = "lower") #lower
    
  #Create a pretty graph my tinkering with different arguments 
      
  corrplot(mat, type = "upper",
           title = "Correlations", # title
           diag = FALSE, # TRUE/FALSE on whether to display diagonal
           order = "hclust", # ordering method of the correlation matrix
           tl.srt = 45, # degrees to rotate text labels
           tl.col = "black" # color of text for variable names
  )
  
  # add signifance to corrplot
    #install.packages("Hmisc")
    library(Hmisc)
    # run correlation using rcorr command
    res <- rcorr(as.matrix(dat)) 
    # extract p-values
    res$P
  
  # create correlation plot where insignificant are crossed
  corrplot(mat, type = "upper", diag = FALSE, order = "hclust", tl.srt = 45, tl.col = "black",
           p.mat = res$P, # the p-values from the rcorr output 
           sig.level = 0.05, # the desired significance level
           insig = "blank") # makes insignificant correlations blank
