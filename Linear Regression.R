rm(list=ls())
setwd("C:/Users/gcm2/OneDrive/Graduate Studies/Misc/TA/Lab 102/Data Sets")



###################
#LINEAR REGRESSION
###################

#Data Creation 
  library(foreign)
  qog <- read.dta("qog.dta")
  dat <- qog[qog$year == 2015,]

#Data Inspection
  class(dat$wbgi_gee)
  class(dat$undp_hdi)

  # Check range of variables and no restriction of range assumption
  summary(dat$wbgi_gee)
  summary(dat$undp_hdi)
  
  
#Data Visualization 
  
  #Does there appear to be a general relationship between the two variables 
  plot(x = dat$wbgi_gee,
       y = dat$undp_hdi, 
       type = "p",
       xlab = "Government Effectiveness",
       ylab = "Human Development Index")
  
  
  # Plot with a loess line (a smooth line through the points, does not have to be linear)
  scatter.smooth(dat$wbgi_gee, # Independent Var
                 dat$undp_hdi, # Dependent Var
                 # lpars: line parameters, use list because including multiple parameters
                 lpars = list(col = "red", # color of line
                              lty = 1, # line type
                              lwd = 2 # line width
                 ),
                 xlab = "Government Effectiveness",
                 ylab = "Human Development Index"
  )

#Data Analysis 
  
  #Estimating the Model : lm(y ~ x, data = nameofdata)

  lm(undp_hdi ~ wbgi_gee, data = dat) #run the regression
  model <- lm(undp_hdi ~ wbgi_gee, data = dat) #store the results in a model 

  #Are there an outliers that might drive the results?
  
  summary(cooks.distance(model))   # Calculate Cook's d
  max(cooks.distance(model))   # get max
  
  par(mfrow = c(1,2)) # set graphing parameters for two plots side by side
  boxplot(dat$wbgi_gee, main = "Government Effectiveness")#plot your x variable
  boxplot(dat$undp_hdi, main = "Human Development Index") #plot your y variable
  par(mfrow = c(1,1))  # reset graphing parameters

  # Homoskedasticity Assumption
  
  # Check this assumption by calculating the correlation between the standardized
  # predicted and residual values from the linear regression model.
  # Get fitted values and residuals from regression output
  pred <- model$fitted.values
  pred
  resid <- model$residuals
  resid

  # standardize values using scale()
  z.pred <- scale(pred)
  z.resid <- scale(resid)

  # calculate the correlation coefficient between standardized predicted values and residuals
  cor.test(z.pred, z.resid, method = "pearson")

  # Because the correlation is not statistically significant, the assumption is not violated
  # IF the assumption were violated (p-value < 0.05), the assumption is violated and you should not interpret the regression results

  # Plot the standardized predicted values against the standardized residuals and include loess line
  scatter.smooth(z.pred, z.resid, lpars = list(col = "red", lwd = 3, lty = 1), xlab = "Predicted Values", ylab = "Residuals")

#Data Interpretation 
  
  summary(model)# Government effectiveness is a significant predictor of human development.
  

  
  # Create plot with data and include regression line 
  plot(x = dat$wbgi_gee,
       y = dat$undp_hdi, 
       type = "p",
       xlab = "Government Effectiveness",
       ylab = "Human Development Index")
  
  abline(lm(undp_hdi ~ wbgi_gee, data = dat), col = "blue", lwd = 2) #This plots the line of best fit
  abline(model, col = "green", lwd = 2) #alternatively you can just use the stored model 
  abline(a = model$coefficients[1], b = model$coefficients[2],  col = "red", lwd = 2)#you could also set 'a' and 'b' equal to the coefficients

  #You can manually create the regrssion line if you'd like 
  model$coefficients
  intercept <- model$coefficients[1]
  slope <- model$coefficients[2]
  abline(a = intercept, b = slope, col = "green", lwd =2)
  
  # Prediction/Predicted values
  # calc predicted value when x = 0.5
  x = 0.5
  p <- intercept + (slope * x)
  p
  
  x <- dat$wbgi_gee[!is.na(dat$wbgi_gee)&!is.na(dat$undp_hdi)]
  plot(x = x,
       y = pred, 
       type = "p",
       xlab = "Government Effectiveness",
       ylab = "Human Development Index")
  
  # predict values of new data
  new.ge <- data.frame(wbgi_gee = c(1,0.6,0.8,-0.3))
  predict(model, newdata = new.ge, type = "response")
  
  # Plot predicted values with confidence intervals
  

  new.wbgi_gee <- seq(min(dat$wbgi_gee, na.rm = TRUE), max(dat$wbgi_gee, na.rm = TRUE), length.out = 10) 
  #You can use seq() function to ensure you captured all the data
  
  preds <- predict(model, newdata = data.frame(wbgi_gee = new.wbgi_gee), interval = "confidence", level = .95) 

        #The first argument should be the name of the variable that you stored your model in 
        #The second argument should be your new independent variable that you just created (in a data.frame)
        #Also be sure that this dataframe has the same independent variable name as the one in your original model 
        #by setting the interval argument to "Confidence", this code will create a predicted variable, with upper and lower limits

  plot(new.wbgi_gee, preds[,1], type="l", ylab="Predicted Probability of HDI", xlab="Government Effectiveness")
  lines(new.wbgi_gee, preds[,2], lty=2, col = "blue")
  lines(new.wbgi_gee, preds[,3], lty=2, col = "blue")


####################
# Multiple Regression

# Instead of just having one independent variable, we will have two
# Regression with multiple independent variables: lm(y ~ x1 + x2, data = nameofdata)
# add foreign direct investment, net inflows as a percentage of GDP (wdi_fdiin) to regression
class(dat$wdi_fdiin)

mod <- lm(undp_hdi ~ wbgi_gee + wdi_fdiin, data = dat)

summary(mod)

# foreign direct investment is not a significant predictor of human development, while
# government effectiveness remains a significant predictor

####################
# Create a table of regression outputs
# install.packages("stargazer")
library(stargazer)

# create table of simple ression output
stargazer(model, type = "text")

# create table of multiple regression output
stargazer(mod, type = "text")

# Create table with both models
stargazer(model, mod, type = "text")

# Add labels to table
stargazer(model, mod, type = "text",
          title="Results",
          dep.var.labels = c("Human Dev. Index"), # label for DV
          covariate.labels = c("Gov. Effectiveness", "FDI inflows") # labels for IVs
)
