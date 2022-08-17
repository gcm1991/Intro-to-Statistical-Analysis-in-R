rm(list = ls())

setwd("C:/Users/cayde/OneDrive/Graduate Studies/Misc/TA/Lab 102/Data Sets")
library(foreign)

anes <- read.csv("anes_timeseries_2016.csv")
energy <- read.csv("PESenergy.csv")
lobby <- read.csv("constructionData.csv")
qog <- read.dta("qog.dta")

###########
#Histogram
############

#Drawing the Histogram

  hist(qog$wdi_lifexp) #Draw a histogram of a certain variable

  hist(qog$wdi_lifexp, freq = TRUE) #There is a frequency argument which determines whether you create a frequency or density graph (defaults to frequency)
  hist(qog$wdi_lifexp, freq = FALSE)#Putting this argument to false switches to a density graph which shows percentage rather than absolute number

#Changing the Number of bins

  hist(qog$wdi_lifexp, breaks = 40) # You can use the break argument to specify the number of bins. But sometimes this doesn't give you the exact value you specify 
  hist(qog$wdi_lifexp, breaks = c(10,20,30,40,50,60,70,80,90)) #You can put a vector specifying the breakpoints giving you more control, of where you draw your bins
  hist(qog$wdi_lifexp, breaks = seq(16,90,2)) #Use the sequence function to create the vector specifying the location of the bins

  hist(qog$wdi_lifexp, breaks = seq(min(qog$wdi_lifexp, na.rm = TRUE),max(qog$wdi_lifexp, na.rm = TRUE),length.out = 21)) #An elegant method for getting the desired number of bins
    #Use the seq() function
    #Put n+1 for the 'length.out' argument where n is the desired number of bins
    #Use min() and max() functions to ensure you capture all the data 


#Adding Labels
  
  hist(qog$wdi_lifexp) #Graph looks ugly with no labels
  
  hist(qog$wdi_lifexp, xlab = "Years") #Add a variable name
  hist(qog$wdi_lifexp, xlab = "Years", main = "Life Expectancy") #Add a title
  hist(qog$wdi_lifexp, xlab = "Years", main = "", ylab = "") #You can also omit a label with ""
  
#Aesthetics
  
  hist(qog$wdi_lifexp, axes = FALSE) #Remove the axes by setting the axes argument to FALSE
  hist(qog$wdi_lifexp, col = "turquoise")#add color to your bars using the col argument
  hist(qog$wdi_lifexp) + box() #Put a box around your graph by adding the box() function
  
#Adding Lines
  
  m <- mean(qog$wdi_lifexp, na.rm = TRUE) #calculate a value for the mean and store it in m
  
  #You can add lines using the abline() function
  hist(qog$wdi_lifexp) + abline(v = m) #draw a vertical line where v is equal to the mean
  hist(qog$wdi_lifexp) + abline(h = 1500) #draw a horizontal line at 1500
  hist(qog$wdi_lifexp) + abline(a= 500, b=10) #draw a line where the intercept is 500, and the slope is 10 
  
  #You can add color to the line with the color argument: col()
  hist(qog$wdi_lifexp) + abline(v=m, col="purple")
  hist(qog$wdi_lifexp) + abline(v=m, col="red")

  #You can change the type of the line with line type argument: lty()
  hist(qog$wdi_lifexp) + abline(v=m, lty = 1) 
  hist(qog$wdi_lifexp) + abline(v=m, lty = 2)
  hist(qog$wdi_lifexp) + abline(v=m, lty = 3)
  
  #You can change the width of the line with the line width argument: lwd()
  hist(qog$wdi_lifexp) + abline(v=m, lwd = 1) 
  hist(qog$wdi_lifexp) + abline(v=m, lwd = 2) 
  hist(qog$wdi_lifexp) + abline(v=m, lwd = 3) 
  hist(qog$wdi_lifexp) + abline(v=m, lwd = 4)
  
  #Pick something that looks nice to you
  hist(qog$wdi_lifexp) + abline(v=m, col = "purple", lty = 2, lwd = 3)
  
#Adding a Legend
 
  #choose where to draw it
  hist(qog$wdi_lifexp) + legend("topleft","Mean")
  hist(qog$wdi_lifexp) + legend("topright","Mean")
  
  hist(qog$wdi_lifexp) + legend(x = 15, y = 1800,"Mean") #Use coordinates for more precision 
  hist(qog$wdi_lifexp) + legend(x = 15, y = 2000,"Mean")
  
  #Adjust the Size
  hist(qog$wdi_lifexp) + legend("topleft","Mean", cex = 1.00) #Use cex argument to specify magnification
  hist(qog$wdi_lifexp) + legend("topleft","Mean", cex = .75) #Make it smaller
  hist(qog$wdi_lifexp) + legend("topleft","Mean", cex = 1.25) #Make it bigger
  
  #Match the legend to your graph
  hist(qog$wdi_lifexp) + legend("topleft","Mean", col = "purple", lty = 2, lwd = 3) #Use the design scheme from your lines 
  
  #Add the legend and the corresponding line
  hist(qog$wdi_lifexp)
    legend("topleft","Mean", col = "purple", lty = 2, lwd = 3, cex = 1.25)
    abline(v=m, col = "purple", lty = 2, lwd = 3)
        #To add the legend and the line put them on different lines of code. Doesn't work if you just conjoin them with the '+'
  
  #Multiple Lines
    
    #Just use the c() function to specify a vector of values. 
    
    #changing the color
    hist(qog$wdi_lifexp)
    legend("topleft",c("Mean", "Random Line"), col = c("purple", "blue"), lty = 2, lwd = 3, cex = .75)
    abline(v=m, col = "purple", lty = 2, lwd = 3) #drawing the mean
    abline(v = 40, col = "blue", lty = 2, lwd = 3) #drawing a random line
    
    #changing the line type
    hist(qog$wdi_lifexp)
    legend("topleft",c("Mean", "Random Line"), col = "purple", lty = c(2,1), lwd = 3, cex = .75)
    abline(v=m, col = "purple", lty = 2, lwd = 3) #drawing the mean
    abline(v = 40, col = "purple", lty = 1, lwd = 3) #drawing a random line
    
#Final plot
  
  hist(qog$wdi_lifexp, 
       breaks = 40,
       main = "Life Expectancy",
       xlab = "Years",
       col = "turquoise")
    legend("topleft","Mean", col = "purple", lty = 2, lwd = 3, cex = 1.25)
    abline(v=m, col = "purple", lty = 2, lwd = 3)
    box()
  

######
#Plot
######

  #Scatterplot
  
   plot(lobby$partratetotalhealth,lobby$healthagenda97) #create a scatter ploty mapping variable x against variable y
    
    #Labeling
   plot(lobby$partratetotalhealth,lobby$healthagenda97, xlab = "Lobby Participation Rate", ylab = "Number of Health Bills") # Add labels to the axis
   plot(lobby$partratetotalhealth,lobby$healthagenda97, xlab = "Lobby Participation Rate", ylab = "Number of Health Bills", main = "Lobbying in the Health Industry") # Add title to the graph

  
   bestfit <- lm(lobby$healthagenda97 ~ lobby$partratetotalhealth) #Estimating a linear model to draw a line on our graph
  
    plot(lobby$partratetotalhealth,lobby$healthagenda97)# Add a line to the graph using abline()
    abline(bestfit)
    
      #Final Scatterplot
    
        plot(lobby$partratetotalhealth,lobby$healthagenda97,  
             main = "Lobbying in the Health Industry",
             xlab = "Lobby Participation Rate",
             ylab = "Number of Health Bills", 
             ) 
        abline(bestfit)
      
      #Scatterplot with Jitter
        
        plot(anes$feeling_dem_party, anes$feeling_rep_party) #We can plot the feelings of democrats vs. Republicans
                                                              #But because participants like to give round numbers, many of the observations overlap on the graph
        plot(jitter(anes$feeling_dem_party,10), jitter(anes$feeling_rep_party,10)) #Here we use the jitter() function to add noise to the data
        plot(jitter(anes$feeling_dem_party,25), jitter(anes$feeling_rep_party,25)) #By adjusting the Jitter, we can start see a pattern

  
  #Line Graph
  
    #Lets use a timeseries dataset to draw a linegraph 
  
    plot(seq(1:180), y = energy$Energy, type = "l") #change it to a line graph using the 'type' argument
                                                    #Useful for time series
                                                   #type defaults to point or "p" by default
                                                   #Data is already ordered by month, so lets just use a sequence for or x value
  
    plot(seq(1:180), y = energy$Energy, type = "l", axes = FALSE) #Let's remove the default axes, because the x axis represents time
  
  
    #Let's redo the graph with better axes
    plot(seq(1:180), y = energy$Energy, #draw the graph
         type = "l", #make it a line graph
         main = "Media Coverage",
         xlab = "Year", #Remove the x label
         ylab = "Number of News Stories",
         axes = FALSE #Remove the axes
    )
      axis(1, at = c(1,37,73,109,145), labels = c("Jan. 1969", "Jan. 1972", "Jan. 1975", "Jan. 1978", "Jan 1981")) #Here we create axis 1, with custom labels and tick marks
      axis(2) #This is the y axis. We don't want to change anything, so there are no values, but we still need to redraw it after we removed both the the axes
      abline(h = 0, col = "gray") #Just drawing a line for the x axis
         
         #If not all the labels are showing up you may need to adjust the size of your plot window

###########
#Bar Graph 
###########
    
#You can compare the different values of a variable using a bar graph
#Easiest method is to use the table function 
    
barplot(table(anes$party_of_registration)) #Look at the different party IDS
barplot(table(anes$vote_choice_2012)) #Look at the different vote choices
barplot(table(anes$vote_in_2012)) #Look at how many voted vs. how many abstained

barplot(prop.table(table(anes$party_of_registration))) #use prop.table to get the proportions or percentages rather than absolute numbers 


  #labels

barplot(table(anes$vote_choice_2012), xlab = "Candidates", ylab = "Number of votes", main = "Vote choice in 2012") #all of the labeling options work for boxplots

  #color

barplot(table(anes$vote_choice_2012), col = "red") #Color all our boxes red 
barplot(table(anes$vote_choice_2012), col = c("blue", "red", "gray")) #Specify a color for each box

  #final plot

barplot(table(anes$vote_choice_2012),
        main = "Vote choice in 2012",
        xlab = "Candidates",
        ylab = "Number of votes",
        col = c("blue", "red", "gray"),
        )
    abline(h = 0) #just drawing a line for the x axis


#################
#Box Whisker Plot
##################
   
#Use Boxplots to identify outliers in your data
#Works on single variables with continuous values
   
  #Drawing the plot  

   boxplot(qog$wdi_lifexp) #draw the plot
   boxplot(qog$wdi_lifexp, horizontal = TRUE) #draw it horizontally with the horizontal argument
   
   #Labeling
   
   boxplot(qog$wdi_lifexp, main = "Life Expectancy", ylab = "Years") #Use the usual arguments for labeling
   boxplot(qog$wdi_lifexp, horizontal = TRUE, main = "Life Expectancy", xlab = "Years") #Make sure to use xlab if you do a horizontal graph
   
   #By groups
   
   boxplot(data = qog, wdi_lifexp~bmr_dem) #plots the life expectancy (wdi_lifexp) grouping by democracy type (bmr_dem)\
   boxplot(data = anes, feeling_bill_clinton~party_of_registration) #plots feelings for Bill based on Party 
   
   

   
   
