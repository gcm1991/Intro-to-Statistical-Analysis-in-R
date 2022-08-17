rm(list = ls())
setwd("C:/Users/cayde/OneDrive/Graduate Studies/Misc/TA/Lab 102/Data Sets")
# Explore Dataframes

#dataframes are a special kind of object in R


####################
#GETTING YOUR DATA
####################

  # 1. Load in a dataset from you computer

    anes <- read.csv("C:/Users/cayde/OneDrive/Graduate Studies/Misc/TA/Lab 102/Data Sets/anes_timeseries_2016.csv") #Specify the full file path & the file name with the proper extension
    anes <- read.csv("anes_timeseries_2016.csv") #Ideally, you can just specify the file name, if you have the folder set as your working directory 
  
    # read.csv("file")    # For .csv files
    # read.dta("file")    # For .dta files STATA v12 or lower (requries "foreign" package)
    # read.dta13("file")  # For .dta files saved in STATA v13 (or higher) (requires "readstata13" package)
    # read_excel("file")  # For .xls and .xlsx files (requires "readxl" package)

  # 2. Use a prebuilt data set from r
    
    data() #See all the prebuilt datasets 
  
    df <- iris
    df <- cars

  # 3. Create your own dataset
    
    #create data using vectors & combine those vectors into a dataframe using "data.frame()"
    students <- c("Ashley", "Mark", "John", "Sammy", "Linzee", "Rebecca")
    grades <- c(84,67,94,87,78,85)
    
    gradebook <- data.frame(students, grades)
    
    #or
    
    #create empty dataset, create vectors, then put vectors inside it 
    gradebook <- data.frame(matrix(ncol = 2, nrow = 6))
    
    students <- c("Ashley", "Mark", "John", "Sammy", "Linzee", "Rebecca")
    grades <- c(84,67,94,87,78,85)
    
    gradebook$X1 <- students
    gradebook$X2 <- grades


####################
#INSPECTING YOUR DATA
####################

  anes #Call dataframe df to display
  View(anes) #use 'view()' or click the data in the global environment to get a better look 
  anes$vote_for_president #Look at a specific variable using '$' (name_of_data$name_of_variable)

  colnames(anes) #get variable names
  names(anes) #another method to get variable names
  rownames(anes) #getting row names (usually not relevant)
  
  ncol(anes) #get number of columns
  nrow(anes) #get number of rows
  dim(anes) #get rows and columns
  
  head(anes)
  tail(anes)
  str(anes)

###############
#INDEXING DATA
###############
  
  #NOTE: A DATAFRAME CAN BE TREATED AS COMPILATION OF VECTORS
  #IF WE SPECIFY WHAT VECTOR(S) WE WANT, WE CAN USE ALL THE COMMANDS THAT WORK ON VECTORS FOR DATAFRAMES 
  
  
  
  #To select an element in a vector we use '[]' 
  vector_1 <- c(3,2,5,6,7)
  vector_1[5] #gets the fifth element of the vector
  
  
  #We can select an element (or observation) in a data frame as well
  #we still use '[]', but we have to put two values seperated by a comma, specifying the row and column
  
    anes[3,5] #this retrieves the observation in the third row and fifth column
    anes[,5] #leaving the row blank returns all rows and the specified column
    anes[3,] #leaving the column blank returns all columns and the specified row 
  
  #Just like we can select certain elements in a vector, we can select certain colunms and rows as well
  
    anes[3:6,] #selects rows three through six and all columns
    anes[,3:6] #selects columns three through six and all rows
    anes[3:6, 3:6] #selects columns three through six and columns three though six 
  
  #The logic for selecting rows/columns is the exact same logic as selecing elements in a vector
  #Indexing commands that work on a vector will work on a dataframe as well, just be sure to have a comman inside the brackets
    
    
  #Because dataframes have column names, we can use '$' to select a variable by colunm name
    
    anes$vote_in_2012
    
    
################
#SUBSETTING DATA
#################
    
  # 1. Using Indexing 
    #Simply select the data you want using '[]' and store it in an object
    
    vote_behavior <- anes[,6:11] #we are getting variables six through eleven and putting them in a new dataframe   
    more_data <- anes[,c(4,7,9)] #we are getting variables four, seven, and nine, and putting them in a new dataframe
    
    vote_behavior <- anes[,c("registered_to_vote", "party_of_registration", "vote_in_primary", "vote_choice_primary", "vote_for_president", "vote_choice_president")] 
    #we can also specify the variables using names rather than numbers
  
    first_100_respond <- anes[1:100,] #Here, we select the data by rows, getting the first 100, and keeping all the columns
  
   anes.1 <- anes[,-1] #use a negative to remove specified columns. Here we are getting rid of the first variable
   anes.2 <- anes[,-c(45:52)] # here we are getting rid of variables 45 through 52
    
  # 2. Using subset()
    #subset(name_of_data, select = vector of variables)
  
    feeling_therm <- subset(anes, select = c("feeling_dem_cand", "feeling_rep_cand")) #using column names
    feeling_therm <- subset(anes, select = c(15,16)) #using colunm numbers
    
    
    anes.1 <- subset(anes, drop = "case_ID")
    
  # 3. Using conditions
    
    democrats <- anes[anes$party_of_registration == "1. Democratic party",] #getting all rows in which the party of registration is "1. Democratic Party" and putting them in a new dataframe called "democrats"
    republicans <- anes[anes$party_of_registration == "2. Republican party",] #doing the same for Republicans
    
    partisans <- anes[anes$party_of_registration == "1. Democratic party" | anes$party_of_registration == "2. Republican party",] #Checking to see if the party is Democrat OR Republican using "|", and putting them in a new dataframe
    high_approval <- anes[anes$approve_of_congress == "1. Approve" & anes$approve_of_president == "1. Approve",] #check to see if the participant approved of Congress AND approved the President using "&"
  

  # 4. Removing NAs
    
    is.na(anes) #check the data for NAs
    is.na(anes$vote_choice_2012) #check a column for NAs
    anes.clean <- na.omit(anes)
    
  # 5. Remove unwanted data
    
    anes <- anes[!anes$party_of_registration == "3. None or 'independent'",] 
    

###############
#MODIFYING DATA
###############    
    
  #Renaming variables    
    
      #base R
       names(anes)[names(anes) == "registered_to_vote"] <- "registration"
        #or
       names(anes)[which(names(anes) == "registered_to_vote")] <- "registration"
       
       #Deplry
       anes <- rename(anes, registration = registered_to_vote) #data_name <- rename(data_name, new_name = old_name)
       

       
       
###############
#ANALYZING DATA
###############
    
    #Because dataframes are composed of vectors, functions that work on vectors should work on dataframes
    
    mean(anes$feeling_dem_cand, na.rm = TRUE)
    min(anes$feeling_dem_cand, na.rm = TRUE)
    max(anes$feeling_dem_cand, na.rm = TRUE)
