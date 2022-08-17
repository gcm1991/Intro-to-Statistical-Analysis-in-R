#Vectors & Classes 

rm(list = ls())

######################
###CREATE A VECTOR
####################3

  #Create a vector using c() function

    c(2,3,3,6,4,9,9,9,1) #manually enter in the values separated by a comma

  #Create vector using seq() or 'sequence' function

    seq(28) #Start a sequence from 1 going to the specified number 
    seq(from = 3, to = 12) #Create a sequence starting at the first number going to the second number
    seq(3,12) #Condense the code
    3:12 #Simplest way is to use a colon

    seq(from = 7, to = 51, by = 3) #create a sequence of numbers, starting at the first number, going to the second number, in intervals specified by the third number 
    seq(7, 51, 3) #condense the code

  #Create vector using rep() or 'repetition' function
    rep(56, 3) #repeat a number x number of times
    rep(c(1,3,4), times = 4) #repeat a vector x number of times
    rep(c(1,3,4), 4) #condense the code
    rep(1:4, times = 4) #repeating a sequence x number of times
    

    rep(c(3,9,4), each = 4) #repeat each element of a vector x number of times
    rep(7:9, each = 3) #repeat each element of a sequence x number of times
    #You cannot condense the code by removing 'each', because 'times' is the default value 

  #Storing the vector

    v1 <- c(2,3,3,6,4,9,9,9,1) # Assign vector to an object called v1
    v2 <- seq(from = 7, to = 51, by = 3) #It doesn't matter how you create the vector, they can all be assigned to objects
    v1 # Call object to display contents

    
#############################
#APPLY FUNCTIONS TO A VECTOR
##############################
    
    
  #For each function, put the name of the vector inside the parentheses

  length(v1) #returns the number of elements in a vector
  sum(v1) #returns the summation of the vector
  mean(v1) #returns the average of the vector
  min(v1) #returns the smallest number of the vector
  max(v1) #returns the largest number of the vector
  sd(v1) #returns the standard deviation of the vector
  var(v1) #returns the variance of the vector
  unique(v1) #returns all unique values of the vector (removing duplicates)
  range(v1) #returns smallest and largest values of the vector
  sort(v1) #returns the vector in sorted order
  rev(v1) #returns the vector but in reverse order
  table(v1) #returns a table that shows how many times each value is in a vector
  
  which(v1 == 9) #returns the position number of elements that meet condition 
  which(v1 < 6) #Condition can be anything, as long as it is TRUE/FALSE
  
  is.na(v1) #checks each element in a vector, and evaluates whether it is NA or not

############################
#SELECT ELEMENTS IN A VECTOR
#############################
  
  #by position

  v1[4] #returns the fourth element of a vector
  v1[2:4] #returns elements 2nd to 4th
  v1[c(2,4)] #returns 2nd and 4th elements
  
  v1[-4] #returns every element of the a vector except for the fourth
  v1[-c(2:4)] #returns all elements except 2nd to 4th
  v1[-c(2,4)] #returns all elements except 2nd and 4th elements
  
  #by value
  
  v1[v1==9] #returns all elements equal to a specified value
  v1[v1 != 9] #returns all elements NOT equal to a specified value
  v1[v1 < 4] #returns all elements that are less than a specified value
  v1[v1 > 4] #returns all elements that are greater than a specified value
  v1[v1 <= 4] #returns all elements that are less than or equal to a specified value
  v1[v1 >= 4] #returns all elements that are greater than or equal to a specified value
  
  v1[v1 %in% c(1,4,9)] #returns all elements that are in a specified set (in this case 1, 4, and9)
  


#objects can have different classes. The main classes are numeric, factor, string, and logical

class(v1)

# Create variables of different classes
# Factor
v2 <- factor(c(1,2,3,2,1))
v2
class(v2)

# Character/string
v3 <- c("USA", "France", "China", "Russia", "UK")
class(v3)

# Logical
v4 <- c(TRUE,FALSE,TRUE,FALSE,FALSE)
class(v4)

# Create vector with missing values
v5 <- c(3,5,NA,8,NA) 
class(v5)

# Determine how many missing values are in the vector
# is.na returns a logical vector with TRUE indicating missing values
is.na(v5)

# Sum the number of TRUE in the vector returned by is.na()
sum(is.na(v5))

#############
