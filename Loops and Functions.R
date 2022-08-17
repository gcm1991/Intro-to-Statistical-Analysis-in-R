rm(list = ls())
setwd("C:/Users/cayde/OneDrive/Graduate Studies/Misc/TA/Lab 102/Data Sets")
anes <- read.csv("anes_timeseries_2016.csv")

#Functions and Loops



###########
#Functions
##########

#R has built in functions, but you can create custom functions to automate your code

#SYNTAX
########

# function_name <- function(argument_name){
#     *do something with arguments
#       return(*the result of your operations)
#   }

#Examples
#########

#Basic Function

  add_three <- function(x){
    y <- x + 3
    return(y)
  }

  add_three(4) #let's call on that function
  
  
  add_three <- function(x){ #you don't have to use the return. But you won't know the result of your function
    y <- x + 3
  }
  
  add_three(4) #Result doesn't show up


#More functions

  subtract_10 <- function(b){ #Argument name doesn't have to be 'x', name it whatever you want
    a <- b - 10
    return(a)
  }
  
  subtract_10(16)
  
  
  double <- function(x){ #because these are arguments for your functions, they do not show up as values in your global environment
    y <- x + x            #Feel free to reuse the names of your arguments without messing up other functions or data
    return(y)
  }
  
  double(8)
  
  
  another_function <- function(a_number){
    result <- a_number*8
    return(result)
  }
  
  another_function(5)


#Function with multiple arguments

  adding_numbers <- function(num_1, num_2){
    summation <- num_1 + num_2
    return(summation)
  }
  
  adding_numbers(5,3)


#You can use prebuilt functions inside your custom function

  get_average <- function(x){
    sum(x)/length(x)
  }
  
  get_average(c(4,7,5,2))
  
  
  get_sum <- function(num_1, num_2){
    sum <- num_1 + num_2
    result <- paste("The sum of", num_1, "and", num_2, "is", sum) #paste combines different values into one string 
    return(result)
  }
  
  get_sum(5,9)

#######
#Loops
########

#You can use a loop if you want to loop through a sequence

#Syntax
#######

# for(name_of_index in start_of_sequence:end_of_sequence){
  #operation that uses number in sequence
#}

#Examples
#########

  for(i in 1:10){ #we are going to go through numbers one through ten and print each one
    print(i)
  }

  for(i in 1:10){ #we are going to go through numbers one through ten and multiply each number by two and print it
    print(2*i)
  }

###############

#Lets get column names of anes, 1 - 5

#here's how we would do it manually, very code intensive and laborous 
  colnames(anes)[1]
  colnames(anes)[2]
  colnames(anes)[3]
  colnames(anes)[4]
  colnames(anes)[5]

#Here's how we would do it using a loop
  for(i in 1:5){ 
    print(colnames(anes)[i])
  }

#Using this methods we can easily get all the names
  for(i in 1:50){ 
    print(colnames(anes)[i])
  }

#we can use other functions to get our sequence, we don't have to manually put it in
  for(i in 1:length(anes)){ 
    print(colnames(anes[i]))
  }

################

#We can create new variables with loops
  
  squared_numbers <- NULL #placeholder vector
  
  for(i in 1:5){
    squared_numbers <- c(squared_numbers, i*i) #defining our variable as itself, plus a new value
  }

  
#We can nest loops inside each other
  
  #This code will go through each column of anes, and at each column go through all the rows
  for(column in 1:ncol(anes)){
    for(row in 1: nrow(anes)){
      print(anes[row,column])
    }
  }
  
  #as we can see nesting loops can be computationally expensive, make sure you have a good reason to do it
  #If the code is taking too long, you can press the red stop button in the top right console
  
  #I used the 'column' and 'row' as the indices because they are intuitive, I could use more generic labels as well
  
  for(i in 1:ncol(anes)){
    for(j in 1: nrow(anes)){
      print(anes[j,i])
    }
  }
  
  #If you have multiple loops, be careful about your indices. For example don't use 'i' for all your loops
