# Week 1: R Basics

########### Running code, assigning values to objects

# You can get output by simply typing into the console, or running (part of) a script file.
# You can run a line of code with the top right button, or Ctrl+Enter (see Tools -> Keyboard Shortcuts Help).
# Output is shown in the console (.R files, such as this), or in the editor (.Rmd files).
5+5

# Assign value (10) to object (x) with the arrow.  
# You can choose object names quite freely, but it's best to avoid e.g. function names (mean, sd).  
# Object names should start with a letter, and can contain letters, numbers, _ and .
# See Environment tab in RStudio!  
x <- 10 

# note: you could just as well use 10 -> x

# Print the object by just typing its name  
x

x + 5

# Create a new object based on x  
y <- 2*x


########### Vectors and functions

# The basic data type of R is a vector, created with c().  
# Let's create a vector named "age", with 5 elements:  
age <- c(13, 15, 12, 13, 16)


# Functions in R are of the form function_name(argument)  
#  Some basic functions:  
length(age)
mean(age)

summary(age)
str(age) # shows structure of the vector: type, length, first values

# Output from functions can also be saved to objects  
mean_age <- mean(age)

# An object can be printed with text using the paste() function
paste("Mean age:", mean_age)

# Concatenate vectors with c(): here, we combine one new element (x) with the age vector
new_age <- c(age, x)
length(new_age)

#### Accessing the elements of a vector
# You can access individual elements of a vector with indices  

age[1]
age[3]
age[1:3]
age[-5]
age[-length(age)]

# You can print elements that fulfill a condition
# which ages are above 13?
age > 13
cond <- (age > 13)
age[cond]

# Above, cond is a logical vector, i.e. it consists of TRUE/FALSE values only. 

# There are also character (categorical) vectors.
firstname <- c("John", "Mark", "Jane", "Alice", "Sophie")

# Print the structure of the vector
str(firstname)


########### From vectors to data frames

# Multiple vectors can be combined into a data frame. It will also show up in the Environment tab.  

df <- data.frame(age, firstname)
str(df)

# print your data frame
df

### Data wrangling: base R vs tidyverse
# Tidyverse is an alternative library to base R (the traditional style of R code), 
# aiming for more readable and understandable code.
# Even if base R solutions may sometimes be shorter, they are usually uglier and/or less clear. 
# Below are some examples with both styles. It's good to have an idea of the logic of base R; 
# however, this course will focus on using tidyverse functions for data wrangling.  

#### Base R
# Print variable of data frame
df$age

# Make new variable (here, centered age values)  
df$age_centered <- (df$age - mean(df$age))

# Select variables age and age_centered
df[c(1,3)]

df[c("age", "age_centered")] 

# Select variables that start with "age"
df[grepl("^age", names(df))] # this uses a thing called regular expressions

#### Same in tidyverse

library(tidyverse) # load tidyverse

# Make age_centered variable with the mutate() function  
df <- mutate(df, age_centered = age - mean(age))

# Select variables age and age_centered  
select(df, age, age_centered)

# Select variables that start with "age"
select(df, starts_with("age"))

##### Pipe operator
# Using the pipe operator %>% enhances readability of your code. 
# It essentially means that whatever comes before the pipe is used as the first argument of the function 
# that comes after the pipe.  
# For example, variables would be selected like this:  
df %>% select(age, age_centered) 

# Or an even simpler example:  
age %>% mean # instead of mean(age)

# The benefits of the pipe operator become more obvious when we need to carry out many operations in sequence.  
# The code below:  
# 1. takes the data frame
# 2. creates a new variable with mutate()  
# 3. filters rows of the data frame with filter()  
# 4. groups the data frame by gender with group_by()  
# 5. computes mean age by gender with summarise()  
# (it's common to use line breaks after each pipe)  

df %>% 
  mutate(gender = c('Male', 'Male', 'Female', 'Female', 'Female')) %>% 
  filter(age >= 13) %>% # filter ages greater than or equal to 13
  group_by(gender) %>% # group by gender
  summarise(age = mean(age)) # find mean age by gender

# It's quite easy to understand what is going on by just reading the code (but don't worry if it isn't just yet!), 
# because you can guess what a tidyverse function does based on its name, and the pipe operator
# helps you to perceive the order of the steps.


## What about figures?

# Base R also has its own plotting functions, which are handy for quick and simple visualizations, 
# but the code becomes messy if you want to make your plots look nice (which we will). 
# That is why we won't go deeper into base R plotting during this course, but will instead focus on the ggplot2 library 
# (part of tidyverse).  

# Here's an ugly base R plot that you will learn to make nicer with ggplot2:  
  
plot(df$age, df$age_centered)

