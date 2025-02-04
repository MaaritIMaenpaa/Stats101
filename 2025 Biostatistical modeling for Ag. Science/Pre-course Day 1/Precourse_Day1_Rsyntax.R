# PhD Course, Biostatistical modeling in Ag. Science, Pre-course, Day 1
# R syntax ---- 
# Aim: My first R script.

#
#
#

# Read through this script, and run the code given.
# You will be presented with Tasks along the way - try to work them out based on 
# what you've learned. 

#
#
#

####
#
# HOUSEKEEPING ----
#
####

# Annotations ----

# To annotate scripts in R, use the #-symbol. Anything written on the right of the symbol won't 
# be read by R. 

If you write without this, you will confuse R: when running the script, it will create errors. 

Sometimes # you may want to annotate on a line of code. This means that things on the left of the # will be read by R, but not what comes after. 

#
#
# TASK 1: ----

# 1.1 Set your cursor to line 1 of this script. Press Ctrl-Enter, or click on "Run" on the top 
#     right of the script. What happens? 
# 1.2 Put your cursor on line 30. Run the code. What happens?
#
# 1.3 Silence (=make sure R won't read) the sentences above. How do you do that?

#
#
#

# Set working directory ----

# When you start a script, it is good practice to define the folder that you are using for all 
# relevant files (scripts, data, outputs, etc.). To tell R where you keep all of this, set a 
# working directory at the beginning of the script.

# Check what working directory you are now using.
getwd()

# Change the path below (in the quotation marks) into the correct path to the folder you are 
# using in your own computer.
setwd("C:/Users/au721810/Git Repositories/Stats101/2025 Biostatistical modeling for Ag. Science/Pre-course Day 1")

# If you struggle to find the path to the folder you wish to use, you can do so by 
# point-and-click method. Follow the steps below.
#
# STEP 1: Go to "Sessions" menu in R studio 
#         Choose "Set working directory"
#         Choose "Choose directory" 
#         Select the right folder in your computer. Click on "Open".
# STEP 2: Look at the console on the bottom left corner of R studio. See the piece of code 
#         that appeared? Copy this code into this script to replace the code above, so if you 
#         wish to rerun your script, you do not need to point-and-click to get to your working 
#         directory every time. 

# Check whether the working directory changed by rerunning this:
getwd()


#
#
#

####
#
# THE CALCULATOR ----
#
####

# You can use R as a calculator. Run the code below. 
1+2
200^4
(200-13*5)/78 + 876^3*1000

#
#
# TASK 2: ----
# Calculate the following using R: 

#
# 1.1. Run this line of code:
Sys.time()

# You just took a timestamp of your computer's idea of what date and 
# time it was when you run the code. 
# Based on that, how many minutes of 2025 has passed up until the time 
# you just extracted?
# Use simple maths, but let R be your calculator to get the answer!


#
# 1.2. Askov long-term experiment has been running since 1894. Mangolds 
# were used as a row crop in different rotations until 1947, after which
# it was replaced by beets, swedes and eventually silage corn. 
# Last data collected from the experiment was from the growing season of
# 2024. What percentage of years of data contain information about
# mangolds?



#
#
#


# After calculating something, you may also wish to save the results
results <- 1+2 

# The arrow (<-) above is read as "gets", and the "results" is a new object we create.
# You can change the name of the new object to anything you wish it to be. 

# What is an "object" ? 
# It can take many forms: single numbers, lists, vectors, matrices, dataframes, etc. 
# Essentially, it is something saved in R that you can refer back to.

# Check the environment in the top right corner of Rstudio for the objects that R currently 
# can see in your workspace. Do you find "results" in there?

# To print out the results from the object you create, simply write the name, and run the 
# line of code:
results

#
# Naming objects ----

# A few rules for naming things: 
#   1. Don't start with a number
#   2. Don't leave spaces in the name
#   3. Don't use anything that could be mistaken for a mathematical operator

# To demonstrate, run these lines:
1wontwork <- 1+2 
this wont either <- 1+2 
this-neither <- 1+2 

# Conventions vary for how to name things. Below you see acceptable names.

# Run these lines:
some_people_like_this <- 1/3
others.prefer.points <- 2*4
OrMaybeYouCanDoThis <- 3+(13*200)
ORjustforget.convention_andMIXthingsUP <- (4+19)/2

# Sometimes you may create nonsense and don't want it to clutter your 
# workspace (see top right corner of R studio). 
# Let's remove the things we just created.

rm(some_people_like_this, others.prefer.points, OrMaybeYouCanDoThis, ORjustforget.convention_andMIXthingsUP)
# Note: rm = "remove"

#
#
# TASK 3: ----
# Actually, we don't need the "results" object either. Remove this too, using the example code 
# from above.


#
#
#

# Vectors ----

# In R, a vector is a basic data structure that holds elements of the 
# same type. Vectors can contain numeric, character, or logical data.

# Vectors are created using the c() function, which stands for 
# "combine" or "concatenate."

# Numeric vector
lunch_review <- c(3, 2.5, 3, 2, 5)

# Character vector
day_of_week <- c("Mon", "Tue", "Wed", "Thu", "Fri")

# Logical vector
working_from_home <- c(FALSE, FALSE, FALSE, FALSE, TRUE)

#
# You can perform operations on vectors 

# Example:
# Transform vector lunch_review from its current scale (0-5) to percentage of the 
# maximum score
lunch_review_perc <- (lunch_review/5)*100
lunch_review_perc

# The operation on a vector can also depend on another vector;

# Create another numeric vector
coffee_review <- c(1,1,1,1,4)

# Calculation
average_goodness_of_day <- (lunch_review+coffee_review)/2
average_goodness_of_day
# What are the values in the new vector?

# Let's print these with names based on another vector (day_of_week)
setNames(average_goodness_of_day, day_of_week)

# This is what is called a named vector. Each element has a corresponding 
# name. It is not always - or even often - necessary, but it can make your data easier to
# follow and use. 


#
#
# TASK 4: ----

# 4.1. What are the last three countries you have visited? Make a vector 
# with their names


# 4.2. Make a vector of the share of land area used for agriculture in 
# 2022 in the countries you listed in the previous step, using the link below. 
# https://ourworldindata.org/grapher/share-of-land-area-used-for-agriculture?tab=table


# 4.3. Make a vector of the total land area of the countries you listed 
# above using the link below.
# https://en.wikipedia.org/wiki/List_of_countries_and_dependencies_by_area


# 4.4. Calculate the absolute area of land used for agriculture based on
# the vectors you created. 

#
#
#

# 
# Matrix ----

# Floating vectors that rely on things being identified correctly based 
# on order are not the safest way of keeping information. In stead, one 
# step safer is to connect the observations coming from the same unit appropriately,
# by putting them in a "table" - in this case, a matrix. 

# A matrix is a a symmetrical array of numbers, arranged in rows and columns.

# Example: Remember these three vectors?
lunch_review
coffee_review
day_of_week

# Let's make a matrix out of them. We can do this in two ways:
cbind(lunch_review, coffee_review) # cbind = "column bind"
rbind(lunch_review, coffee_review) # rbind = "row bind"

#
# Side note: R can do maths on your matrices

# Create two matrices with the function called matrix, and tell it how many rows and columns
# the matrix will have
matrix1 <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2, ncol = 3)
matrix2 <- matrix(c(6, 5, 4, 3, 2, 1), nrow = 2, ncol = 3)

# Let's sum up the two matrices:
matrix1 + matrix2

# Or substract
matrix1 - matrix2

# You can do matrix algebra in more complex ways as well, but we won't go there here
# If you're interested, check this as one example: https://css18.github.io/linear-algebra.html
#

# Back to review of our week - we created a matrix of the vectors before, but we did not save it.
# If you wish to save a matrix as an object, same rules apply as the general object creation we've
# dealt with previously.
week_matrix <- cbind(lunch_review, coffee_review)
week_matrix

# We can do many things for a matrix, but for this case, to improve its presentation,
# we can give names to the rows based on day_of_week:
rownames(week_matrix) <- day_of_week
week_matrix

# Note in the above, on the left side of the "gets" arrow (<-), we refer to an element of the
# object week_matrix. On it's own it returns the list of rownames from the object.
rownames(week_matrix)
# The code above replaces the rownames with the vector of day_of_week
# Let's give new column names as well, using the same principle:

# First, look at the current names:
colnames(week_matrix)

# Then give them a replacement:
colnames(week_matrix) <- c("Lunch", "Coffee")

# And then let's check if things changed:
week_matrix

#
# Downsides of matrices: Can only take one type of data. See what changes
# if we try to make a matrix including the day of the week as a column:

week_matrix <- cbind(day_of_week, lunch_review, coffee_review)
week_matrix

# The "-symbols indicate everything is read as a character, not a number. R cannot understand "Mon"
# as anything but a string of characters, so it will convert everything else to match this type of data.


#
#
#

#
# Dataframe ----

# When you have mixed types of data, in stead of a matrix, you may wish
# to store your data in a dataframe:

# You can convert a matrix to a dataframe. 
# Let's create one from the previously created matrix:
week_data <- data.frame(week_matrix)
week_data

# Or you could create this from scratch, and control the column names as you please:
week_data <- data.frame(Weekday = day_of_week,
                        Lunch = lunch_review,
                        Coffee =coffee_review)
week_data

# Note that you can change the elements within the code, as long as they have the right
# number of observations:
week_data <- data.frame(Weekday = c("Mon", "Mon", "Mon", "Mon", "Mon"),
                        Lunch = lunch_review,
                        Coffee =coffee_review)
week_data

# For something like the Weekday column we created, R actually has a handy function:
# Try to figure out what it does.
rep("Mon", 5)

# What about this one:
seq(1,7,0.4)
# What does it do? Play with the three numbers in the code to see how things change.


# 
# TASK 5: ----
# Create a matrix and a dataframe from your vectors of information from Task 4.



#
#
#

####
#
# DATA MANAGER ----
#
####

# To inspect dataframes, you can do various things:

# See the names of your columns:
colnames(week_data)
names(week_data) # for the lazier option

# Print the first n-number of rows of the data:
head(week_data, n = 2)

# Or the last:
tail(week_data, n=2)

# Summarise all data:
summary(week_data)

# What if you want to see just one column of your data?
week_data$Lunch

# See the structure of the data:
str(week_data)
# Note, you can see the mode/class of each column here 

# You can also change the mode/class of your column. For example, we 
# see that Weekday is read as "chr" (character). We would like it to be 
# a factor (a categorical variable with specific levels)

# Let's change it:
week_data$Weekday <- factor(week_data$Weekday)
str(week_data)

# For the sake of an example, let's miss-classify something:
week_data$Lunch <- factor(week_data$Lunch)
str(week_data)

# How do we fix this? By changing it back to character, and then to 
# numerical data.
# Note: When converting a factor to a numeric variable, you always have to go through a character, 
# otherwise things will go wrong
week_data$Lunch <- as.numeric(as.character(week_data$Lunch))
str(week_data)

# Exploratory illustrations ----
# You can also summarise a variable in your data in different ways 
hist(week_data$Lunch)
# This is a histogram. What does it show you?

#
#
#

# 
# Cleaning ----

# We've created many objects in this script. Let's see a list of all of them:
ls()

# Now, we can use that to help us remove all the objects from the 
# working environment. Remember, we can remove objects with rm-function.
# For example, we can remove lunch_review2
rm(lunch_review_perc)

# Now, let's combine rm() with the ls() to remove all objects we have created
rm(list=ls())

#
#
#

# END ----
