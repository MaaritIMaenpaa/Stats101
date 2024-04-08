# Open workshop in R, Session 1
# Aim: My first graph in R
 
# When you start a script, it is good practice to define the folder that you are using for all relevant files.
# To tell R where you keep your data, scripts, and everything else that is relevant, set a working directory at the beginning of the script.

####
#
# Set working directory
#
####

# Change the path below (in the quotation marks) into the correct path to the folder you are using
setwd("C:/Users/au721810/Git Repositories/Stats101")

# Check what working directory you are using
getwd()

####
# Logic of R

# You can run this for using R as a calculator
1+2

# But you may also wish to save the results
results <- 1+2 

# The arrow above is read as "gets", and the "results" is a new object we create
# You can change the name of the new object to anything you wish it to be. 
# Check the environment in the top right corner of Rstudio for the objects that R currently can see in your workspace.

# To print out the results from the object you create, simply write the name, and run the line of code:
results


#####
#
# Reading in data
#
#####

# There are multiple ways of reading in data. Here are a couple short-cuts:

#
# Read in what you have copied in the clipboard of your computer:

# Step 1: Go to an excel sheet and select and copy your data (This is best done with small datasets).
# Step 2: Read in the line below:
data <- read.table("clipboard", header=T)
data


# 
# Select the file you wish to use by point-and-click:
data <- read.table(file.choose(), header=T)
# This has opened a new window on your computer. Make sure you open it, and use it to select the file you wish to open.
data

# Note: In the last two examples, we created an object called "data" - but R doesn't allow you to have two objects of the same name.
# In the second example, you are overwriting the first "data" you created! 

#
# 
# Read in data in all code

# Note - this is the most reproducible, trackable and recommendable practice: This is what you should aim for for your own projects! 

# Comma separated files
# Check the type of data you are working with, and use this code if your data is in csv-format.

data <- read.csv("example_data/Rexample_CO2.csv")
# Note: Here, we are reading a file called "Rexample_CO2.csv" from a subfolder "example_data", which is located within the folder we specified as the working directory.
# Make sure you have the folder structure correctly matching to where your files are located!


# 
# Excel files

# There are multiple packages (=collections of R functions) that allow you to read in excel-sheets.
# The example here is only one option

# If you don't already have a package called readxl installed in R, run the line below
install.packages("readxl")

# You only need to use the install.packages() ONCE! After you do it, you have downloaded a package into your computer, and they are ready to be used.
# However, you need to use function library() to activate the package. This needs to be done every time you start up R again. 
library(readxl)

# read_excel() is a function we use from package readxl. Note, we specify the folder the datasheet is in, followed by dash (/), and then the name of the file.
# For excel sheets, you also need to specify the sheet you wish to read in within the file.
# Take a look at the code for how this is done. 
dataxl <- read_excel("example_data/Rexample_CO2.xlsx", sheet="Rexample_CO2")
dataxl

#
#
#

###### 
#
# Looking at your data
#
#####

# You can look at your entire dataset by clicking at the object in the top right corner of Rstudio.
# You can also print it out in your console by typing its name:
dataxl
# Or by typing 
View(dataxl)

# To get a quick glimpse, you can look at the first 6 rows:
head(data)

# Or the last 6 rows:
tail(data)

# You can summarise all the columns you have, to get an idea of what your data looks like:
summary(data)

# Or you can check the structure of your data, including the class of the data:
str(data)
# The class specifies whether R thinks each column is numeric, an integer (whole number), a factor (a categorical grouping), or a character (a string of letters)

#
#
# To take a subset, you can use code from package dplyr (remember to install it if you don't already have it!)
library(dplyr)

# Let's take a subset of the example data only containing observations in the category "nonchilled"
nonchilled_data <- filter(dataxl, Treatment=="nonchilled")

# Or opposite to that, let's take a subset of everything except the nonchilled plants
chilled_data <- filter(dataxl, Treatment=="nonchilled")

##
#
# R has many ways of taking subsets, and just to show you another way of creating the subset of nonchilled data:
nonchilled_data <- dataxl[which(dataxl$Treatment=="nonchilled"),]

# Note 2 things above.
# 1: square brackets. Square brackets look into the object in the form: DATA[rows, columns].
# Meaning, that if you want to look at row 3 of the dataset, you could write:
dataxl[3,]
# Or maybe you want to get the 5th column;
dataxl[,5]
# Or maybe you wish to get the 5th column value of row 3:
dataxl[3,5]

# 2: Dollar sign. Dollar sign extracts a specific column out of your data.
# For example, here's how to look at column Plant in dataxl:
dataxl$Plant

# Check this out for more useful functions for data wrangling: https://www.rstudio.com/wp-content/uploads/2015/02/data-wrangling-cheatsheet.pdf

####
#
# MAKING YOUR FIRST FIGURE
#
####

# You need the library called ggplot2 (install first if you don't have it already!)
library(ggplot2)

# ggplot works in layers. The first layer defines your axes:
ggplot(dataxl, aes(x=Treatment, y=uptake, colour=Type)) 

# I don't like the gray background, so I want to make this black and white. Let's add a layer using a +-sign
ggplot(dataxl, aes(x=Treatment, y=uptake, colour=Type)) +
  theme_bw()

# That's nicer, but I actually want it even more minimalistic
ggplot(dataxl, aes(x=Treatment, y=uptake, colour=Type)) +
  theme_classic()

# Ok, let's add some content. How about points indicating all the observations we have?
ggplot(dataxl, aes(x=Treatment, y=uptake, colour=Type)) +
  theme_classic() +
  geom_point()

# Let's scatter the points so that we see them better
ggplot(dataxl, aes(x=Treatment, y=uptake, colour=Type)) +
  theme_classic() +
  geom_point(position=position_jitter(.1))


# I want to see the mean and standard error as well. 
ggplot(dataxl, aes(x=Treatment, y=uptake, colour=Type)) +
  theme_classic() +
  geom_point(position=position_jitter(.1)) +
  stat_summary()

# But I want to see the means better. Let's change their size:
ggplot(dataxl, aes(x=Treatment, y=uptake, colour=Type)) +
  theme_classic() +
  geom_point(position=position_jitter(.1)) +
  stat_summary(size=3)

# I want the original datapoints all to be in gray colour. Let's change that.
ggplot(dataxl, aes(x=Treatment, y=uptake, colour=Type)) +
  theme_classic() +
  geom_point(position=position_jitter(.1), colour="gray") +
  stat_summary(size=3)


# That's nice, but maybe a boxplot would be better?
ggplot(dataxl, aes(x=Treatment, y=uptake, colour=Type)) +
  theme_classic() +
  geom_boxplot() 

# I want to change where the legend is located. Let's put it on the bottom of the graph.
ggplot(dataxl, aes(x=Treatment, y=uptake, colour=Type)) +
  theme_classic() +
  geom_boxplot() +
  theme(legend.position="bottom")

# You may notice that you can modify things within each layer based on what they control.
# Most common modifications refer to "size", "shape", "fill", and "colour". Try to change them!
# ggplot is a googling excercise. Figure out what you want to do, and find an example online for how to do it!  :)

#
#
# Saving a figure in R. 

# You can export your figure using the "Export" button in the bottom right corner of Rstudio. 

# However, a more controlled way to do that is using ggsave function:

ggsave("output/ExampleBoxplot.png", last_plot(), width=20, height=15, units="cm")
# Now check the folder where you keep your files. There, in a folder called "output" you should find a png-figure. 
# last_plot will save the latest plot you printed. However, you can also save your ggplots, and call them by their names:

# First, run the code for creating the figure by giving a name to the plot you create
boxplot <- ggplot(dataxl, aes(x=Treatment, y=uptake, colour=Type)) +
  theme_classic() +
  geom_boxplot() +
  theme(legend.position="bottom")

# Now use the name in the code to save your figure:
ggsave("output/ExampleBoxplot.png", boxplot, width=20, height=15, units="cm")


