# PhD Course, Biostatistical modeling in Ag. Science, Pre-course, Day 1
# Reading data ---- 
# Aim: Loading data into R.

#
#
#

# Packages ----

# When you open R, ~15 basic "packages" - collections of R functions, data, and compiled code - 
# are automatically loaded in. 
# Almost always you need to do more than these base packages allow you to, and that means you may 
# need to install extra packages, and then load them in R. 

# In the next task, we will need package called "readxl". Let's install and load this. 

# Alternatively, install with code:
install.packages("readxl")

# You only install once! You now already have this in your computer, and you only need to tell 
# R to start using it. Once you've done this, you should silence the installation code, or remove
# it from your script.

# To load the package in R use function "library"
library(readxl) # (this is a package for reading excel files)
# This part, you do again every time you restart R. 


#
#
# TASK 1: ----

# Later on in this script, you will need package called "dplyr". Install this, and load it. 

#
#
#

####
#
# READING DATA IN R ----
#
####

# TASK 2: ----

# There are multiple ways of reading in data in R. Here are a couple short-cuts.

# First, find a file called "Biostat2025_BachmaierNitrogen.xlsx" in Sharepoint and save it to 
# your own computer.

#
#
# Method 1: COPY-PASTE ----

# ATTENTION!!!!!
# This is not a sustainable way, but it can be used for quick checks of data!

# Step 1: Go to an excel sheet and select and copy your data (This is best done with small datasets).
# Step 2: Read in the line below:

# For PC: 
data <- read.table("clipboard", header=T, sep = "\t")
data

# For Mac: 
data <- read.table(pipe("pbpaste"), sep="\t", header=T)
data

#
#
#

# Method 2: MANUAL FILE SELECTION ----

# Select the file you wish to use by point-and-click:
data <- read_xlsx(file.choose())
# This has opened a new window on your computer. Make sure you open it, and use it to select the 
# file you wish to open.
data

# Note: In the last two examples, we created an object called "data" - but R doesn't allow you to 
# have two objects of the same name. In the second example, you are overwriting the first "data" 
# you created! 

#
#
#

# Method 3: POINT-AND-CLICK ----

# Go to the "Environment" tab in the top right corner of R studio. 
# Choose "Import dataset" and click through the options in the window that opens.

#
#
#

# Method 4: READ DATA WITH CODE ----

# Note - this is the most reproducible, trackable and recommendable practice: This is what you 
# should aim for for your own projects! 

# How to do this depends on the type of your file. These are for the three most common options - 
# comma separated values file (csv), text files (txt), and excel sheet (xlsx).

# Go ahead and download files Biostat2025_BridgesCucumber.txt and Biostat2025_.csv from Sharepoint into
# the same location you downloaded the xlsx file earlier.

#
# csv-file ----
# Check the type of data you are working with, and use this code if your data is in csv-format.

corn <- read.csv("data/Biostat2025_NassCornTexas.csv")

#
# txt-file ----

cucumber <- read.table("data/Biostat2025_BridgesCucumber.txt", header=TRUE)
# What happens if you include/don't include the header=TRUE?

# 
# Excel file ----

# There are multiple packages that allow you to read in excel-sheets. We use "readxl" that we 
# installed and loaded in before.

# read_excel() is a function we use from package readxl. Note, we specify the folder the datasheet 
# is in, followed by dash (/), and then the name of the file.
# For excel sheets, you also need to specify the sheet you wish to read in within the file.
# Take a look at the code for how this is done. 
dataxl <- read_excel("data/Biostat2025_BachmaierNitrogen.xlsx", sheet="Data")
dataxl

# Did it work? Do you actually have a folder called "data" in your computer? Make sure you specify the
# path to the file as it should be in your own computer.

# Let's name the nitrogen data stored in the data-object as something more easy to identify
nitrogen <- dataxl

# You should now have at least five objects: nitrogen, cucumber, ..., and data and dataxl in your 
# environment - and some of them contain the same data. Remove the ones you don't wish to use by filling in the object you don't
# wish to keep in the code below. 

# (rm = remove)

rm()

#
#
#

### 
#
# Looking at your data ----
#
###

# You have three datasets - try to look at all of them and make observations with the code below.

# You can look at your entire dataset by clicking at the object in the top right corner of Rstudio.
# You can also print it out in your console by typing its name. 

cucumber
# Or by typing 
View(cucumber)

# To get a quick glimpse, you can look at the first 6 rows:
head(nitrogen)

# Or the last 6 rows:
tail(corn)

# You can summarise all the columns you have, to get an idea of what your data looks like:
summary(cucumber)

# Or you can check the structure of your data, including the class of each colum:
str(corn)
# The class specifies whether R thinks each column is numeric, an integer (whole number), 
# a factor (a categorical variable), or a character (a string of letters)

#
#
# Subset your data ----
# We are going to use functions from package dplyr. You should have read this package in in Task 1.
# However, if you haven't make sure to do so here.

# Let's take a subset of the cucumber data only containing observations of the cucumber variety
# "Guardian"

# First, you need to make sure that the variety is read in as a factor - a categorical variable
# with set levels
cucumber$gen <- factor(cucumber$gen)

# Now, take a subset
cucumber_guardian <- filter(cucumber, gen=="Guardian")

# Check the dataset. Did the subsetting work?

#
#
# R has many ways of taking subsets, and just to show you another way of creating the subset 
# of Guardian data:
cucumber_guardian <- cucumber[which(cucumber$gen=="Guardian"),]

# Note 2 things above.
# 1: square brackets. Square brackets look into the object in the form: DATA[rows, columns].
# Meaning, that if you want to look at row 3 of a dataset called data, you could write:
data[3,]
# Or maybe you want to get the 5th column;
data[,5]
# Or maybe you wish to get the 5th column value of row 3:
data[3,5]

# 2: Dollar sign. Dollar sign extracts a specific column out of your data.
# For example, here's how to look at values in column yield in nitrogen:
nitrogen$yield


#
# TASK 5: ----
# Take a subset of nitrogen data, only containing the zone "low"


#
# 
# But wait! 
# There are mistakes in all of the datafiles! 

# Corn mistakes ----
summary(corn)
# Look at the summary of the year variable. Are all the values there realistic?
# The maximum for one looks to be unreasonable

# To fix it in R, we can first try to make sure that we can point at the right value in the dataset
# First, this is how we can point at a maximum
max(corn$year)

# What should it be? Look at the data for clues
View(corn)

# next, we can filter out the data to only include rows with the maximum year
corn[corn$year==max(corn$year),]

# Now, we want to change the first value in that subset - let's aim at column year
corn[corn$year==max(corn$year),]$year

# And, let's give it a new value based on what the year in question is likely to be
corn[corn$year==max(corn$year),]$year <- 1897

# Check if this worked.
summary(corn)


#
# TASK 6: ----
# Corn data has another mistake. Can you spot it? Can you fix it with code?


#
# Cucumber mistakes ----

# Look at the summary of the cucumber data
summary(cucumber)

# We need to turn the location variable into a factor to see this fully
cucumber$loc <- factor(cucumber$loc)
summary(cucumber)

# It looks like there are 15 observations from Clemson, and 1 observation from Clemsont
# This is likely a typo. How can we fix it?

# First, how do we point at exactly the row where the mistake is in in the data?
cucumber[cucumber$loc=="Clemsont",]

# And just the cell with the location typo
cucumber[cucumber$loc=="Clemsont",]$loc

# Let's fix it!
cucumber[cucumber$loc=="Clemsont",]$loc <- "Clemson"

# Did it work?
summary(cucumber)

# Factors can be funny - they will retain information about factor levels even if the data is no longer there
# To get rid of empty factor levels, simply re-factor the variable:
cucumber$loc <- factor(cucumber$loc)
summary(cucumber)

#
# TASK 7: ----
# Can you see other mistakes in the cucumber data? Try fixing them. 


#
# TASK 8: ---
# Nitrogen data also has mistakes. Try to see if you can spot them, and think of a way of fixing them.


#
# END ----