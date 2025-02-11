# PhD Course, Biostatistical modeling in Ag. Science, Course, Day 2
# Practical ----

# Create and assess statistical models. You can use the code from examples
# given in the lectures. 
# See the example scripts for CO2 and Waynick.

#
# Packages ----
library(ggplot2)
library(dplyr)
library(agridat) # For example dataset
library(glmmTMB) # For glms

#
#
# DATA ----

# You can choose one of the two example datasets:

# Option 1: Edlinger data, our old friend 
edlinger_data <- readxl::read_excel("example_data/mwd_soc_diggdeeper.xlsx", 
                                    skip = 1,   # Skip the first row
                                    na = "NA")  # Treat "NA" as missing values

# Option 2: Vargas wheat data (from agridat)
# Published here: https://doi.org/10.2135/cropsci1998.0011183X003800030010x
data(vargas.wheat1.traits)
vargas.wheat <- vargas.wheat1.traits
head(vargas.wheat)

# See more info (including trait descriptions):
??vargas.wheat1.traits

# STUDY QUESTION ----
# Look at the data you chose. Write down a simple study question
# that you could explore on a chosen set of the measured variables.

# Note: For the sake of practice, it is ok if you ignore some variables


# 
# LINEAR MODEL ----

# Make a linear model with NO MORE than 3 predictor variables 
# based on the example dataset of your choosing.


###
# GENERALISED LINEAR MODEL ----

# Create a glm using the same response and predictors as the 
# linear model. Choose the probability distribution wisely. 


#
# ASSESS MODELS ---

# Plot model residuals for both lm and glm ----
# Show residuals as distances between observed and estimated.



# Plot fitted values from both lm and glm ----
# Compare (visually) to your raw observations


# Extract the model matrix ----
# If it is too big, look at its head and tail to assess how things are coded


# Check model assumptions for both models ----


# Partial residual plots ----
# Use partial residual plots to assess if another variable of your
# choosing should be included in the model. 


# 
# Results ----

# Explore model estimates with an ANOVA table, summaries, and post-hoc 
# tests.



# END ----