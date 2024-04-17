# RECAP SESSION ----

# This script shows example code for the steps you need to take to go from reading 
# in your  data to illustrating it with a figure, exploring it with a linear model, 
# and interpreting the results. 

# TIP: Document outline ----
# Click on the button at the top right corner of the script window, next to Source. 
# You can use the list to navigate through this script.
# If you write "----" at the end of a commented line, you can see it appearing on the
# list on of topics. 

#
#
#

# Set working directory ----

# Change the path below (in the quotation marks) into the correct path to the folder you are using
setwd("C:/Users/au721810/Git Repositories/Stats101")

# Check what working directory you are using
getwd()

#
#
#

# Packages ----
library(ggplot2)
library(emmeans)


#
#
#

# READING IN DATA ----

#
# Comma separated files (Recommended)

data <- read.csv("example_data/Rexample_CO2.csv")
# Note: Here, we are reading a file called "Rexample_CO2.csv" from a subfolder 
# "example_data". This folder is located in the folder we specified as the root of 
# the working directory. 

# Make sure you have the folder structure correctly matching to where your files are located!


# 
# Excel files

# You need an extra package, for example, this one:
library(readxl)

# read_excel() is a function we use from package readxl. 
# Note, we specify the folder the datasheet is in, followed by dash (/), 
# and then the name of the file.
# You also need to specify the sheet you wish to read within the file.
# Take a look at the code for how this is done. 
dataxl <- read_excel("example_data/Rexample_CO2.xlsx", sheet="Rexample_CO2")
dataxl

#
#
#

# Figure of the data ----

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
  stat_summary(size=2)

# I want the original datapoints all to be in gray colour. Let's change that.
ggplot(dataxl, aes(x=Treatment, y=uptake, colour=Type)) +
  theme_classic() +
  geom_point(position=position_jitter(.1), colour="gray") +
  stat_summary(size=2)

# That's nice, but maybe a boxplot would be better?
ggplot(dataxl, aes(x=Treatment, y=uptake, colour=Type)) +
  theme_classic() +
  geom_boxplot() 

# Let's add some colour in the boxes. Let's fill the boxes with a colour based on 
# plant type, and let's draw the boxes with black outlines.
ggplot(dataxl, aes(x=Treatment, y=uptake, colour=Type)) +
  theme_classic() +
  geom_boxplot(aes(fill=Type), colour="black") 

# I want to change where the legend is located. Let's put it on the bottom of the graph.
ggplot(dataxl, aes(x=Treatment, y=uptake, colour=Type)) +
  theme_classic() +
  geom_boxplot(aes(fill=Type), colour="black") +
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
# Now check the folder you set as working directory. 
# There, in a folder called "output" you should find a png-figure. 
# last_plot will save the latest plot you printed. 

# However, you can also save your ggplots, and call them by their names:

# First, run the code for creating the figure by giving a name to the plot you create
boxplot <- ggplot(dataxl, aes(x=Treatment, y=uptake, colour=Type)) +
  theme_classic() +
  geom_boxplot(aes(fill=Type), colour="black") +
  theme(legend.position="bottom")

# Now use the name in the code to save your figure:
ggsave("output/ExampleBoxplot.png", boxplot, width=20, height=15, units="cm")

#
#
#

# Linear model ----

# From the example data: is CO2 uptake different in different treatments.
# Look at the figure we created, and match the model to what you see.
mod_cat <- lm(uptake ~ Treatment + Type + Treatment:Type, data=data)

# Is this model any good?
par(mfrow=c(1,3))
qqnorm(resid(mod_cat));qqline(resid(mod_cat))
hist(resid(mod_cat))
plot(fitted(mod_cat), resid(mod_cat)); abline(h=0)
par(mfrow=c(1,1))
# What can you tell about the assumptions?

# What does the model tell us?
summary(mod_cat)
# Residuals: Look for -
# 1. Median should be close to 0 - means that the estimate from the model is unbiased
# 2. Symmetry: min and 1st quartile (1Q), compared to third quartile (3Q) and max, respectively, 
# should be approximately equidistance from the median

# Coefficients: Estimate + Std. Error = effect size, model estimated mean
# Intercept is the mean for the treatment that is not listed below (=nonchilled, Mississippi)
# Treatmentchilled is the DIFFERENCE between the mean of the chilled and the mean of the nonchilled.
# What does TypeQuebec mean? What about the interaction?

# The t-values and the p-values test for whether the estimate is different from 0
# Intercept test shows nonchilled plants have non-zero CO2-uptake
# Treatment test shows that the two treatments have significantly different uptakes

# F-statistic, DF and p-value are the test of the variance of the entire model - do the fixed effects explain the variation?

#
#
#

# Post-hoc comparisons ----

# Note: This makes sense when you have more categories than 2 in the factor. 
# In this case, post-hoc pairwise comparisons are unnecessary, however, we illustrate
# how to run them so you could use them for models with more categories.

#
# emmeans - short for estimated marginal means 

# For main effects

# To see what the estimated means for Treatment (averaged over Type) according to the model are:
emmeans(mod_cat, ~Treatment)

# Add a test for the pairwise comparisons between treatments. Look into the contrasts table:
emmeans(mod_cat, pairwise ~Treatment)
# The t-value and the p-value test wether the difference between two treatments (estimate)
# is different from 0. That means: are the two treatments different from one another.

# For interactions 

# Explore what is the difference between these two options:
emmeans(mod_cat, ~Treatment|Type)
emmeans(mod_cat, ~Treatment:Type)
emmeans(mod_cat, ~Type|Treatment)

# Also explore with this notation:
emmeans(mod_cat, pairwise ~ Type|Treatment)

# Are you missing the compact display letters? 
# Note that you don't actually need them, you already have all the information from above. 
# But if you want them, you need to use a function from another package:
library(multcomp)
library(multcompView)

# Save the emmeans output
em_factors <- emmeans(mod_cat, pairwise ~ Type|Treatment)

# Create the cld-output
cld(em_factors, Letters=letters)

#
#
#

# Visualise the model predictions ----

# First, extract the fitted values from the linear and polynomial model
data$fit_cat <- mod_cat$fitted.values

# Plot the values on the figure from before
ggplot(data, aes(x=Treatment, y=uptake, colour=Type)) +
  theme_classic() +
  geom_boxplot(aes(fill=Type), colour="black") +
  theme(legend.position="bottom") +
  stat_summary(aes(y=fit_cat, fill=Type), # Change the y-axis to the fitted values we just added into the dataset 
               colour="black", # Make sure the means are visible
               position=position_jitterdodge(.05), # This is to match the position of the boxes of the boxplot 
               size=1.5) # Size of the model estimate point 
#
#
#

# Alternative: Plot emmeans table:

# Let's save the means from the emmeans-table as a dataframe
mod_estimates <- data.frame(emmeans(mod_cat, ~ Type|Treatment))

# Plot the mean values on the figure from before
ggplot(data, aes(x=Treatment, y=uptake, colour=Type)) +
  theme_classic() +
  geom_boxplot(aes(fill=Type), colour="black") +
  theme(legend.position="bottom") +
  geom_point(data=mod_estimates, aes(y=emmean, x=Treatment, fill=Type), # For the means
             size=5, colour="black", position=position_jitterdodge(.05))  # Making the means look better 


#
#
#

# Linear model with a continuous predictor ----

# A model explaining CO2 uptake by CO2 concentration (=continuous variable)

# First a figure
ggplot(CO2, aes(x=conc, y=uptake)) + 
  theme_classic() +
  geom_point()  + 
  stat_smooth(method=lm)

# Model
mod_cont <- lm(uptake ~ conc, data=CO2)  

# Assumptions of the model
par(mfrow=c(1,3))
qqnorm(resid(mod_cont));qqline(resid(mod_cont))
hist(resid(mod_cont))
plot(fitted(mod_cont), resid(mod_cont)); abline(h=0)
par(mfrow=c(1,1))
# Issues here, especially with the residuals vs. fitted - there is a pattern
# Normally you would try to fix this before looking into the results. 

# The (not-trustworthy) results from the model 
summary(mod_cont)
# Difference to the model before:

# Coefficients: Estimate + Std. Error = effect size, model estimated slope for continuous variable
# Intercept is the value of CO2 uptake when the continuous variable is at 0
# conc is the slope of the association between CO2 concentration and CO2 uptake (see the figure drawn from the data before)
# The t-values and the p-values still test for whether the estimate is different from 0
# This time the intercept tests for whether the CO2 uptake at conc=0 is significantly non-zero
# And the conc slope tests for whether there is a slope. A slope with a value of 0 means no association.

# Note: F-statistic, DF and p-value listed at the end are essentially the same as the t and p-values from the coefficient table
# This is because the model is a simple regression, with only one predictor variable
# Compare to: 
anova(mod_cont)

#
#
#

# Post-hoc comparisons ----

# Again - this only makes sense when you compare multiple slopes, so when you have
# an interaction with a categorical variable and a continuous variable in the model.

# Let's add one, for the sake of an example
mod_cont <- lm(uptake ~ conc + Treatment + conc:Treatment, data=data)  

# Is there a significant interaction?
anova(mod_cont)
# No - we should actually leave this here. 

# emtrends
emtrends(mod_cont, ~Treatment, var="conc")
emtrends(mod_cont, pairwise ~ Treatment, var="conc")

# Letters? 
# If you insist.
em_slopes <- emtrends(mod_cont, pairwise ~ Treatment, var="conc")
cld(em_slopes, Letters=letters)

# Considering there was no interaction here, we won't continue with this model.
# This was here as an example of how to extract pairwise comparisons of slopes.

#
#
# 



# 
# Non-linearity ----

# Can we fix the model by fitting a polynomial into the data? 

# Looking at the figure, we can see that the data is not very linear. 
# In stead of predicting y = a + b*x, we can test for y = a + b*x + c*x^2 

# Polynomial model
mod_poly <- lm(uptake ~ poly(conc,2), data=CO2)  

# Are assumptions improved?
par(mfrow=c(1,3))
qqnorm(resid(mod_poly));qqline(resid(mod_poly))
hist(resid(mod_poly))
plot(fitted(mod_poly), resid(mod_poly)); abline(h=0)
par(mfrow=c(1,1))
# A little improvement, but there is still a pattern in the residuals vs. fitted (last figure)

# How does the summary change?
summary(mod_poly)
# Now CO2 concentration (conc) has two components - the first order and the second order polynomial
# The first order shows the linear association
# The second one shows there is curvature in the association
# Negative estimate for x^2 - concave downwards
# Positive estimate for x^2 - concave upwards (convex)

#
#
#

# Visualise the model predictions ----

# First, extract the fitted values from the linear and polynomial model
CO2$fit_cont <- mod_cont$fitted.values
CO2$fit_poly <- mod_poly$fitted.values

# Plot the values
ggplot(CO2, aes(x=conc, y=uptake)) + 
  theme_classic() + 
  geom_point()  + # This is still raw data
  geom_smooth(aes(y=fit_cont), col="blue") + # Fit the linear model, print the line in blue 
  geom_smooth(aes(y=fit_poly), col="red") # Fit the polynomial model, print the line in red

