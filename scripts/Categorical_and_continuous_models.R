# Interpretation of output of linear models with continuous or categorical variables

# Packages ----
library(ggplot2)

# Data from example dataset in R
data(CO2)

# Look at the data
head(CO2)

# For summary of what this data shows
help(CO2)

# CATEGORICAL MODEL ----

# A model explaining CO2 uptake by treatment (=categorical variable)

# Figure of data
ggplot(CO2, aes(x=Treatment, y=uptake)) + 
  geom_boxplot() +
  stat_summary(size=2, colour="red")

# The linear model
mod_cat <- lm(uptake ~ Treatment, data=CO2)

# Is this model any good?
par(mfrow=c(1,3))
qqnorm(resid(mod_cat));qqline(resid(mod_cat))
hist(resid(mod_cat))
plot(fitted(mod_cat), resid(mod_cat)); abline(h=0)
par(mfrow=c(1,1))
# There are some issues with normality, but for the sake of example, we ignore them here

# What does the output look like?
summary(mod_cat)
# Residuals: Look for -
# 1. Median should be close to 0 - means that the estimate from the model is unbiased
# 2. Symmetry: min and 1st quartile (1Q), compared to third quartile (3Q) and max, respectively, should be approximately equidistance from the median

# Coefficients: Estimate + Std. Error = effect size, model estimated mean
# Intercept is the mean for the treatment that is not listed below (=nonchilled)
# Treatmentchilled is the DIFFERENCE between the mean of the chilled and the mean of the nonchilled
# The t-values and the p-values test for whether the estimate is different from 0
# Intercept test shows nonchilled plants have non-zero CO2-uptake
# Treatment test shows that the two treatments have significantly different uptakes

# Residual standard error shows the standard error of the residual distribution
# R-squared: To be discussed later 

# F-statistic, DF and p-value are the test of the variance of the entire model - do the fixed effects explain the variation?


# CONTINUOUS MODEL ----

# A model explaining CO2 uptake by CO2 concentration (=categorical variable)

# First a figure
ggplot(CO2, aes(x=conc, y=uptake)) + 
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

# 
# Non-linearity ----

# Can we fix the model by fitting a polynomial into the data? 

# Looking at the figure, we can see that the data is not very linear. In stead of predicting y = a + b*x, we can test for y = a + b*x + c*x^2 

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

# Visualise the model predictions from the model

# First, extract the fitted values from the linear and polynomial model
CO2$fit_cont <- mod_cont$fitted.values
CO2$fit_poly <- mod_poly$fitted.values

# Plot the values
ggplot(CO2, aes(x=conc, y=uptake)) + 
  geom_point()  + # This is still raw data
  geom_smooth(aes(y=fit_cont), col="blue") + # Fit the linear model, print the line in blue 
  geom_smooth(aes(y=fit_poly), col="red") # Fit the polynomial model, print the line in red
