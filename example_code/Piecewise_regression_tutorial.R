# Packages ----
library(ggplot2)
library(nlme)
library(segmented)

#
#
#

# Simulate data for the example ----
# Note that this is just for creating an example
# For the analysis example, skip to Piecewise regression section below

set.seed(123)

# Number of observations
n <- 200

# Generate values for % weed cover
weed_perc <- seq(0, 35, length.out = n)

# Defining breakpoints (=where slope of weed % to yield changes)
bp1 <- 10    # We'll make this weak
bp2 <- 25    # We'll make this strong

# Defining slopes for each segment
slope1 <- 7
slope2 <- 20
slope3 <- 0.5

# Defining intercepts so the segments connect smoothly
intercept1 <- 5
intercept2 <- intercept1 + (slope1 * bp1) - (slope2 * bp1) 
intercept3 <- intercept2 + (slope2 * bp2) - (slope3 * bp2)

# Generate leaching values with noise
leach_reduction <- ifelse(weed_perc <= bp1,
                          intercept1 + (slope1 * weed_perc),
                          ifelse(weed_perc <= bp2,
                                 intercept2 + slope2 * weed_perc,
                                 intercept3 + slope3 * weed_perc)) +
  rnorm(n, mean = 5, sd = 20)  

# Combine into a data frame
data <- data.frame(weed_perc = weed_perc, 
                   leach_reduction = leach_reduction)

# Split the data randomly to 10 years
data$year <- factor(sample(1:10, 200, replace=TRUE))
# Note: This is making an assumption that years don't have a difference

# Numbers in years
table(data$year)

# Plot the data
ggplot(data, aes(x=weed_perc, y=leach_reduction)) +
  theme_bw() +
  geom_point(aes(colour=year)) +
  labs(x="Weed cover (%)", y="N leaching reduction (kgN/ha)") +
  geom_vline(xintercept=bp1, linetype="dashed") +
  geom_vline(xintercept = bp2, linetype="dashed") +
  geom_segment(aes(x = 0, 
                   y = intercept1, 
                   xend = bp1, 
                   yend = intercept2+slope2*bp1),
               size = 2, colour="steelblue1") +
  geom_segment(aes(x = bp1, 
                   y = intercept2+slope2*bp1, 
                   xend = bp2, 
                   yend = intercept3+slope3*bp2),
               size = 2, colour="steelblue3")+
  geom_segment(aes(x = bp2, 
                   y = intercept3+slope3*bp2, 
                   xend = 35, 
                   yend = intercept3+slope3*35),
               size = 2, colour="steelblue4")

  
#
#
#

# Piecewise regression ----

# In the example: Predicting leaching reduction based on weed % in 
# the field. Testing whether there is a threshold when the effect 
# of weed on leaching changes. 

# Run the initial model - note: using year as a random effect
model <- lme(leach_reduction ~ weed_perc, 
             random= ~1|year, 
             data = data)

# Run the segmented model
# In mixed model setting (segmented.lme) only works with one breakpoint
segmented_model <- segmented.lme(model, 
                                 seg.Z = ~ weed_perc, 
                                 random = list(year = ~ 1),
                                 data = data)

# Results:
summary(segmented_model)
plot(segmented_model)
# In the fixed effects summary, you get the estimate of the intercept, the slopes and the breakpoint
# Breakpoint estimated to be 27.03, which is pretty close to the 25 we assigned it to

# Did we need a breakpoint? Compare the AIC's
c("No breakpoint" = AIC(model), 
  "1 Breakpoint"= AIC(segmented_model))

# Linear model case ----
# Note: We ignore year because we simulated the data, you shouldn't ignore data structures in real cases
model <- lm(leach_reduction ~ weed_perc, data = data)

segmented_model <- segmented(model, 
                             seg.Z = ~ weed_perc, 
                             data = data)

summary(segmented_model)
plot(segmented_model) # Note:simple figure!

# With a linear model we can also ask for more breakpoints
segmented_model_2bp <- segmented(model,
                                 seg.Z = ~ weed_perc, 
                                 psi = c(10, 20),
                                 data = data)

summary(segmented_model_2bp)
plot(segmented_model_2bp) # Note:simple figure!


# Did we need  more breakpoints? Compare the AIC's
c("No breakpoint" = AIC(model), 
  "1 breakpoint" = AIC(segmented_model),
  "2 breakpoints" = AIC(segmented_model_2bp))

# With a linear model, this also works: 
davies.test(segmented_model) # If significant - breakpoints are there
davies.test(segmented_model_2bp) 

#
#
#

# Alternative package: strucchange ----
# Doesn't work with random effects, but can test multiple breakpoints

# The package
library(strucchange)

# Linear model
model <- lm(leach_reduction ~ weed_perc, 
            data = data)

# Test for structural breakpoints
bp_test <- breakpoints(leach_reduction ~ weed_perc, 
                       data = data)

summary(bp_test)
# Note: lower BIC + RSS --> better model 

# Plot the breakpoint number likelihood
plot(bp_test)

# Get estimated breakpoints (indices)
bp_indices <- bp_test$breakpoints
bp_indices # These are observation numbers
bp_values <- data$weed_perc[bp_indices]
bp_values # These are observation numbers turned into the scale of the weed percentage

# For slopes:
# Fit segmented model using breakpoints
fitted_model <- lm(leach_reduction ~ breakfactor(bp_test) + weed_perc, data = data)

summary(fitted_model)

#
#
#

# Alternative using mcp ----
# Note: Requires JAGS - https://sourceforge.net/projects/mcmc-jags/

# Package
library(mcp)

# Specify the model formula for all segments expected
model <- list(leach_reduction 
              ~ 1 + weed_perc + (1|year),   # first segment
              ~ 1 + weed_perc + (1|year),     # second segment
              ~ 1 + weed_perc + (1|year)      # third segment
              )

# Fit the model
fit <- mcp(model, 
           data = data, 
           adapt = 5000, 
           chains = 2, 
           cores = 2)

# Summarize results
summary(fit)
# Estimates for breakpoints (=changepoint, cp), intercepts, residual error (=sigma), and slopes

# Plot breakpoints and uncertainty
plot(fit)

#
#
#

# END ----