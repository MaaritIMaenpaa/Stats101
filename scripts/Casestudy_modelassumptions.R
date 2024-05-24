# Let's take a model that we ran as an example in a previous session
# (script: From_reading_data_to_linear_model.R), and try to fix the 
# problems with the assumptions.

# First, the set-up

# Working directory ----
setwd("C:/Users/au721810/Git Repositories/Stats101")

# Packages ----
library(ggplot2)
library(lme4)

# Read in the data
data <- read.csv("example_data/Rexample_CO2.csv")

# Initial model
mod_cat <- lm(uptake ~ Treatment + Type + 
                Treatment:Type, 
              data=data)

# Stop for a moment. What is the model predicting? Let's look at the 
# figure we produced previously:

ggplot(dataxl, aes(x=Treatment, y=uptake, colour=Type)) +
  theme_classic() +
  geom_boxplot(aes(fill=Type), colour="black") +
  theme(legend.position="bottom")

# We are modeling two categorical variables (Type and Treatment) in 
# their effects on CO2 uptake. Look here for more information about 
# the dataset:
help(CO2)

# Did the model assumptions hold?
par(mfrow=c(1,3))
qqnorm(resid(mod_cat));qqline(resid(mod_cat))
hist(resid(mod_cat))
plot(fitted(mod_cat), resid(mod_cat)); abline(h=0)
par(mfrow=c(1,1))
# Data is clearly not normal (first two figures), and there is also a
# pattern of increasing error with increasing fitted values.

# What do we do?

# First things first, let's look at whether our data is properly 
# represented in the model
head(data)
# We only modeled Treatment and Type. But there are two other variables
# to consider. It looks like the experiment was 

ggplot(data, aes(x=Plant, y=uptake)) +
  theme_classic() +
  geom_boxplot()

ggplot(data, aes(x=Plant, y=uptake)) +
  theme_classic() +
  geom_boxplot(aes(fill=Type))

ggplot(data, aes(x=Plant, y=uptake)) +
  theme_classic() +
  facet_grid(~conc) +
  geom_boxplot(aes(fill=Type))

ggplot(data, aes(x=conc, y=uptake)) +
  theme_classic() +
  facet_grid(Treatment ~ Type) +
  geom_point()

ggplot(data, aes(x=conc, y=uptake)) +
  theme_classic() +
  facet_grid(Treatment ~ Type) +
  geom_point(aes(colour=Plant)) +
  #stat_smooth(method=lm)
  geom_line(aes(colour=Plant))


# Taking the full experimental design into account
mod_cat <- lmer(uptake ~ Treatment + Type + conc + 
                  Treatment:Type + 
                  Type:conc +
                  Treatment:conc +
                  Treatment:Type:conc + 
                  (1|Plant), 
              data=data)

# Assumptions
par(mfrow=c(1,3))
qqnorm(resid(mod_cat));qqline(resid(mod_cat))
hist(resid(mod_cat))
plot(fitted(mod_cat), resid(mod_cat)); abline(h=0)
par(mfrow=c(1,1))


ggplot(data, aes(x=conc, y=log(uptake))) +
  theme_classic() +
  facet_grid(Treatment ~ Type) +
  geom_point(aes(colour=Plant)) +
  stat_smooth(method=lm)


# Attempt 1 to fix residual variance (vs. fitted)
# Transformation (log/sqrt)
mod_cat <- lmer(sqrt(uptake) ~ Treatment + Type + conc + 
                  Treatment:Type + 
                  Type:conc +
                  Treatment:conc +
                  Treatment:Type:conc + 
                  (1|Plant), 
                data=data)

# Assumptions
par(mfrow=c(1,3))
qqnorm(resid(mod_cat));qqline(resid(mod_cat))
hist(resid(mod_cat))
plot(fitted(mod_cat), resid(mod_cat)); abline(h=0)
par(mfrow=c(1,1))

# Attempt 1 to fix residual variance (vs. fitted)
# Transformation (log/sqrt)
mod_cat <- glmer(uptake ~ Treatment + Type + conc + 
                  Treatment:Type + 
                  Type:conc +
                  Treatment:conc +
                  Treatment:Type:conc + 
                  (1|Plant), 
                 family=Gamma(link="log"),
                data=data)

# Assumptions
par(mfrow=c(1,3))
qqnorm(resid(mod_cat));qqline(resid(mod_cat))
hist(resid(mod_cat))
plot(fitted(mod_cat), resid(mod_cat)); abline(h=0)
par(mfrow=c(1,1))

# GLM assumptions
library(DHARMa)
plot(simulateResiduals(mod_cat))

# Attempt 2 to fix residual variance (vs. fitted)
# GLM
mod_cat <- glmer(uptake ~ Treatment + Type + conc + 
                   Treatment:Type + 
                   Type:conc +
                   Treatment:conc +
                   Treatment:Type:conc + 
                   (1|Plant), 
                 family=Gamma(link="log"),
                 data=data)

# Assumptions
par(mfrow=c(1,3))
qqnorm(resid(mod_cat));qqline(resid(mod_cat))
hist(resid(mod_cat))
plot(fitted(mod_cat), resid(mod_cat)); abline(h=0)
par(mfrow=c(1,1))

# GLM assumptions
library(DHARMa)
plot(simulateResiduals(mod_cat))

#
ggplot(data, aes(x=conc, y=uptake)) +
  theme_classic() +
  facet_grid(Treatment ~ Type) +
  geom_point(aes(colour=Plant)) +
  stat_smooth(method=lm)

# Attempt 3 to fix residual variance (vs. fitted)
# GLM
mod_cat <- lmer(uptake ~ Treatment + Type + poly(conc,2) + 
                   Treatment:Type + 
                   Type:poly(conc,2) +
                   Treatment:poly(conc,2) +
                   Treatment:Type:poly(conc,2) + 
                   (1|Plant),
                 data=data)

# Assumptions
par(mfrow=c(1,3))
qqnorm(resid(mod_cat));qqline(resid(mod_cat))
hist(resid(mod_cat))
plot(fitted(mod_cat), resid(mod_cat)); abline(h=0)
par(mfrow=c(1,1))

# Plot the model
data$Poly_fits <- fitted(mod_cat)

#
ggplot(data, aes(x=conc, y=uptake)) +
  theme_classic() +
  facet_grid(Treatment ~ Type) +
  geom_point(aes(colour=Plant)) +
  geom_point(aes(y=Poly_fits)) +
  stat_smooth(aes(y=Poly_fits))
