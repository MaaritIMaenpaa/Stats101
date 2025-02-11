# Load necessary libraries
library(lme4)
library(glmmTMB)
library(DHARMa)
library(tidyverse)
library(nlme)
library(emmeans)

# Example 1 -------------------------------------------------------------------
# Load your data
# Replace 'path_to_your_data.csv' with the actual path to your data file
atriplex <- read.table('2025 Biostatistical modeling for Ag. Science/Day 3/Atriplex.txt', 
                       header=TRUE, sep='\t')

# Inspect the first few rows of the data
head(atriplex)

#Some context
# https://identify.plantnet.org/the-plant-list/species/Atriplex%20cordobensis%20Gand.%20&%20Stuck./data
# Data from Atriplex cordobensis, a forage shrub. The experimental units are arranged in complete blocks.
# Data courtesy of Dr. M. T. Aiazzi, Faculty of Agricultural Sciences, UNC

# Description:
# Size: Size of the seed
# Color: seed coat color
# Germination: Germination percentage
# Normal.seeding
# DW: Dry Weight
# Block: block identification


# Visualize the data ----
atriplex |>  
  ggplot(aes(x = Color, y = Germination, 
                        col=as.character(Block))) +
  geom_point(size=5) +
  facet_grid(.~Size)+
  theme_bw()

# The Block is assumed to be continous so I will convert it to a factor
atriplex$Block <- as.factor(atriplex$Block)

# Models ----
# Fit the first mixed linear model using lmer from lme4 package
# Block is used as a random variable - pay attention to the notation
# (1|Block) the "1" here refers to the intercept (it means block affects the intercept of the model)
model1 <- lmer(Germination ~ Color + Size + (1 | Block), 
               data = atriplex)

# Fit the second mixed linear model using lme from the nlme package
# Notice that the random effect notation is different, but also contains the "1" for intercept
model2 <- lme(Germination ~ Color + Size, 
                     random = ~1 | Block, 
                     data = atriplex)

# Fit the third mixed linear model using glmmTMB
model3 <- glmmTMB(Germination ~ Color + Size + (1 | Block), 
                  data = atriplex)

# Diagnostics ----
# Compare the models using AIC
AIC(model1, model2, model3)

# Evaluate the residuals using DHARMa for model1
simulationOutput1 <- simulateResiduals(fittedModel = model1)
plot(simulationOutput1)

# Evaluate the residuals using DHARMa for model2
simulationOutput2 <- simulateResiduals(fittedModel = model2)
plot(simulationOutput2)

# Evaluate the residuals using DHARMa for model3
simulationOutput3 <- simulateResiduals(fittedModel = model3)
plot(simulationOutput3)

# Extract the fitted values and residuals
atriplex$fitlme <- fitted(model1)
atriplex$reslme <- residuals(model1)

# Plot the fitted values vs. the observed values
ggplot(atriplex, aes(x = fitlme, y = Germination)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  theme_minimal() +
  labs(title = "Fitted vs Observed Values",
       x = "Fitted Values",
       y = "Observed Values")

# Plot the residuals
ggplot(atriplex, aes(x = fitlme, y = reslme)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  theme_minimal() +
  labs(title = "Residuals vs Fitted Values",
       x = "Fitted Values",
       y = "Residuals")

# Results ----
# Summarize the results of model1
car::Anova(model1)
summary(model1)

# Summarize the results of model2
car::Anova(model2)
summary(model2)

# Summarize the results of model3
car::Anova(model3)
summary(model3)

# Visualise the results ----
# Calculate marginal effects using emmeans 
marginal_effects_glmm <- emmeans(model1, ~Color | Size)
marginal_effects_glmm

# Plot marginal effects for m1.glmm
plot(marginal_effects_glmm) +
  theme_bw() +
  labs(x = "Estimated marginal mean",
       y = "Predictors")

# Second method for extracting model estimates
# Calculate marginal effects using ggeffects 
marginal_effects <- ggeffects::ggpredict(model1, terms = c("Color", "Size"))
marginal_effects

# Plot marginal effects
plot(marginal_effects) +
  theme_minimal() +
  labs(title = "Marginal Effects of Color and Size on Germination",
       x = "Predictors",
       y = "Predicted Germination")

# Example 2 -------------------------------------------------------------------

# Data ----
library(agridat)
data(crowder.seeds)
head(crowder.seeds)
# To see more details of the data:
??crowder.seeds

# Visualize the data ----
crowder.seeds |> 
  ggplot(aes(x = gen, y = germ / n)) +
  geom_point() +
  geom_boxplot(aes(group = gen), alpha = 0.5) +
  facet_wrap(~ extract) +
  theme_minimal() +
  labs(title = "crowder.seeds",
       x = "Gen",
       y = "Germination Rate")


# Model ----
# Fit a linear mixed model using glmmTMB
m1.glmmtmb <- glmmTMB(germ / n ~ gen * extract + (1 | plate),
                      data = crowder.seeds)

# Diagnostics ---
# Evaluate the residuals using DHARMa for m1.glmmtmb
plot(simulateResiduals(fittedModel = m1.glmmtmb))

# Extract the fitted values and residuals for m1.glmm
crowder.seeds$fit_glmm <- fitted(m1.glmmtmb)
crowder.seeds$res_glmm <- residuals(m1.glmmtmb)

# Plot the fitted values vs. the observed values for m1.glmm
ggplot(crowder.seeds, aes(x = fit_glmm, y = germ/n)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  theme_minimal() +
  labs(title = "Fitted vs Observed Values (glmmPQL)",
       x = "Fitted Values",
       y = "Observed Values")

# Results ----
# Summarize the results of m1.glmmtmb
car::Anova(m1.glmmtmb)
summary(m1.glmmtmb)

# Visualise the results ----
# Calculate marginal effects using emmeans 
marginal_effects_glmm <- emmeans(m1.glmmtmb, ~ extract | gen)
marginal_effects_glmm

# Plot marginal effects for m1.glmm
plot(marginal_effects_glmm) +
  theme_bw() +
  labs(x = "Estimated marginal mean",
       y = "Predictors")

# Second method of extracting marginal effects
# Calculate marginal effects for m1.glmm using ggeffects
marginal_effects_glmm <- ggeffects::ggpredict(m1.glmmtmb, terms = c("gen", "extract"))
marginal_effects_glmm

# Plot marginal effects for m1.glmm
plot(marginal_effects_glmm) +
  theme_minimal() +
  labs(title = "Marginal Effects of Gen and Extract on Germination (glmmPQL)",
       x = "Predictors",
       y = "Predicted Germination")
