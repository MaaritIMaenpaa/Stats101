# Load necessary libraries
library(tidyverse)
library(lme4)
library(glmmTMB)
library(DHARMa)
library(nlme)
library(ggeffects)

# Load the data
# This example shows data from a trial with 25 wheat varieties and 6 repetitions.
# Rows and columns effects will be accounted for within each square (Replication).
# Rows and columns constitute incomplete blocks.
# This trial was conducted in 1976 in Slate Hall Farm, Cambridgeshire, UK (Gleason, 1997).
# Each replication has the 25 varieties in a square with 5 rows and 5 columns.
# Additionally, the information of position in field of each plot (Longitude and Latitude)
# is in the data set to perform models including spatial statistics.

# Gleson AC. 1997. Spatial Analysis. In RA Kempton,
# PN Fox (eds) Statistical Methods for Plant Variety Evaluation.
# Chapman & Hall, London, UK. pp. 85-116.

gleson <- read.table("2025 Biostatistical modeling for Ag. Science/Day 4/Square.lattice.Gleson.txt", 
                     header = TRUE, 
                     sep = "\t") %>%
  mutate(Replication = as.factor(Replication),
         Yield = as.numeric(Yield))

# Inspect the data
head(gleson)
summary(gleson)

# Visualize the data
gleson %>%
  ggplot(aes(x = Column, y = Row, fill = Variety)) + 
  geom_tile() + 
  theme_minimal() + 
  facet_wrap(~Replication) +
  labs(title = "Variety Distribution by Row and Column")

gleson %>%
  ggplot(aes(x = Column, y = Row, fill = Yield)) + 
  geom_tile() + 
  theme_minimal() + 
  facet_wrap(~Replication) +
  labs(title = "Yield Distribution by Row and Column")

# Fit the simple mixed linear model using lmer from lme4 package
# Model without spatial or row/column autocorrelation
simple_model <- lmer(Yield ~ Variety + 
                       (1 | Replication) + 
                       (1 | Row) + (1 | Column), data = gleson)

# Evaluate the residuals using DHARMa for simple_model
simple_model_residuals <- simulateResiduals(fittedModel = simple_model)
plot(simple_model_residuals)

# Summarize the results of simple_model
summary(simple_model)

# Extract the fitted values and residuals for simple_model
gleson$fit_simple <- fitted(simple_model)
gleson$res_simple <- residuals(simple_model)

# Plot the fitted values vs. the observed values for simple_model
ggplot(gleson, aes(x = fit_simple, y = Yield)) +
  geom_point() +
  geom_abline(slope = 1, intercept = 0, color = "red") +
  theme_minimal() +
  labs(title = "Fitted vs Observed Values (Simple Model)",
       x = "Fitted Values",
       y = "Observed Values")

# Plot the residuals for simple_model
ggplot(gleson, aes(x = fit_simple, y = res_simple)) +
  geom_point() +
  geom_hline(yintercept = 0, color = "red") +
  theme_minimal() +
  labs(title = "Residuals vs Fitted Values (Simple Model)",
       x = "Fitted Values",
       y = "Residuals")

# Fit the mixed linear model using lme from nlme package with a spatial autocorrelation structure
spatial_model_lme <- lme(
  Yield ~ Variety,
  correlation = corGaus(form = ~ Longitude + Latitude, nugget = TRUE),
  data = gleson,
  random = ~ 1 | Replication
)

# Summarize the results of spatial_model_lme
summary(spatial_model_lme)

# Evaluate the residuals using DHARMa for spatial_model_lme
spatial_model_lme_residuals <- simulateResiduals(fittedModel = spatial_model_lme)
plot(spatial_model_lme_residuals)

# Fit the spatial linear model using glmmTMB
spatial_model_glmmtmb <- glmmTMB(
  Yield ~ Variety + (1 | Replication),
  data = gleson,
  family = gaussian(),
  dispformula = ~ Longitude + Latitude
)

# Summarize the results of spatial_model_glmmtmb
summary(spatial_model_glmmtmb)

# Evaluate the residuals using DHARMa for spatial_model_glmmtmb
spatial_model_glmmtmb_residuals <- simulateResiduals(fittedModel = spatial_model_glmmtmb)
plot(spatial_model_glmmtmb_residuals)

# Calculate marginal effects for simple_model
marginal_effects_simple <- ggpredict(simple_model, terms = c("Variety"))

# Plot marginal effects for simple_model
plot(marginal_effects_simple) +
  theme_minimal() +
  labs(title = "Marginal Effects of Variety on Yield (Simple Model)",
       x = "Variety",
       y = "Predicted Yield")