# Load necessary libraries
library(tidyverse)
library(glmmTMB)
library(DHARMa)
library(emmeans)

# Load the cochran.crd dataset from the agridat package
data("cochran.crd")

# Inspect the dataset
head(cochran.crd)
str(cochran.crd)

# Let's first visualize the data to understand the infection rates for different treatments
# Field plan visualization using ggplot2
cochran.crd |> ggplot(aes(x = col, y = row, label = trt)) +
  geom_tile(aes(fill = inf), color = "white") +
  geom_text(size = 3) +
  scale_fill_gradient(low = "white", high = "red") +
  theme_minimal() +
  labs(title = "Field Plan: cochran.crd",
       x = "Column",
       y = "Row",
       fill = "Infection %")


cochran.crd <- cochran.crd |> 
  mutate(cult_type = case_when(
  str_starts(trt, "S") ~ "Spring",
  str_starts(trt, "F") ~ "Fall",
  trt == "O" ~ "Control"
))

# Fit a model with glmmTMB with a dispersion formula spatially structured with row + col
mod_glmmTMB <- glmmTMB(inf ~ cult_type + (1 | row) + (1 | col),
                       dispformula = ~ row + col,
                       data = cochran.crd,
                       family = gaussian())

# Summarize the results of the model
summary(mod_glmmTMB)

# Evaluate residuals using DHARMa
residuals_glmmTMB <- simulateResiduals(fittedModel = mod_glmmTMB)
plot(residuals_glmmTMB)

# Perform posterior comparison of the treatment as a difference to the control using emmeans
emmc <- emmeans(mod_glmmTMB, ~ cult_type)
contrast(emmc, method = "trt.vs.ctrl", ref = "Control")

# Calculate marginal effects using ggeffects
marginal_effects <- ggpredict(mod_glmmTMB, terms = "cult_type")

# Plot the marginal effects with respect to the control
plot(marginal_effects) +
  theme_bw() +
  labs(x = "Cultivation Type",
       y = "Estimated Marginal Mean",
       title = "Marginal Effects Compared to Control")
