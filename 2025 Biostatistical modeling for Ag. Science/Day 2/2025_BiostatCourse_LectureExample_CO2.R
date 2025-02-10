# PhD Course, Biostatistical modeling in Ag. Science, Course, Day 2
# CO2 example ----

#
# Packages ----
library(ggplot2)
library(dplyr)
library(minpack.lm) # For a non-linear model
library(DHARMa) 

#
#
#

# Load the example dataset 
data("CO2")
head(CO2)

# Initial figure
ggplot(CO2, aes(x=conc, y=uptake, col=Plant)) +
  theme_classic() +
  facet_wrap(~Type) +
  geom_point() +
  theme(legend.position = "none")

# Reduce the dataset to only one location
CO2 <- filter(CO2, Type=="Quebec")

# Fit a model - polynomial ----
modelCO2 <- lm(uptake ~ poly(conc,3), data=CO2)

# Model matrix
model.matrix(modelCO2)

# Save the fitted values
CO2$fitted <- fitted(modelCO2)

# Data plot ----
p1 <-
  ggplot(CO2, aes(x=conc, y=uptake)) +
  theme_classic() +
  geom_point(colour="steelblue") +
  theme(legend.position = "none") +
  labs(y="CO2 uptake (mmol/m^2 sec)",
       x="CO2 concentration (mL/L)")
p1
#ggsave("CO2_data.png", p1, width=15, height=10, units="cm")

# Polynomial with residuals ----
p2 <-
  ggplot(CO2, aes(x=conc, y=uptake)) +
  theme_classic() + 
  geom_point(colour="steelblue") +
  geom_smooth(method="lm", formula = y ~ poly(x,3), 
              se=F, colour="black") + # Plot the polynomialmodel
  geom_segment(aes(x=conc, xend = conc, y=uptake, yend = fitted), 
               color = "red") + # Plot residuals
  theme(legend.position = "none") +
  labs(y="CO2 uptake (mmol/m^2 sec)",
       x="CO2 concentration (mL/L)")
p2
#ggsave("CO2_poly.png", p2, width=15, height=10, units="cm")

# Polynomial with fitted values ----
  ggplot(CO2, aes(x=conc, y=uptake)) +
  theme_classic() + 
  geom_point(colour="steelblue") +
  geom_smooth(method="lm", formula = y ~ poly(x,3), 
              se=F, colour="black") + # Plot the polynomialmodel
  geom_point(aes(y=fitted), colour="red", size=3) +
  theme(legend.position = "none") +
  labs(y="CO2 uptake (mmol/m^2 sec)",
       x="CO2 concentration (mL/L)")

#
# Prediction ----

# Predict into a vector
pred_poly <- predict(modelCO2,
                     newdata = data.frame(conc=seq(min(CO2$conc), max(CO2$conc), 10)),
                     se=T)

# Create a dataframe
pred <- data.frame(fit=pred_poly$fit,
                   fit_se =pred_poly$se.fit,
                   conc=seq(min(CO2$conc), max(CO2$conc), 10))

# Polynomial with prediction ----
ggplot(CO2, aes(x=conc, y=uptake)) +
  theme_classic() + 
  geom_point(colour="steelblue") +
  geom_smooth(method="lm", formula = y ~ poly(x,3), 
              se=F, colour="black") + # Plot the polynomialmodel
  geom_point(data=pred, aes(y=fit, x=conc), colour="darkgreen") +
  theme(legend.position = "none") +
  labs(y="CO2 uptake (mmol/m^2 sec)",
       x="CO2 concentration (mL/L)")


# Polynomial with fitted values ----
ggplot(CO2, aes(x=conc, y=uptake)) +
  theme_classic() + 
  geom_point(colour="steelblue") +
  geom_smooth(method="lm", formula = y ~ poly(x,3), 
              se=F, colour="black") + # Plot the polynomialmodel
  geom_point(aes(y=fitted), colour="red", size=3) +
  theme(legend.position = "none") +
  labs(y="CO2 uptake (mmol/m^2 sec)",
       x="CO2 concentration (mL/L)")


# Second order polynomial ----
modelCO2 <- lm(uptake ~ poly(conc,2), data=CO2)
CO2$fitted <- fitted(modelCO2)

p2.5 <-
  ggplot(CO2, aes(x=conc, y=uptake)) +
  theme_classic() +
  geom_point(colour="steelblue") +
  geom_smooth(method="lm", formula = y ~ poly(x,2), se=F, colour="black") +
  geom_segment(aes(x=conc, xend = conc, y=uptake, yend = fitted), 
               color = "red") +
  #  stat_regline_equation(label.x.npc = "left") +
  theme(legend.position = "none") +
  labs(y="CO2 uptake (mmol/m^2 sec)",
       x="CO2 concentration (mL/L)")
p2.5
#ggsave("CO2_poly2.png", p2.5, width=15, height=10, units="cm")


#
# Non-linear model ----
nls_CO2 <- nlsLM(uptake ~ a*(1-exp(-1*k*conc)),
                 start=list(a=40, k=0.05),
                 data=CO2)
CO2$fitted <- fitted(nls_CO2)

# Non-linear plot ----
p3 <-
  ggplot(CO2, aes(x=conc, y=uptake)) +
  theme_classic() +
  geom_point(colour="steelblue") +
  geom_line(stat = "smooth", 
            method = "nlsLM",
            formula = y ~ a*(1-exp(-1*k*x)),
            method.args=list(start=c(a=40,k=0.05)),
            linetype="solid",
            se = FALSE,
            linewidth=1) +
  theme(legend.position = "none") +
  labs(y="CO2 uptake (mmol/m^2 sec)",
       x="CO2 concentration (mL/L)")
p3

#ggsave("CO2_nls.png", p3, width=15, height=10, units="cm")

#
# GLM ----
CO2_glm <- glmmTMB(uptake ~ carbon, )


# Partial residual plots ----
sim_res <- simulateResiduals(fittedModel = modelCO2)
plot(sim_res)
plotResiduals(sim_res, form = CO2$Treatment)

