# PhD Course, Biostatistical modeling in Ag. Science, Course, Day 2
# Waynick soil example ----

# Packages ----
library(ggplot2)
library(dplyr)
library(agridat)
library(glmmTMB)
library(emmeans)

#
#
# Example data ----
data(waynick.soil)
head(waynick.soil)

# See details
??waynick.soil

# Initial plot ----
ggplot(waynick.soil, aes(x=carbon, y=nitro)) +
  theme_bw() +
  facet_wrap(~field) +
  geom_point(colour="steelblue") +
  labs(y="Nitrogen content (%)",
       x="Carbon content (%)")

# Filter to a single field
waynick <- filter(waynick.soil, field=="Oakley")

# Model ----
model <- lm(nitro ~ carbon, data=waynick)

# Fitted values
waynick$fits <- fitted(model)

# Data plot ----
p0 <-
  ggplot(waynick, aes(x=carbon, y=nitro)) +
  theme_classic() +
  geom_point(colour="steelblue") +
  theme(legend.position = "none") +
  labs(y="Nitrogen content (%)",
       x="Carbon content (%)")
p0
#ggsave("Waynick_data.png", p0, width=15, height=10, units="cm")

# Linear model with residuals ----
p1 <-
  ggplot(waynick, aes(x=carbon, y=nitro)) +
  theme_classic() +
  geom_point(colour="steelblue") +
  geom_smooth(method="lm", se=F, colour="black") +
  geom_segment(aes(x=carbon, xend = carbon, y=nitro, yend = fits), 
               color = "red") +
  theme(legend.position = "none") +
  labs(y="Nitrogen content (%)",
       x="Carbon content (%)")
p1
#ggsave("Waynick_stat.png", p1, width=15, height=10, units="cm")

# Mathematical model(ish) ---- 
p2 <-
  ggplot(waynick, aes(x=carbon, y=nitro)) +
  theme_classic() +
  geom_smooth(method="lm", se=F, colour="black") +
  theme(legend.position = "none") +
  labs(y="Nitrogen content (%)",
       x="Carbon content (%)")
p2
#ggsave("Waynick_math.png", p2, width=15, height=10, units="cm")

#
# Model evaluation
residuals(model)
waynick$fits <- fitted(model)

# Plot fitted values ----
p3 <-
  ggplot(waynick, aes(x=carbon, y=nitro)) +
  theme_classic() +
  geom_point(colour="steelblue1") +
  geom_smooth(method="lm", se=F, colour="black") + 
  geom_point(aes(y=fits), colour="red", size=2.5) +
  labs(y="Nitrogen content (%)",
       x="Carbon content (%)")
p3
#ggsave("Waynick_fitted.png", p3, width=15, height=10, units="cm")

# Predict ----
predict(model)

# Predict into new data
pred_vec <-
  predict(model,
          newdata=data.frame(carbon=c(0,1.5,2)), 
          se=TRUE)

# Save into a dataframe
pred <- data.frame(nitro=pred_vec$fit,
                   nitro_se =pred_vec$se.fit,
                   carbon=c(0,1.5,2))

# Plot with prediction
p4 <-
  ggplot(waynick, aes(x=carbon, y=nitro)) +
  theme_classic() +
  geom_point(colour="steelblue1") +
  geom_errorbar(data=pred, aes(ymin=nitro-nitro_se, ymax=nitro+nitro_se),
                width=0.1) +
  geom_point(data=pred, aes(y=nitro, x=carbon), colour="red", size=3) +
  geom_smooth(method="lm", se=F, colour="black") + 
  labs(y="Nitrogen content (%)",
       x="Carbon content (%)")
p4
#ggsave("Waynick_predict.png", p11, width=15, height=10, units="cm")

# Increase prediction ----
pred_vec <-
  predict(model,
          newdata=data.frame(carbon=seq(0, 10, by=0.1)), 
          se=TRUE)

pred <- data.frame(nitro=pred_vec$fit,
                   nitro_se =pred_vec$se.fit,
                   carbon=seq(0, 10, by=0.1))

# Predict way out of sample ----
ggplot(waynick, aes(x=carbon, y=nitro)) +
  theme_classic() +
  geom_point(colour="steelblue1") +
  geom_errorbar(data=pred, aes(ymin=nitro-nitro_se, ymax=nitro+nitro_se),
                width=0.1) +
  geom_point(data=pred, aes(y=nitro, x=carbon), colour="red", size=1) +
  geom_smooth(method="lm", se=F, colour="black") + 
  labs(y="Nitrogen content (%)",
       x="Carbon content (%)")

# Categorical model ----
# Note: Using unfiltered data
model_cat <- lm(nitro ~  field, data=waynick.soil)

# Model matrix ----
head(model.matrix(model_cat))
tail(model.matrix(model_cat))

# Categorical plot ----
p5 <-
  ggplot(waynick.soil, aes(x=field, y=nitro)) +
  theme_classic() +
  geom_boxplot(colour="black", fill="steelblue1") +
  labs(y="Nitrogen content (%)",
       x="Location")
p5
#ggsave("Waynick_cat.png", p13, width=15, height=10, units="cm")

# Categorical + continuous model ----
model_anc <- lm(nitro ~  field + carbon + field:carbon, 
                data=waynick.soil)

# Fitted values
waynick.soil$fits <- fitted(model_anc)

# Model matrix
head(model.matrix(model_anc))
tail(model.matrix(model_anc))

# Plot cat+cont model ----
p6 <-
  ggplot(waynick.soil, aes(x=carbon, y=nitro, colour=field)) +
  theme_classic() +
  geom_point() +
  geom_smooth(aes(y=fits), method="lm", se=F) + # Plot model estimated regression lines
  labs(y="Nitrogen content (%)",
       x="Carbon content (%)") +
  theme(legend.title = element_blank(),
        legend.position = c(0.1,0.9)) 
p6
#ggsave("Waynick_anc.png", p14, width=15, height=10, units="cm")

#
# GLM ----
waynick_glm <- glmmTMB(nitro ~ field + carbon +
                         field:carbon, 
                       family=Gamma(link="log"),
                       data=waynick.soil)

waynick.soil$GLMfits <- fitted(waynick_glm)

# Model matrix ----
head(model.matrix(waynick_glm))
tail(model.matrix(waynick_glm))

# Plot glm residuals ----
p7 <-
  ggplot(waynick.soil, aes(x=carbon, y=nitro, colour=field)) +
  scale_colour_manual(values=c("steelblue", "orange")) +
  theme_classic() +
  geom_point() +
  geom_segment(aes(x=carbon, xend = carbon, y=nitro, yend = GLMfits), 
               color = "red")  +
  geom_point(aes(y=GLMfits), colour="black") + # Plot estimates
  labs(y="Nitrogen content (%)",
       x="Carbon content (%)") +
  theme(legend.title = element_blank(),
        legend.position = c(0.1,0.9))
p7
#ggsave("Waynick_glmfit.png", p7, width=15, height=10, units="cm")


#
# Model evaluation ----

# Linear model 

# Normality with quantile plots:
qqnorm(resid(model_anc));qqline(resid(model_anc))
hist(resid(model_anc))

# Heteroscedasticity:
plot(fitted(model_anc), resid(model_anc));abline(h=0)

# DHARMa
plot(simulateResiduals(model_anc))
plot(simulateResiduals(waynick_glm))


# 
# Partial residual plots ----

# simulate residuals from the categorical model using DHARMa 
sim_res <- simulateResiduals(fittedModel = model_cat)

# Model assumptions
plot(sim_res)

# Partial residual plot - is carbon explaining leftover variation in
# model_cat

sim_res <- simulateResiduals(fittedModel = model_cat)
plotResiduals(sim_res, form = waynick.soil$carbon)

#
# Model results ----

# ANOVA ----
anova(model_anc)
car::Anova(waynick_glm)

# Null model
model_null <- lm(nitro ~ 1, data=waynick.soil)
waynick.soil$null <- fitted(model_null)

p_null <-
  ggplot(waynick.soil, aes(y=nitro, x=rownames(waynick.soil))) +
  theme_classic() +
  geom_point(colour="steelblue") +
  geom_segment(aes(x=rownames(waynick.soil), 
                   xend = rownames(waynick.soil), 
                   y=nitro, 
                   yend = null), 
               color = "red") +
  theme(legend.position = "none") +
  labs(y="Nitrogen content (%)")
p_null
ggsave("Waynick_null.png", p_null, width=15, height=10, units="cm")

# Only carbon
model_C <- lm(nitro ~ carbon, data=waynick.soil)
waynick.soil$carbfits <- fitted(model_C)

p_c <-
  ggplot(waynick.soil, aes(y=nitro, x=carbon)) +
  theme_classic() +
  geom_point(colour="steelblue") +
  geom_segment(aes(x=carbon, 
                   xend = carbon, 
                   y=nitro, 
                   yend = carbfits), 
               color = "red") +
  geom_smooth(method="lm", se=F, colour="black") + 
  theme(legend.position = "none") +
  labs(y="Nitrogen content (%)",
       x="Carbon content (%)")
p_c
#ggsave("Waynick_c.png", p_c, width=15, height=10, units="cm")

# Only field
waynick.soil$catfits <- fitted(model_cat)

p_field <-
  ggplot(waynick.soil, aes(y=nitro, x=rownames(waynick.soil))) +
  theme_classic() +
  facet_wrap(~field) +
  geom_point(colour="steelblue") +
  geom_segment(aes(x=rownames(waynick.soil), 
                   xend = rownames(waynick.soil), 
                   y=nitro, 
                   yend = catfits), 
               color = "red") +
  geom_smooth(method="lm", se=F, colour="black") + 
  theme(legend.position = "none") +
  labs(y="Nitrogen content (%)")
p_field
#ggsave("Waynick_field.png", p_field, width=15, height=10, units="cm")

# Both variables
waynick.soil$fits <- fitted(model_anc)

p_anc <-
  ggplot(waynick.soil, aes(y=nitro, x=carbon)) +
  theme_classic() +
  geom_point(colour="steelblue") +
  geom_segment(aes(x=carbon, 
                   xend = carbon, 
                   y=nitro, 
                   yend = fits), 
               color = "red") +
  geom_smooth(method="lm", se=F, aes(colour=field)) + 
  theme(legend.position = "none") +
  labs(y="Nitrogen content (%)",
       x="Carbon content (%)")
p_anc
#ggsave("Waynick_both.png", p_anc, width=15, height=10, units="cm")

# SUMMARY ----
summary(model_anc)
summary(waynick_glm)

# POST-HOC ----

multcomp::cld(emmeans(model_anc, pairwise~field), Letters=letters)
multcomp::cld(emtrends(model_anc, pairwise~field, var="carbon"), Letters=letters)

multcomp::cld(emmeans(waynick_glm, pairwise~field, type="response"), 
              Letters=letters)
multcomp::cld(emtrends(waynick_glm, pairwise~field, var="carbon", type="response"), 
              Letters=letters)
