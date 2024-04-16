####
#
#  TEMPLATE FOR CREATING FOREST PLOTS 
#
####

# In this example, I am using an example dataset from R package agridat to create a forest plot.
# This example has three factors differentiating the means. You may not always need to divide by 
# colour, shape and facets. Modify the figure accordingly. 
# This is one example of a colour/shape scheme. Please modify the aesthetic features of the plot as you please. 

# Example- 

# Packages ----
library(agridat) # For example dataset
library(lme4)
library(emmeans)
library(ggplot2)

# Example data ----
data("chakravertti.factorial")
head(chakravertti.factorial)
??chakravertti.factorial

# Dummy model ----
# with three categorical factors and their interactions
model <- lmer(yield ~ gen + date + seeds + 
                gen:date +  gen:seeds + date:seeds + 
                gen:date:seeds + 
                (1|block), 
              data=chakravertti.factorial)
 
# Marginal means ----
# Save your emmeans results into an R object
em_results <- data.frame(emmeans(model, ~gen|date|seeds))
em_results

#
#
#

# Forest plot ----
# Note, depending on your model, your model estimated means in the emmeans table may be saved with "emmean" or with "response" or "prob"
# In those cases, you need to change the "emmean" in the code below to match the column in your own data.
# Always check the column names of the object em_results!

#
#
#

# Version 1

# Bare minimum information: 
ggplot(em_results, aes(y=gen, x=emmean, xmin=lower.CL, xmax=upper.CL,  
                       colour=date, 
                       shape=seeds)) + # MODIFY; variables to match your own model
  geom_vline(xintercept=0) + # Adds a line at 0 effects
  geom_errorbarh(height=.0, position=ggstance::position_dodgev(height=0.75)) +
  geom_point(size=3, position=ggstance::position_dodgev(height=0.75)) 

# Made to look nice:
ggplot(em_results, aes(y=gen, x=emmean, xmin=lower.CL, xmax=upper.CL,  
                     colour=date, 
                     shape=seeds)) + # MODIFY; variables to match your own model
  theme_bw(base_size = 11) +
  geom_vline(xintercept=0, colour="black", linetype="dashed") + # Adds a line at 0 effects
  geom_errorbarh(height=.0, position=ggstance::position_dodgev(height=0.75)) +
  geom_point(size=3, position=ggstance::position_dodgev(height=0.75)) +
  scale_colour_brewer(palette = "Dark2") +
  scale_shape_manual(values = c(15, 16, 17)) + # MODIFY: number of shapes listed needs to match the shape FACTOR2
  labs(x="Effect Size (yield)", # MODIFY: Add the title as appropriate 
       y="Genotype", # Add axis label name
       shape = "Seeds",  
       colour = "date") +
  theme(legend.position = "top", 
        panel.grid.minor = element_blank()) 

#
#
#

# Version 2

ggplot(em_results, aes(y=gen, x=emmean, xmin=lower.CL, xmax=upper.CL, colour=date)) + 
  theme_bw(base_size = 11) +
  facet_grid(seeds ~ ., scales = "free_y", space = "free_y") + # This is the added layer
  geom_vline(xintercept=0, colour="black", linetype="dashed") +
  geom_errorbarh(height=.0, position=ggstance::position_dodgev(height=0.5))  +
  geom_point(size=3, position=ggstance::position_dodgev(height=0.5)) +
  scale_colour_brewer(palette = "Dark2") +
  labs(x="Effect Size (yield)",  
       y="Genotype") +  
  theme(legend.position = "top", panel.grid.minor = element_blank())  

#
#
#

# Version 3

ggplot(em_results, aes(y=gen, x=emmean, xmin=lower.CL, xmax=upper.CL, colour=date)) +
  theme_bw(base_size = 11) +
  facet_grid(seeds ~ date, scales = "free_y", space = "free_y") + # Changes in this layer
  geom_vline(xintercept=0, colour="black", linetype="dashed") +
  geom_errorbarh(height=.0, position=ggstance::position_dodgev(height=1))  +
  geom_point(size=3, position=ggstance::position_dodgev(height=1)) +
  scale_colour_brewer(palette = "Dark2") +
  labs(x="Effect Size (yield)",  
       y="Genotype") + 
  theme(legend.position = "top", panel.grid.minor = element_blank()) 

