# Packages
library(emmeans)

# For further information:
# https://bcdudek.net/anova/beginning-to-explore-the-emmeans-package-for-post-hoc-tests-and-contrasts.html
# https://cran.r-project.org/web/packages/emmeans/vignettes/basics.html
# https://cran.r-project.org/web/packages/emmeans/vignettes/interactions.html

#
#
#

# Example data 

# Using datasets in R package "agridat". First, install the package to your computer.
#install.packages("agridat") 
library(agridat)

# Recommended example: 
data("chakravertti.factorial")
head(chakravertti.factorial)
??chakravertti.factorial

# Alternatives:

data("archbold.apple")
head(archbold.apple)
??archbold.apple

data("cochran.bib")
head(cochran.bib)
??cochran.bib

data("connolly.potato")
head(connolly.potato)
??connolly.potato

#
#
#

# example model - build a model from real data using this as a template
model <- lm(response ~ factor1 + factor2 + covariate +
              factor1:factor2 +
              factor1:covariate +
              factor2:covariate,
            data=dataset)

#
#
# Post-hoc exploration of a model

# First, check if the model says there are any differences between factor levels
anova(model)
# If there are, you can continue looking further into what these differences are,
# but if there are not - you don't need post-hoc comparisons.

#
# emmeans - short for estimated marginal means 

# For main effects
emmeans(model, ~factor1)
emmeans(model, pairwise ~ factor1)

# For interactions 

# Explore what is the difference between these two options:
emmeans(model, ~factor1|factor2)
emmeans(model, ~factor1:factor2)

# Also explore with this notation:
emmeans(model, pairwise ~ factor1|factor2)

# For covariates

# emtrends
emtrends(model, ~factor1, var="covariate")

#
#
# 

# Compact letter display (cld)

# Save the output of emmeans to an object
em_factors <- emmeans(model, pairwise ~ factor1|factor2)

# Packages needed (for cld-function)
library(multcomp)
library(multcompView)

# Create the cld-output
cld(em_factors, Letters=letters)

