# Packages
library(emmeans)

# For further information:
# https://cran.r-project.org/web/packages/emmeans/vignettes/basics.html
# https://cran.r-project.org/web/packages/emmeans/vignettes/interactions.html

# example model
model <- lm(response ~ factor1 + factor2 + covariate + 
              factor1:covariate +
              factor2:covariate,
            data=dataset)

# emmeans 
emmeans(model, ~factor1)
emmeans(model, pairwise ~ factor1)

# emtrends
emtrends(model, ~factor1, var="covariate")
