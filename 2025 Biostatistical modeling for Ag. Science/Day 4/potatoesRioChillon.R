# Load necessary libraries
library(tidyverse)
library(readxl)
library(lme4)
library(agricolae)

# If you experience some problems and you are working with the updated version 
# you can use the following when installing lme4
# remove.packages("Matrix")
# remove.packages("lme4")
# install.packages("lme4", type = "source")
# library(lme4)

# Analysis of potatoes in Peru using the Mother/Baby Trial Design
data(RioChillon)

# Let's first explore the metadata
?RioChillon

# For this example, we will work with the babies data
data_g <- RioChillon$babies

head(data_g)
str(data_g)

# We first perform a fixed effect model to evaluate if the environment represented 
# by the farmer has an effect on yield
mod_fix <- lm(yield ~ farmer, data = data_g)

summary(mod_fix)
anova(mod_fix)

# Now let's fit a mixed model with a random effect on the average 
mod_ran <- lmer(yield ~ farmer + (1 | clon), data = data_g)

summary(mod_ran)

# Obtaining the BLUPs from the fitted model
ranef(mod_ran)

# We can also generate a data frame to work further with 
blups_babies <- ranef(mod_ran)$clon |> # Select the element
  rownames_to_column("clon") |> # Generate a new variable with the row names which in our case are the clones
  as.data.frame() |> # Convert to data frame
  rename("yield_blup_babies" = `(Intercept)`) # Rename the variable to a more convenient name

# Exploring the distribution 
blups_babies |> ggplot(aes(x = yield_blup_babies)) + 
  geom_density() +
  theme_bw()

# We perform a bar plot with the ranking of BLUPs 
blups_babies |> ggplot(aes(x = reorder(clon, yield_blup_babies), # Define the clones in x axis but reordered by the BLUP value
                           y = yield_blup_babies, 
                           fill = yield_blup_babies)) + 
  geom_col() +
  labs(x = "Clon", y = "Yield BLUPs (Babies)") +
  theme_bw()

## Activity ----
## Work on the model with the mother data fitting a mixed model with a random effect on the average 
## Extract the BLUPs and make a table with the ranking of the clones based on the BLUPs
## Then copy the BLUPs estimations of the babies and mothers and compare them in a plot and explain what this means agronomicaly 

# For this example, we will work with the mother data
data_m <- RioChillon$mother

