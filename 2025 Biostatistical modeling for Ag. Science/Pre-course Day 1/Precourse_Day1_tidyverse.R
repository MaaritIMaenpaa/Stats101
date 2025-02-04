# # PhD Course, Biostatistical modeling in Ag. Science, Pre-course, Day 1
# Tidyverse ---- 
# Aim: Examples for using tidyverse

# Install necessary packages
# For these examples, you will need packages belonging to the same family of packages: https://www.tidyverse.org/ 
install.packages("tidyverse")

# Read in the packages ----
library(dplyr)
library(lubridate)

# This script only gives a very brief scratch of what you can do. 
# For more information and inspiration, see here: https://r4ds.hadley.nz/
# And here: https://posit.cloud/learn/recipes


# Data for examples in these scripts ----
cucumber <- read.table("data/Biostat2025_BridgesCucumber.txt", header=TRUE)


#
#
# Piping ----

# Tidyverse uses "piping" - structuring the commands and assignments in R with |> or %>% to
# essentially nest them within each other

# So this
cucumber %>%
  summary()

# does the same as 
summary(cucumber)

#
#
# Subsetting the data ----
cucumber_guardian <- cucumber %>%
  filter(gen=="Guardian")


#
#
# Changing one variable's class ----
cucumber <- cucumber %>%
  mutate(gen = as.factor(gen))

str(cucumber)

#
#
# Changing variable classes/modes in multiple variables at a time ----
cucumber <- cucumber %>%
  mutate(across(c(loc, gen), factor),
         across(c(row, col, yield), as.numeric))

str(cucumber)


#
#
# Calculating descriptive statistics ----
cucumber %>% 
  group_by(gen) %>%
  summarise(mean_value = mean(yield, na.rm = TRUE),
            standard_dev = sd(yield, na.rm=TRUE),
            sample_size = n())


#
#
# Dates ----

# Package lubridate makes dates easy/easier to handle
# Simple example with a vector of dates - for more, see: https://lubridate.tidyverse.org/

# NOTE! This is an example using a vector of dates. You will probably have your dates in a dataset,
# so in your own code, you will need to keep track of where the vector is located.
# I.e. whenever there would be a reference to "dates", you would need to use "DATA$dates" to tell R
# that your vector is in a dataframe.

# Example date strings
dates <- c("2025-02-05", 
           "2025-02-07", 
           "2025-02-10",
           "2025-02-11",
           "2025-02-12",
           "2025-02-13",
           "2025-02-14")

# Check how these dates are read
str(dates)

# turn the dates into a date-object
dates <- ymd(dates)
str(dates)

# Extract components from the dates
year <- year(dates)
month <- month(dates)
day <- day(dates)

# Calculate duration
days <- dates - min(dates)
days
