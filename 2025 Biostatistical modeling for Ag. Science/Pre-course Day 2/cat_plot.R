library(tidyverse) # This is for general housekeeping
library(ggplot2) # This is for making figures

# Run all the lines below. 

# Install necessary packages
install.packages(c("gganimate", "tidyverse"))
remotes::install_github("R-CoderDotCom/ggcats@main")

# Load in the necessary packages
library(gganimate) 
library(ggcats)
library(tidyverse)

# Create a function for creating data
data_cat <- function(from = 0, to = 80, by = 1, fun = rnorm, 
                     cat = "", sd = 3, category = "") {
  tibble(
    x = seq(from, to, by),
    y = fun(x) + rnorm(length(x), sd = sd),
    category = rep(category, length(x)),
    cat = rep(cat, length(x))
  )
}

# Create data according to a function for category 1
confusion <- data_cat(
  fun = function(x) 50 - (-20 -50)*exp(-exp(20*(40-x)/(-20-50) + 1)) + 5 * sin(x), 
  cat = "maru", 
  sd = 1, category = "Confusion"
)

# Create data according to a function for category 2
skills <- data_cat(
  fun = function(x) 0 + (90 -0)*exp(-exp(10*(20-x)/(90-0) + 1)), 
  cat = "pusheen_pc", 
  sd = 2, category = "R skills"
)

# Create data according to a function for category 3
procras <- data_cat(
  fun = function(x) 5 + exp(x/15) + 4 * sin(x), 
  cat = "nyancat", 
  sd = 1, category = "Procrastination"
)

# Combine the data
full_data <- rbind(confusion, skills, procras)

# Assign category order
full_data$category <- factor(full_data$category, levels=c("Confusion", "R skills", "Procrastination"))

# Plot
ggplot(full_data, aes(x, y)) +
  geom_line(aes(color = category), linewidth = 1) +
  geom_cat(aes(cat = cat), size = 4) +
  labs(y = "Amount perceived", x = "Time (min)") +
  scale_color_manual(values = c("#EE2C2C", "#68228B", "#FF8C00")) +
  theme_bw() +
  theme(
    axis.text.y = element_text(size = 12, color = "black"),
    axis.title.y = element_text(size = 14),
    axis.text.x = element_text(size = 12, color = "black"),
    axis.title.x = element_text(size = 14),
    legend.text = element_text(size = 14),
    legend.position = "top",
    legend.title = element_blank()
  ) +
  transition_reveal(x)

# To save plot to your own computer, remove # below 
#anim_save(filename="cats.gif")

