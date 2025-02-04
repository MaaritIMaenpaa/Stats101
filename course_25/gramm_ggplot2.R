# Packages
library(tidyverse)

#reference data

#article
# https://onlinelibrary.wiley.com/doi/10.1111/gcb.16677


# Step 1: Load the Dataset
# Download the dataset from the provided link and save it as 'Edlinger2022.csv'
# Load the dataset into R
edlinger_data <- readxl::read_excel("example_data/mwd_soc_diggdeeper.xlsx", # Replace with the correct path to your dataset
                                    skip = 1,
                                    na="NA") 
# Inspect the dataset
head(edlinger_data)  # View the first few rows of the dataset

# Step 2: Basic Data Exploration
# Check the structure of the dataset
str(edlinger_data)  # Display the structure of the dataset

# Summary statistics
summary(edlinger_data)  # Get summary statistics for each column

# Check for missing values
sum(is.na(edlinger_data))  # Count the number of missing values in the dataset

# Step 3: Data Visualization with ggplot2
# Replace 'variable1', 'variable2', 'factor_variable', and 'numeric_variable' with actual column names from your dataset

# Scatter Plot
ggplot(data = edlinger_data, aes(x = Lat, y = Long)) +
  geom_point() +  # Create a scatter plot
  labs(title = "rough map of the data",
       x = "Longitude",
       y = "Latitude")

# Boxplot
ggplot(data = edlinger_data, aes(x = Land_use, y = MWD)) +
  geom_boxplot() +  # Create a boxplot
  labs(title = "Boxplot of Numeric Variable by Factor Variable")

# Bar Plot
ggplot(data = edlinger_data, aes(x = Land_use)) +
  geom_bar() +  # Create a bar plot
  labs(title = "Bar Plot of Factor Variable",
       x = "Factor Variable",
       y = "Count")

# Histogram
ggplot(data = edlinger_data, aes(x = MWD)) +
  geom_histogram() +  # Create a histogram with a binwidth of 5
  labs(title = "Histogram",
       y = "Count")

# Density Plot
ggplot(data = edlinger_data, aes(x = MWD, fill = Land_use)) +
  geom_density(alpha = 0.5) +  # Create a density plot with transparency
  labs(title = "Density Plot of Numeric Variable by Factor Variable",
       y = "Density")

# Step 4: Customizing Themes
# Customizing themes to change the appearance of plots
ggplot(data = edlinger_data, aes(x = Lat, y = Long)) +
  geom_point() + 
  theme_minimal()


ggsave("edlinger_plot.png", edlinger_plot)  # Save the plot as a PNG file

# Step 6: Faceting
# Faceting the plot by a factor variable to create multiple plots
ggplot(data = edlinger_data, aes(x = Land_use, y = MWD)) +
  geom_boxplot() +
  facet_wrap(~Country)  # Create separate plots for each level of the factor variable

# Step 7: Adding Smooth Lines
# Adding a smooth line to the scatter plot to visualize trends
ggplot(data = edlinger_data, aes(x = Clay, y = MWD, col = Land_use)) +
  geom_point() +
  geom_smooth(method = "lm")  # Add a linear regression line

# Step 8: Building a panels of plots with ggplot2 and ggpubr

library(ggpubr)
#combined_plot.png
ggarrange(p1,p2,nrow = 2
          , heights = c(2, 1)
          ) # Arrange the plots in a grid with 2 rows of equal widths and different heights


# Step X: Saving Plots
ggsave("combined_plot.png", combined_plot)  # Save the combined plot as a PNG file


# A map of europ with the data points
library(giscoR)
library(sf)

eu_sh <- get_eurostat_geospatial(resolution = 10, 
                        nuts_level = 0)

class(eu_sh)

eu_sh |> 
  ggplot()+
  geom_sf()+
  scale_x_continuous(limits = c(-10, 35)) +
  scale_y_continuous(limits = c(35, 65)) 

edlinger_data_sf <- edlinger_data |> 
  st_as_sf(coords = c("Long", "Lat"), remove=FALSE, crs = 4326)


eu_sh |> 
  ggplot()+
  geom_sf()+
  scale_x_continuous(limits = c(-10, 35)) +
  scale_y_continuous(limits = c(35, 65)) +
  geom_sf(data = edlinger_data_sf, aes(color = MWD))

# Lets get crazy it is Friday after all 

# what Chat GTP propose
ggplot(eu_sh) +
  geom_sf(fill = "black", color = "white") +
  scale_x_continuous(limits = c(-10, 35)) +
  scale_y_continuous(limits = c(35, 65)) +
  geom_sf(data = edlinger_data_sf,
          aes(
            color = MWD,
            size = SOC,
            shape = Land_use
          ),
          alpha = 0.7) +
  scale_color_gradientn(colors = rainbow(10)) +
  geom_point(data = edlinger_data_sf,
             aes(Long, Lat, color = Aridity, size = MAP),
             alpha = 0.5) +
  geom_text(
    data = edlinger_data_sf,
    aes(Long, Lat, label = Sample_ID, angle = runif(nrow(edlinger_data_sf), 0, 360)),
    size = 2,
    color = "white"
  ) +
  theme_void() +
  theme(plot.background = element_rect(fill = "black"),
        legend.position = "none")

# deepseek

eu_sh |>
  ggplot() +
  geom_sf() +
  scale_x_continuous(limits = c(-10, 35)) +
  scale_y_continuous(limits = c(35, 65)) +
  geom_sf(data = edlinger_data_sf, aes(color = MWD), size = 3) + geom_sf_text(
    data = edlinger_data_sf,
    aes(label = Sample_ID),
    # Add sample IDs as text
    color = "red",
    size = 2,
    check_overlap = TRUE          # Avoid overlapping text
  ) +
  scale_color_gradientn(colors = c("purple", "orange", "cyan", "pink")) +  # Custom color gradient
  theme_minimal() +
  theme(panel.background = element_rect(fill = "black")) + # Black background for contrast
  scale_fill_viridis_c(option = "magma") +
  theme_dark()

# Gemini 
eu_sh |>
  ggplot() +
  geom_sf() +
  scale_x_continuous(limits = c(-10, 35)) +
  scale_y_continuous(limits = c(35, 65)) +
  geom_sf(data = edlinger_data_sf, 
          aes(color = MWD), 
          size = 1 + edlinger_data_sf$SOC/10) + 
  scale_color_gradient(low = "blue", high = "red") + 
  theme_cleveland() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  ) 


# You?

# Image data analysis https://dahtah.github.io/imager/imager.html
