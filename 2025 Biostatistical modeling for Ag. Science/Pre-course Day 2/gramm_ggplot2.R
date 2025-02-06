# Data Analysis and Visualization with Edlinger 2022 Dataset--------

## Load Required Packages ----
library(tidyverse)  # Data wrangling and visualization
library(readxl)  # Read Excel files
library(ggpubr)  # Arrange multiple plots
library(eurostat)  # European geospatial data
library(sf)  # Spatial data handling

## Step 1: Load the Dataset ----
# Reference: https://onlinelibrary.wiley.com/doi/10.1111/gcb.16677
# Download the dataset and save it as 'mwd_soc_diggdeeper.xlsx'

# Load the dataset into R
edlinger_data <- readxl::read_excel("example_data/mwd_soc_diggdeeper.xlsx", 
                                    skip = 1,   # Skip the first row
                                    na = "NA")  # Treat "NA" as missing values

# Inspect the dataset
head(edlinger_data)  # View the first few rows
str(edlinger_data)  # Display structure of dataset
summary(edlinger_data)  # Summary statistics of all columns
sum(is.na(edlinger_data))  # Count missing values

## Step 2: Data Visualization with ggplot2 ------

# Scatter Plot: Rough map of the data points
ggplot(data = edlinger_data, aes(x = Lat, y = Long)) +
  geom_point() +
  labs(title = "Rough Map of the Data",
       x = "Longitude",
       y = "Latitude")

# Boxplot: MWD across different land uses
ggplot(data = edlinger_data, aes(x = Land_use, y = MWD)) +
  geom_boxplot() +
  labs(title = "Boxplot of MWD by Land Use")

# Bar Plot: Count of samples per land use type
ggplot(data = edlinger_data, aes(x = Land_use)) +
  geom_bar() +
  labs(title = "Distribution of Land Use Types",
       x = "Land Use",
       y = "Count")

# Histogram: Distribution of MWD values
ggplot(data = edlinger_data, aes(x = MWD)) +
  geom_histogram(binwidth = 5) +
  labs(title = "Histogram of MWD",
       y = "Count")

# Density Plot: MWD distribution by Land Use
ggplot(data = edlinger_data, aes(x = MWD, fill = Land_use)) +
  geom_density(alpha = 0.5) +
  labs(title = "Density Plot of MWD by Land Use",
       y = "Density")


## Step 3: Customizing Themes ---------


# Scatter Plot with Minimal Theme
#edlinger_plot
ggplot(data = edlinger_data, aes(x = Lat, y = Long)) +
  geom_point() + 
  theme_minimal()

# Save plot as a PNG file
ggsave("edlinger_plot.png", edlinger_plot)


## Step 4: Faceting ---------------------

# Boxplot faceted by country
ggplot(data = edlinger_data, aes(x = Land_use, y = MWD)) +
  geom_boxplot() +
  facet_wrap(~Country)  # Create separate plots for each country

## Step 5: Adding Trend Lines ----

# Scatter plot with linear regression line
ggplot(data = edlinger_data, aes(x = Clay, y = MWD, col = Land_use)) +
  geom_point() +
  geom_smooth()  # Add a linear regression line #method = "lm"

## Step 6: Creating Multiple Panels with ggpubr --------

# Arrange multiple plots into a single panel
#combined_plot <- 
ggarrange(p1, p2, 
          nrow = 2,  # Two rows
          heights = c(2, 1))  # Different heights for plots

# Save the combined plot
ggsave("combined_plot.png", combined_plot)

## Step 7: Mapping Data on a European Map-----------

# Load European shapefile
eu_sh <- 
  get_eurostat_geospatial(resolution = 10, nuts_level = 0)

# Convert dataset to a spatial format (sf)
edlinger_data_sf <- edlinger_data |> 
  st_as_sf(coords = c("Long", "Lat"), remove = FALSE, crs = 4326)

# Basic map of Europe with data points
eu_sh |> 
  ggplot() +
  geom_sf() +
  scale_x_continuous(limits = c(-10, 35)) +
  scale_y_continuous(limits = c(35, 65)) +
  geom_sf(data = edlinger_data_sf, aes(color = MWD))

## Step 8: Advanced Mapping for a Stylish Visualization -----

# "Friday Night Crazy" Version 🎨
ggplot(eu_sh) +
  geom_sf(fill = "black", color = "white") +
  scale_x_continuous(limits = c(-10, 35)) +
  scale_y_continuous(limits = c(35, 65)) +
  geom_sf(data = edlinger_data_sf, 
          aes(color = MWD, size = SOC, shape = Land_use),
          alpha = 0.7) +
  scale_color_gradientn(colors = rainbow(10)) +
  geom_point(data = edlinger_data_sf,
             aes(Long, Lat, color = Aridity, size = MAP),
             alpha = 0.5) +
  geom_text(data = edlinger_data_sf,
            aes(Long, Lat, label = Sample_ID, angle = runif(nrow(edlinger_data_sf), 0, 360)),
            size = 2, color = "white") +
  theme_void() +
  theme(plot.background = element_rect(fill = "black"),
        legend.position = "none")

# Alternative Mapping Styles

# DeepSeek Version
eu_sh |>
  ggplot() +
  geom_sf() +
  scale_x_continuous(limits = c(-10, 35)) +
  scale_y_continuous(limits = c(35, 65)) +
  geom_sf(data = edlinger_data_sf, aes(color = MWD), size = 3) + 
  geom_sf_text(data = edlinger_data_sf,
               aes(label = Sample_ID),
               color = "red",
               size = 2,
               check_overlap = TRUE) +  # Avoid overlapping text
  scale_color_gradientn(colors = c("purple", "orange", "cyan", "pink")) +  
  theme_minimal() +
  theme(panel.background = element_rect(fill = "black")) +
  scale_fill_viridis_c(option = "magma") +
  theme_dark()

# Gemini Version
eu_sh |>
  ggplot() +
  geom_sf() +
  scale_x_continuous(limits = c(-10, 35)) +
  scale_y_continuous(limits = c(35, 65)) +
  geom_sf(data = edlinger_data_sf, 
          aes(color = MWD), 
          size = 1 + edlinger_data_sf$SOC / 10) + 
  scale_color_gradient(low = "blue", high = "red") + 
  theme_cleveland() +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank()
  )

# Additional Resources -------------
# 🤗
# Image Data Analysis: https://dahtah.github.io/imager/imager.html
# Time Series Analysis in R:
#   - https://www.r-bloggers.com/2024/09/mastering-date-and-time-data-in-r-with-lubridate/
#   - https://r4ds.had.co.nz/dates-and-times.html
