# Soil Organic Carbon (SOC) Stock Calculation -----

# Function to calculate SOC stock (tn/ha)
#Functions can be broken down into three components: 
#arguments, body, and environment.
#

calc_stock <- function(soc, bd, depth) { # formals(), the list of arguments that control how you call the function.
  
  # SOC stock formula: SOC (%) * BD (g/cm3) * depth (cm) * 10/100
  soc_stock <- soc * bd * depth * 10 / 100 #body(), the code inside the function.
  
  
  
  return(soc_stock)
}

# Example usage of calc_stock function
calc_stock(1.7, 1.2, 20)
calc_stock(2.0, 1.3, 20)

# explore the function 

formals(calc_stock)

body(calc_stock)

environment(calc_stock)

# SOC Stock Calculation with Rock Fragment Adjustment -----

# Function to calculate SOC stock considering rock fragments (rf)
calc_stock_adjrf <- function(soc, bd, depth, rf) {
  
  # Adjusted SOC stock formula: SOC (%) * BD (g/cm3) * depth (cm) * (1 - rf) * 10/100
  soc_stock <- soc * bd * depth * (1 - rf) * 10 / 100
  
  return(soc_stock)
}

# Example usage of calc_stock_adjrf function
calc_stock_adjrf(1.7, 1.2, 20, 0.12)
calc_stock_adjrf(2.0, 1.3, 20, 0.12)

# Combining Functions for SOC Stock Calculation ----

# Function to compute SOC stock with and without rock fragment adjustment
alt_soc_calc <- function(soc, bd, depth, rf) {
  soc_t <- calc_stock(soc, bd, depth)  # SOC stock without adjustment
  soc_ad <- calc_stock_adjrf(soc, bd, depth, rf)  # SOC stock with rock fragment adjustment
  return(c(soc_t, soc_ad))
}

# Example usage of alt_soc_calc function
alt_soc_calc(1.7, 1.2, 20, 0.12)

# Bulk Calculation of SOC Stock for Multiple Values ----

# Define sample size
n <- 100

# Generate random values for SOC, bulk density (BD), and rock fragments (RF)
soc <- runif(n, 0.5, 12)
bd <- runif(n, 1.1, 1.6)
rf <- runif(n, 0.05, 0.3)

# Create a data frame with input values
input <- data.frame(soc, bd, rf)

# Initialize lists to store results
soc_t <- list()
soc_ad <- list()

# Loop through each row to compute SOC stock
for (i in 1:n) {
  socalt <- alt_soc_calc(soc = input$soc[i], 
                         bd = input$bd[i], 
                         depth = 20, 
                         rf = input$rf[i])
  
  soc_t[i] <- socalt[1]  # Store SOC stock without adjustment
  soc_ad[i] <- socalt[2]  # Store SOC stock with adjustment
}

# Convert results to a data frame
results_df <- data.frame(
  "soc_t" = do.call(rbind, soc_t),
  "soc_ad" = do.call(rbind, soc_ad)
)

# Using apply() Function for Vectorized Calculation ----

# Alternative approach using apply() function
res_apply <- apply(input, 1, function(row) {
  alt_soc_calc(soc = row["soc"], 
               bd = row["bd"], 
               depth = 20, 
               rf = row["rf"]) 
}) |> t()

# Assign column names
colnames(res_apply) <- c("soc_t", "soc_ad")

# Handling missing values in rock fragments (RF)
input$rf[sample(1:n, 10)] <- NA  # Introduce NA values in random rows

# Recalculate with missing RF values replaced by 0
res_apply <- apply(input, 1, function(row) {
  alt_soc_calc(
    soc = row["soc"],
    bd = row["bd"],
    depth = 20,
    rf = ifelse(is.na(row["rf"]), 0, row["rf"])  # Replace NA with 0
  )
}) |> t()

# Assign column names
colnames(res_apply) <- c("soc_t", "soc_ad")

# Importing Multiple Files from a Directory ----

# Define folder path (modify as needed)
folder_list <- list.files("O:/Tech_AGRO/Jornaer/Franca/ctool_II_hal", 
                          full.names = TRUE)

# Function to read and import data from files
out_func <- function(i) {
  
  # Read data from the file
  Cdec <- read.csv(
    paste(folder_list[i], "\\totalAmount.xls", sep = ""),
    sep = "\t", 
    header = TRUE
  )
  
  return(Cdec)
}

# Apply function to import first 4 files
list_co2_out <- lapply(1:4, out_func)

# Combine results into a single data frame
out_tbl_all <- do.call(rbind, list_co2_out)
