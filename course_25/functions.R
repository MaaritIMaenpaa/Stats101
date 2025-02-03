
# Functions --------


# Defining a simple function to calculate SOC stock
calc_stock <- function(soc, bd, depth) {
  
  # SOC stock (tn/ha): SOC (%) * BD(g/cm3) * depth(m)* 1/10
  soc_stock <- soc * bd * depth *10/100
  
  return(soc_stock)
}

# Using the function 
calc_stock(1.7, 1.2, 20)

calc_stock(2.0, 1.3, 20)

# Defining another function to calculate SOC stock with an adjustment for rock fragments

calc_stock_adjrf <- function(soc, bd, depth, rf) {
  
  # SOC stock (tn/ha): SOC (%) * BD(g/cm3) * depth(cm)* (1-rf(decimal fraction))* 1/10
  soc_stock <- soc * bd * depth * (1-rf)*10/100
  
  return(soc_stock)
}

# Using the function
calc_stock_adjrf(1.7, 1.2, 20,.12)

calc_stock_adjrf(2.0, 1.3, 20, .12)

# Combining functions
alt_soc_calc <- function(soc, bd, depth, rf) {
  soc_t <- calc_stock(soc, bd, depth)
  soc_ad <- calc_stock_adjrf(soc, bd, depth, rf)
  return(c(soc_t, soc_ad))
}

# Using the combined function
alt_soc_calc(1.7, 1.2, 20,.12)


# Calculating SOC for both functions for multiple combinations posible values of inputs 

n=100

# Generate random variables
soc <- runif(n, .5, 12)
bd <- runif(n, 1.1, 1.6)
rf <- runif(n, .05, .3)

# GEnerate a data frame 
input <- data.frame(soc, bd, rf)

# Go by row utilizing the function and calculating soc for both functions
# using a loop for 

soc_t <- list()

soc_ad <- list()

for(i in 1:n) {
  socalt <- alt_soc_calc(soc=input$soc[i], 
                         bd=input$bd[i], 
                         depth=20, 
                         rf=input$rf[i])
  #print(socalt)
  soc_t[i]  <-  socalt[1]
  soc_ad[i]  <-  socalt[2]
}

# make a data frame with the results of the list
do.call(rbind, soc_t)

data.frame("soc_t"=do.call(rbind, soc_t),
         "soc_ad"=do.call(rbind, soc_ad))

# with apply
res_apply <- apply(input, 1, function(row) {alt_soc_calc(soc = row["soc"], 
                                           bd = row["bd"], 
                                           depth = 20, 
                                           rf = row["rf"]) }) #|> t()

colnames(res_apply) <- c("soc_t", "soc_ad")


# we don't have the rf for all rows so we can use the ifelse function to replace the NA values with 0

input$rf[sample(1:n, 10)] <- NA

res_apply <- apply(input, 1, function(row) {
  alt_soc_calc(
    soc = row["soc"],
    bd = row["bd"],
    depth = 20,
    rf = if (is.na(row["rf"])) {0} else {row["rf"]} # ifelse(is.na(row["rf"]), 0, row["rf"])
  )
}) |> t()

colnames(res_apply) <- c("soc_t", "soc_ad")


# Defining a function to read and import multiple files with a pattern 

folder_list <- list.files("O:/Tech_AGRO/Jornaer/Franca/ctool_II_hal", 
           full.names = TRUE)


out_func <- function(i) {
  
  #Read 
  Cdec <- read.csv(
    paste(folder_list[i], 
          "\\totalAmount.xls",
          sep = ""),
    sep = "\t", 
    header = TRUE
  )
  

return(Cdec)
}

list_co2_out <- lapply(1:4, out_func)

out_tbl_all<- do.call(rbind,list_co2_out)