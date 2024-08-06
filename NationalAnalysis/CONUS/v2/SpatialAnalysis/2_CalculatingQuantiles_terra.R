# rm(list = ls())

# This script:
# 1)  Converts background value of 120 in MoBI raster to NA.
#     Note that in order for this step to run, the MoBI raster needs to be at 990m resolution.
# 2)  Calculates the quantile breaks for the 3 datasets (TNC Resilient Sites, TNC Connectivity & Climate Flow, and MoBI RSR).
#     The default settings were used, but see ?quantile to understand different options for how to calculate percentiles by setting type== and integer
# 3)  Converts quantile breaks to dataframes as exports as csv files.
#     Note that the 'as.data.frame' function couldn't be thrown into quantile calculation step, otherwise the resulting df doesn't match what's printed as the raw/printed quantile outputs (see Richness as example). 

# The output quantile breaks will be used in a python script to reclassify the rasters (the rasters are too big to be reclassified in R).

# Written by Ellie Linden in September 2022.

library(terra)
library(tidyverse)

#####################
### Set Variables ###
#####################

### Set Workspaces ###
inWS <- "S:/Projects/NPCA/_FY25/Data/Intermediate/NationalAnalysis/CONUS/1_DecileCalculationInputs/"
outWS <- "S:/Projects/NPCA/_FY25/Data/Intermediate/NationalAnalysis/CONUS/2_Percentiles/HC/"

### Import Rasters ###
ResilientSites <- terra::rast(str_c(inWS, "ResilientSites_null.tif"))
ConnectivityClimateFlow <- terra::rast(str_c(inWS, "ConnectivityClimateFlow_30m.tif"))
#RSR <- terra::rast(str_c(inWS, "RSR_30m.tif")) # large value conversion wouldn't work with 30m raster
#RSR_990m <- terra::rast("S:/Data/NatureServe/Species_Distributions/MoBI_HabitatModels/April2021/RSR_All.tif")
RSR_330m <- terra::rast(str_c(inWS, "mobi_rsr.tif"))

##########################
### Check/Prep Rasters ###
##########################

print(global(RSR_330m, c("min", "max", "mean"), na.rm = TRUE))

### Checking Rasters ###
plot(ResilientSites) # got error when tried to do this (possibly because of the raster size)
plot(ConnectivityClimateFlow)
plot(RSR_330m)

###################################
### Calculate Percentile Breaks ###
###################################

## this method below tests creating a random sample of each raster to perform the calculations on
thresholds<-c(0.2, 0.4, 0.6, 0.8)

# Function to take a random sample of the raster values
sample_raster_values <- function(raster, sample_size) {
  n_cells <- ncell(raster)
  sample_indices <- sample(n_cells, size = sample_size, replace = FALSE)
  sampled_values <- values(raster)[sample_indices]
  return(sampled_values)
}

# Calculate quantiles based on a random sample
calculate_sampled_quantiles <- function(raster, probs, sample_size = 1e6) {
  sampled_values <- sample_raster_values(raster, sample_size)
  sampled_values <- na.omit(sampled_values) # Remove NA values
  final_quantiles <- quantile(sampled_values, probs = probs, na.rm = TRUE)
  return(final_quantiles)
}

# Run the calculations
sample_size <- 1e5  # You can adjust this sample size based on available memory

ResilientSites_percentiles <- calculate_sampled_quantiles(ResilientSites, thresholds, sample_size)
print("Resilient Site percentiles calculated")

ConnectivityClimateFlow_percentiles <- calculate_sampled_quantiles(ConnectivityClimateFlow, thresholds, sample_size)
print("Connectivity & Climate Flow percentiles calculated")

RSR_percentiles <- calculate_sampled_quantiles(RSR_330m, thresholds, sample_size)
print("RSR percentiles calculated")


### this method below testing chunking the rasters to manage memory issues in order to calculate the thresholds
# 
# # Custom function to calculate quantiles for each chunk
# chunk_quantiles <- function(chunk, probs) {
#   chunk <- as.vector(chunk) # Convert matrix to vector
#   chunk <- na.omit(chunk) # Remove NA values
#   if (length(chunk) == 0) {
#     return(rep(NA, length(probs))) # Return NA if chunk has no valid values
#   }
#   quantiles <- quantile(chunk, probs = probs, na.rm = TRUE)
#   return(as.numeric(quantiles))
# }
# 
# # Calculate quantiles using terra::app for chunk processing
# calculate_chunked_quantiles <- function(raster, probs) {
#   # Process in chunks and get quantiles for each chunk
#   quantiles_list <- terra::app(raster, fun = function(x) {
#     result <- chunk_quantiles(x, probs)
#     if (length(result) != length(probs)) {
#       stop("chunk_quantiles did not return the expected number of values")
#     }
#     return(result)
#   }, cores = 1)
#   
#   # Combine the results from all chunks
#   combined_values <- as.vector(quantiles_list)
#   
#   # Calculate the final quantiles across all combined values
#   final_quantiles <- quantile(combined_values, probs = probs, na.rm = TRUE)
#   return(final_quantiles)
# }
# 
# ResilientSites_percentiles <- calculate_chunked_quantiles(ResilientSites, thresholds)
# print("Resilient Site percentiles calculated")
# 
# ConnectivityClimateFlow_percentiles <- calculate_chunked_quantiles(ConnectivityClimateFlow, thresholds)
# print("Connectivity & Climate Flow percentiles calculated")
# 
# RSR_percentiles <- calculate_chunked_quantiles(RSR_330m, thresholds)
# print("RSR percentiles calculated")

####################################
### Convert to Dataframes/Export ###
####################################

### Resilient Sites ###
ResilientSites_percentiles_df <- as.data.frame(ResilientSites_percentiles)
write.csv(ResilientSites_percentiles_df, str_c(outWS, "ResilientSites_percentiles.csv"))

### Connectivity & Climate Flow ###
ConnectivityClimateFlow_percentiles_df <- as.data.frame(ConnectivityClimateFlow_percentiles)
write.csv(ConnectivityClimateFlow_percentiles_df, str_c(outWS, "ConnectivityClimateFlow_percentiles.csv"))

### RSR ###
RSR_percentiles_df <- as.data.frame(RSR_percentiles)
write.csv(RSR_percentiles_df, str_c(outWS, "RSR_percentiles.csv"))

print("Script complete")
