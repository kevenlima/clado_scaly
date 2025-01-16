# =====================================================================
#   Niche Overlap Analysis for Species Distribution Models (SDMs)
# =====================================================================
#
# Author: Lucas Lima - (lucaslima1618@gmail.com)
# Date: 05/2024
#
# Description:
# This script calculates the niche overlap between species distribution models 
# (SDMs). It utilizes the 'dismo' package to 
# calculate the niche overlap using the D and I statistics. The script performs 
# the following steps:
# 1. Loads the species distribution maps (raster files) for eight species.
# 2. Ensures all species rasters have the same resolution and extent.
# 3. Applies a function to convert any negative values in the rasters to zero.
# 4. Calculates the niche overlap between each pair of species using the D 
#    and I statistics.
# 5. Saves the niche overlap results to CSV files.
# 6. Optionally, visualizes the species distribution maps.
#
# Dependencies:
# Input:
# - Species distribution model (GeoTIFF format).
#
# Output:
# - Two CSV files containing the niche overlap matrices for D and I statistics.
# - Optional: Species distribution maps visualized as plots.
#
# Notes:
# - Ensure that all species distribution maps are in the same resolution and 
#   extent.
# - The function 'nicheOverlap' from the 'dismo' package is used to calculate 
#   niche overlap.
#
# =====================================================================

# Load required packages
install.packages("dismo")
install.packages("raster")
library(dismo)
library(raster)

getwd()

# Define file names for the species GeoTIFFs
species_files <- list.files(path = "E:/Modelagem/keven/niche_overlap", pattern = "\\.tif$", full.names = TRUE)

# Load species distribution maps as raster objects
species_rasters <- lapply(species_files, raster)

# Check if all rasters have the same resolution and extent
extent_list <- lapply(species_rasters, extent)
resolution_list <- lapply(species_rasters, res)

if(length(unique(extent_list)) > 1 || length(unique(resolution_list)) > 1) {
  stop("All maps must have the same resolution and extent.")
}

# Function to ensure that all values are non-negative
make_non_negative <- function(r) {
  r[r < 0] <- 0
  return(r)
}

# Apply the function to the rasters
species_rasters <- lapply(species_rasters, make_non_negative)

# Initialize matrices to store niche overlap results
overlap_D <- matrix(nrow = length(species_rasters), ncol = length(species_rasters))
overlap_I <- matrix(nrow = length(species_rasters), ncol = length(species_rasters))
colnames(overlap_D) <- rownames(overlap_D) <- basename(species_files)
colnames(overlap_I) <- rownames(overlap_I) <- basename(species_files)

# Calculate niche overlap between species for D and I statistics
for(i in 1:length(species_rasters)) {
  for(j in 1:length(species_rasters)) {
    if(i != j) {
      overlap_D[i, j] <- nicheOverlap(species_rasters[[i]], species_rasters[[j]], stat = "D")
      overlap_I[i, j] <- nicheOverlap(species_rasters[[i]], species_rasters[[j]], stat = "I")
    } else {
      overlap_D[i, j] <- NA
      overlap_I[i, j] <- NA
    }
  }
}

# Display the niche overlap matrices
print("Niche overlap matrix (D):")
print(overlap_D)

print("Niche overlap matrix (I):")
print(overlap_I)

# Save the niche overlap matrices as CSV files
write.csv(overlap_D, file = "niche_overlap_results_D.csv")
write.csv(overlap_I, file = "niche_overlap_results_I.csv")

# Plot species maps (optional)
par(mfrow = c(2, 4))
for(i in 1:length(species_rasters)) {
  plot(species_rasters[[i]], main = basename(species_files[i]))
}
