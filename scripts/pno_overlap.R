# =====================================================================
#   Script to generate the Predicted Niche Occupation Profile (PNO) 
#    and PNO overlap.
# =====================================================================
#
# Author: Keven Lima - (kevenlima1999@gmail.com)
# Date: 10/2024
#
# Description:
# This script performs the following tasks:
# 1. Loads and aligns raster data for multiple species and environmental variables.
# 2. Extracts suitability values and creates density plots (PNO) for each variable.
# 3. Computes overlap percentages between species based on suitability values.
# 4. Generates heatmaps (matrix) to visualize the overlap between species for each variable.
#
# Dependencies:
# The script requires the following files.
# - The ready-made species distribution models.
# - The environmental variables
# - (Preferably all in .tif format)
#
# Input:
# - Raster files of species distribution models.
# - Raster files of environmental variables.
#
# Output:
# - Density plots (PNO) for each environmental variable.
# - Heatmaps (matrix) showing overlap percentages between species.
# - Plots are saved in the specified output directory.
#
# Notes:
# - Ensure that all input raster files are preprocessed to match the 
#   resolution, extent, and coordinate reference system (CRS).
# - Adjust file paths and parameters as needed.
#
# =====================================================================

# Load required packages
library(raster)
library(ggplot2)
library(tidyr)
library(dplyr)
library(reshape2)
library(gridExtra)

# Function to load and align rasters
load_and_align_raster <- function(path, template) {
  r <- raster(path)
  if (!compareRaster(r, template, extent = TRUE, rowcol = TRUE, crs = TRUE, res = TRUE, stopiffalse = FALSE)) {
    r <- resample(r, template, method = "bilinear")
  }
  return(r)
}

# Paths for raster layers and variable names
paths <- list(
  Microgramma_nana = "C:/Users/PC/OneDrive/_KEVEN LIMA/Keven/new_scaly_for_overlap_bin/Threshoud_nana.tif",
  Microgramma_tecta = "C:/Users/PC/OneDrive/_KEVEN LIMA/Keven/new_scaly_for_overlap_bin/Threshoud_tecta.tif",
  Microgramma_dictyophylla = "C:/Users/PC/OneDrive/_KEVEN LIMA/Keven/new_scaly_for_overlap_bin/Threshoud_dictyophylla.tif",
  Microgramma_latevagans = "C:/Users/PC/OneDrive/_KEVEN LIMA/Keven/new_scaly_for_overlap_bin/Threshoud_latevagans.tif",
  Microgramma_percussa = "C:/Users/PC/OneDrive/_KEVEN LIMA/Keven/new_scaly_for_overlap_bin/Threshoud_percussa.tif",
  Microgramma_piloselloides = "C:/Users/PC/OneDrive/_KEVEN LIMA/Keven/new_scaly_for_overlap_bin/Threshoud_piloselloides.tif",
  Microgramma_tobagensis = "C:/Users/PC/OneDrive/_KEVEN LIMA/Keven/new_scaly_for_overlap_bin/Threshoud_tobagensis.tif",
  Microgramma_reptans = "C:/Users/PC/OneDrive/_KEVEN LIMA/Keven/new_scaly_for_overlap_bin/Threshoud_reptans.tif"
)

variables <- list(
  bio_02 = "Mean diurnal range",
  bio_03 = "Isothermality",
  bio_08 = "Mean temperature of wettest quarter",
  bio_14 = "Precipitation of driest month",
  bio_15 = "Precipitation of seasonality",
  bio_18 = "Precipitation of warmest quarter",
  bio_19 = "Precipitation of coldest quarter",
  bio_20 = "Cloud seasonality",
  bio_21 = "EVI dissimilarity",
  bio_22 = "Global elevation",
  bio_23 = "Terrain roughness"
)

# Function to create and save the PNO plot
create_pno_plot <- function(data, var, var_name, output_folder) {
  p <- ggplot(data, aes(x = bio_var, weight = Suitability, color = species)) +
    geom_density(adjust = 2.5, linewidth = 1.2) +
    labs(x = var_name, y = "Density", color = NULL, face = "bold") +
    theme_light() +
    scale_color_brewer(palette = "Set1", guide = guide_legend(override.aes = list(shape = 15))) +
    theme(legend.text = element_text(face = "italic", size = 20, family = "serif"),
          axis.text = element_text(size = 20, family = "serif"),
          axis.title = element_text(size = 25, family = "serif"))
  
  print(p)
  ggsave(file.path(output_folder, paste0("PNO_", var, ".png")), plot = p, dpi = 300, width = 10, height = 6)
}

# Function to create a folder if it doesn't exist
create_folder_if_not_exists <- function(folder) {
  if (!dir.exists(folder)) {
    dir.create(folder, recursive = TRUE)
  }
}

# Function to calculate overlap between two species
calculate_overlap <- function(data, species1, species2) {
  data_filtered <- data %>% filter(species %in% c(species1, species2))
  
  density_species1 <- density(data_filtered$bio_var[data_filtered$species == species1], 
                              weights = data_filtered$Suitability[data_filtered$species == species1], adjust = 2.5)
  
  density_species2 <- density(data_filtered$bio_var[data_filtered$species == species2], 
                              weights = data_filtered$Suitability[data_filtered$species == species2], adjust = 2.5)
  
  df_density <- data.frame(
    bio_var = density_species1$x,
    species1_density = density_species1$y,
    species2_density = density_species2$y
  )
  
  df_density$overlap <- pmin(df_density$species1_density, df_density$species2_density)
  
  auc_species1 <- sum(df_density$species1_density) * (df_density$bio_var[2] - df_density$bio_var[1])
  auc_species2 <- sum(df_density$species2_density) * (df_density$bio_var[2] - df_density$bio_var[1])
  auc_overlap <- sum(df_density$overlap) * (df_density$bio_var[2] - df_density$bio_var[1])
  
  percent_overlap <- auc_overlap / ((auc_species1 + auc_species2) / 2) * 100
  return(percent_overlap)
}

# Function to create and save the overlap plot
create_overlap_plot <- function(data, var, output_folder) {
  species_names <- unique(data$species)
  species_pairs <- combn(species_names, 2, simplify = FALSE)
  
  result_matrix <- matrix(NA, nrow = length(species_names), ncol = length(species_names), 
                          dimnames = list(species_names, species_names))
  
  for (pair in species_pairs) {
    species1 <- pair[1]
    species2 <- pair[2]
    percent_overlap <- calculate_overlap(data, species1, species2)
    result_matrix[species1, species2] <- percent_overlap
    result_matrix[species2, species1] <- percent_overlap
  }
  
  species_names <- sort(unique(data$species), decreasing = TRUE)
  result_matrix_sorted <- result_matrix[species_names, species_names]
  
  df_heatmap <- melt(result_matrix_sorted, na.rm = TRUE)
  var_name <- paste0("Bio_", var, " (%)")
  
  p_heatmap <- ggplot(df_heatmap, aes(Var1, Var2, fill = value)) +
    geom_tile(color = "white") +
    scale_fill_gradient(low = "white", high = "blue", na.value = "grey50", name = var_name) +
    geom_text(aes(label = sprintf("%.1f", value)), color = "black", size = 4) +
    labs(x = NULL, y = NULL) +
    theme_minimal() +
    theme(axis.text.x = element_text(angle = 45, hjust = 1, face = "italic", size = 20),
          axis.text.y = element_text(face = "italic", size = 20))
  
  print(p_heatmap)
  ggsave(file.path(output_folder, paste0("OVERLAP_PNO_", var, ".png")), plot = p_heatmap, dpi = 300, width = 8, height = 4)
}

# Output folder
output_folder <- "C:/Users/PC/OneDrive/_KEVEN LIMA/Keven/resultado_overlap_ono_02_10_24"
create_folder_if_not_exists(output_folder)

# Loop for overlap graphs
for (var in names(variables)) {
  paths[[var]] <- paste0("C:/Users/PC/OneDrive/_KEVEN LIMA/Keven/tif_neo_07/", var, ".tif")
  rasters <- lapply(paths, load_and_align_raster, template = raster(paths$Microgramma_nana))
  
  data <- data.frame(
    bio_var = values(rasters[[var]]),
    Microgramma_nana = values(rasters$Microgramma_nana),
    Microgramma_tecta = values(rasters$Microgramma_tecta),
    Microgramma_dictyophylla = values(rasters$Microgramma_dictyophylla),
    Microgramma_latevagans = values(rasters$Microgramma_latevagans),
    Microgramma_percussa = values(rasters$Microgramma_percussa),
    Microgramma_piloselloides = values(rasters$Microgramma_piloselloides),
    Microgramma_tobagensis = values(rasters$Microgramma_tobagensis),
    Microgramma_reptans = values(rasters$Microgramma_reptans)
  )
  
  data <- na.omit(data)
  
  data_long <- pivot_longer(data, 
                            cols = starts_with("Microgramma"), 
                            names_to = "species", 
                            values_to = "Suitability")
  
  data_long$species <- sub("Microgramma", "M.", data_long$species)
  data_long$species <- gsub("_", " ", data_long$species)
  
  create_overlap_plot(data_long, var, output_folder)
}
