# =====================================================================
##           Mantel Test for Correlating Niche Overlap                 
##                 with Patristic Distance.                            
# =====================================================================
#
# Author: Keven Lima - (kevenlima1999@gmail.com)
# Date: 07/2024
#
# Introduction:
# This script performs a Mantel test to assess the correlation between
# niche overlap (D and I statistics) and phylogenetic distance (patristic 
# distance) for different species. The test is run for both D and I overlap
# statistics separately.
# The script also visualizes the relationship between niche overlap and 
# phylogenetic distance with scatter plots, adds regression lines, and 
# saves the results as images and text files.
#
# The process is as follows:
# 1. Load phylogenetic distance matrices and niche overlap matrices (for D and I).
# 2. Create scatter plots to visualize the correlation.
# 3. Perform the Mantel test to calculate correlation values.
# 4. Save the results as text files and plots.
# =====================================================================

# Install necessary packages (only if they are not installed yet)
install.packages("ape")
install.packages("vegan")
install.packages("ggplot2")
install.packages("reshape2")
install.packages("permute")
install.packages("lattice")

# Load packages
library(ape)
library(vegan)
library(ggplot2)
library(reshape2)
library(permute)
library(lattice)

#------------------------------------------------------------------------------
### --- D - Correlation
#------------------------------------------------------------------------------

# Load phylogenetic distance and niche overlap matrices
phylo_dist <- read.csv("D:/R/niche_overlap_mantel/distancia_filogenetica_mantel.csv", row.names = 1)
niche_overlap <- read.csv("D:/R/niche_overlap_mantel/overlap_mantel_d.csv", row.names = 1)

# Convert matrices to data frame format for ggplot
phylo_dist_long <- melt(as.matrix(phylo_dist))
colnames(phylo_dist_long) <- c("Species1", "Species2", "Phylo_Dist")

niche_overlap_long <- melt(as.matrix(niche_overlap))
colnames(niche_overlap_long) <- c("Species1", "Species2", "Niche_Overlap")

# Merge the long data frames by species
combined_data <- merge(phylo_dist_long, niche_overlap_long, by = c("Species1", "Species2"))

# Create scatter plot with ggplot2
p <- ggplot(combined_data, aes(x = Niche_Overlap, y = Phylo_Dist)) +
  geom_point() +
  geom_point(size = 4) +  # Adjust point size here
  geom_smooth(method = "lm", col = "blue") +  # Regression line
  labs(x = "Niche overlap",
       y = "Patristic distance") +
  theme_light() +
  theme(axis.title.x = element_text(size = 23, face = "bold", family = "serif", margin = margin(t = 8)),
        axis.title.y = element_text(size = 23, face = "bold", family = "serif", margin = margin(r = 8)),
        axis.text = element_text(size = 14),  # Axis text size
        axis.text.x = element_text(margin = margin(t = 5)),  # Adjust margin for x-axis numbers
        axis.text.y = element_text(margin = margin(r = 5))  # Adjust margin for y-axis numbers
  )

# Display the plot
print(p)

# Save the plot as a PNG image file
ggsave("01_1correlacao_D_mantel.png", plot = p, width = 6, height = 6, units = "in", dpi = 300)

### ---- MANTEL TEST RESULTS ---- ###

# Calculate the Mantel correlation
mantel_test <- mantel(as.dist(phylo_dist), as.dist(niche_overlap), method = "pearson", permutations = 999)

# Display the results of the Mantel test
print(mantel_test)

# Format results for saving in text file
r_value <- mantel_test$statistic
p_value <- mantel_test$signif
alpha <- 0.05  # Significance level (alpha)

mantel_results <- paste("r(AB) =", round(r_value, 3), "\n",
                        "p-value (Two-tailed) =", p_value, "\n",
                        "alpha =", alpha, "\n")

# Save the Mantel test results in a text file
writeLines(mantel_results, "01_resultados_D_mantel.txt")

#------------------------------------------------------------------------------
### --- I - Correlation
#------------------------------------------------------------------------------

# Load phylogenetic distance and niche overlap matrices for I
phylo_dist <- read.csv("D:/R/niche_overlap_mantel/distancia_filogenetica_mantel.csv", row.names = 1)
niche_overlap <- read.csv("D:/R/niche_overlap_mantel/overlap_mantel_I.csv", row.names = 1)

# Convert matrices to data frame format for ggplot
phylo_dist_long <- melt(as.matrix(phylo_dist))
colnames(phylo_dist_long) <- c("Species1", "Species2", "Phylo_Dist")

niche_overlap_long <- melt(as.matrix(niche_overlap))
colnames(niche_overlap_long) <- c("Species1", "Species2", "Niche_Overlap")

# Merge the long data frames by species
combined_data <- merge(phylo_dist_long, niche_overlap_long, by = c("Species1", "Species2"))

# Create scatter plot with ggplot2
p <- ggplot(combined_data, aes(x = Niche_Overlap, y = Phylo_Dist)) +
  geom_point() +
  geom_point(size = 4) +  # Adjust point size here
  geom_smooth(method = "lm", col = "blue") +  # Regression line
  labs(x = "Niche overlap",
       y = "") +
  theme_light() +
  theme(axis.title.x = element_text(size = 23, face = "bold", family = "serif", margin = margin(t = 8)),
        axis.title.y = element_text(size = 23, face = "bold", family = "serif", margin = margin(r = 8)),
        axis.text = element_text(size = 14),  # Axis text size
        axis.text.x = element_text(margin = margin(t = 5)),  # Adjust margin for x-axis numbers
        axis.text.y = element_text(margin = margin(r = 5))  # Adjust margin for y-axis numbers
  )

# Display the plot
print(p)

# Save the plot as a PNG image file
ggsave("01_correlacao_I_mantel.png", plot = p, width = 6, height = 6, units = "in", dpi = 300)

### ---- MANTEL TEST RESULTS ---- ###

# Calculate the Mantel correlation
mantel_test <- mantel(as.dist(phylo_dist), as.dist(niche_overlap), method = "pearson", permutations = 999)

# Display the results of the Mantel test
print(mantel_test)

# Format results for saving in text file
r_value <- mantel_test$statistic
p_value <- mantel_test$signif
alpha <- 0.05  # Significance level (alpha)

mantel_results <- paste("r(AB) =", round(r_value, 3), "\n",
                        "p-value (Two-tailed) =", p_value, "\n",
                        "alpha =", alpha, "\n")

# Save the Mantel test results in a text file
writeLines(mantel_results, "01_resultados_I_mantel.txt")
