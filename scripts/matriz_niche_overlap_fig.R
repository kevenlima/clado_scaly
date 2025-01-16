# =====================================================================
#     Code to Create D and I Niche Overlap Matrices                  
#      D in the lower diagonal and I in the upper diagonal.             
#                                                                    
#    After generating these matrices, a figure will be created in GIMP 
#    (https://www.gimp.org/) by combining the two matrices.           
#                                                                    
# =====================================================================
#
# Author: Keven Lima - (kevenlima1999@gmail.com)
# date: 16-09-2024    
#
# Description:
# This script creates niche overlap matrices (D and I) for species,
# with D placed in the lower diagonal and I placed in the upper diagonal.
# 
# The following steps are performed:
# 1. Load the niche overlap data (D and I matrices) from CSV files.
# 2. Convert the data into matrices and format the species names.
# 3. Plot the matrices using the 'corrplot' function, applying a custom color palette.
# 4. Save the resulting plots as PNG files.
# 5. The final figure, combining both matrices, will be created in GIMP.
# =====================================================================

# Install necessary packages (only if not installed yet)
install.packages("ggplot2")
install.packages("ggcorrplot")
install.packages("corrplot")
install.packages("RColorBrewer")

# Load packages
library(ggplot2)
library(ggcorrplot)
library(corrplot)
library(RColorBrewer)

# =====================================================================

getwd()
# Load data from the CSV file
dados <- read.csv("D:/R/new_overlap/02/niche_overlap_results_D_01.csv", row.names = 1)

# Convert data to matrix
matriz <- as.matrix(dados)

# Add "M. " before each row and column name
rownames(matriz) <- paste("M.", rownames(matriz), sep = " ")
colnames(matriz) <- paste("M.", colnames(matriz), sep = " ")

# Create a custom color palette from red to blue
# minha_paleta <- colorRampPalette(c("red", "blue"))(100)  # 100 is the number of colors in the palette

#---

# Create a PiYG color palette with 100 colors
minha_paleta <- colorRampPalette(brewer.pal(11, "PuBu"))(100)

# Set global font family
par(family = "serif", font = 3)  # Replace "serif" with your desired font family

# Adjust margins to ensure the matrix fits within a square
# par(mar = c(5.1, 4.1, 4.1, 2.1))  # Adjust margins in all directions

# Plot the matrix with the custom color palette and species names on the sides
corrplot(matriz, is.corr = FALSE, col.lim = c(0, 1), method = 'color', 
         col = minha_paleta, cl.pos = 'r', addgrid.col = 'gray', addCoef.col = 'black',
         tl.pos = 'lt', tl.col = 'black', tl.cex = 1.4, cl.cex = 1.4, number.cex = 1.3,
         type = 'lower')

# Rotate the labels diagonally with "tl.srt = 45"

#---

# Save the plot as a PNG file
# Replace "path/to/your/file/filename.png" with the desired file path and name
png("D:/R/new_overlap/02/niche_overlap_fig_D.csv.png", width = 8, height = 8, units = "in", res = 300)

# Reproduce the plot to save it in the file
# Plot the matrix with the custom color palette and species names on the sides
par(family = "serif", font = 3)  # Replace "serif" with your desired font family
corrplot(matriz, is.corr = FALSE, col.lim = c(0, 1), method = 'color', 
         col = minha_paleta, cl.pos = 'r', addgrid.col = 'gray', addCoef.col = 'black',
         tl.pos = 'lt', tl.col = 'black', tl.cex = 1.4, cl.cex = 1.4, number.cex = 1.3,
         type = 'lower')

# Finalize the file recording
dev.off()

# =====================================================================

# Load data from the CSV file for I
dados <- read.csv("D:/R/new_overlap/02/niche_overlap_results_I_0.csv", row.names = 1)

# Convert data to matrix
matriz <- as.matrix(dados)

# Add "M. " before each row and column name
rownames(matriz) <- paste("M.", rownames(matriz), sep = " ")
colnames(matriz) <- paste("M.", colnames(matriz), sep = " ")

# Create a custom color palette from red to blue
# minha_paleta <- colorRampPalette(c("red", "blue"))(100)  # 100 is the number of colors in the palette

#---

# Create a PiYG color palette with 100 colors
minha_paleta <- colorRampPalette(brewer.pal(11, "PuBu"))(100)

# Set global font family
par(family = "serif", font = 3)  # Replace "serif" with your desired font family

# Adjust margins to ensure the matrix fits within a square
# par(mar = c(5.1, 4.1, 4.1, 2.1))  # Adjust margins in all directions

# Plot the matrix with the custom color palette and species names on the sides
corrplot(matriz, is.corr = FALSE, col.lim = c(0, 1), method = 'color', 
         col = minha_paleta, cl.pos = 'r', addgrid.col = 'gray', addCoef.col = 'black',
         tl.pos = 'lt', tl.col = 'black', tl.cex = 1.4, cl.cex = 1.4, number.cex = 1.3,
         type = 'upper')

# Rotate the labels diagonally with "tl.srt = 45"

#---

# Save the plot as a PNG file
# Replace "path/to/your/file/filename.png" with the desired file path and name
png("D:/R/new_overlap/02/niche_overlap_fig_I.csv.png", width = 8, height = 8, units = "in", res = 300)

# Reproduce the plot to save it in the file
# Plot the matrix with the custom color palette and species names on the sides
par(family = "serif", font = 3)  # Replace "serif" with your desired font family
corrplot(matriz, is.corr = FALSE, col.lim = c(0, 1), method = 'color', 
         col = minha_paleta, cl.pos = 'r', addgrid.col = 'gray', addCoef.col = 'black',
         tl.pos = 'lt', tl.col = 'black', tl.cex = 1.4, cl.cex = 1.4, number.cex = 1.3,
         type = 'upper')

# Finalize the file recording
dev.off()