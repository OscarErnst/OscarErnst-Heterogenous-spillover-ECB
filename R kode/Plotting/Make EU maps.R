rm(list = ls())  

# Load necessary packages
library(giscoR)
library(sf)

setwd("/Users/OscarEAM/Library/CloudStorage/OneDrive-UniversityofCopenhagen/Økonomi - Kandidat/Advanced Macroeconomics - Empirical Analysis")  

# Define country groups
eurozone_countries <- c("AT", "BE", "CY", "EE", "FI", "FR", "DE", "GR", "IE", "IT", "LV",  
                        "LT", "LU", "MT", "NL", "PT", "SK", "SI", "ES")  

erm_ii_countries <- c("BG", "HR")  # Pre-members in ERM II
non_euro_eu <- c("CZ", "HU", "PL", "RO", "SE")  # EU but not using Euro
fixed_exchange_countries <- c("DK")  # Countries with fixed exchange rate to the Euro

# Load European countries from GISCO
europe <- gisco_get_countries(resolution = "3")  
europ <- europe %>% filter(nam)
# Define bounding box to focus on Europe
europe_bbox <- st_bbox(c(xmin = -25, xmax = 35, ymin = 33, ymax = 72), crs = st_crs(europe))  
europe <- st_crop(europe, europe_bbox)  

# Filter for specific groups
eurozone <- europe[europe$CNTR_ID %in% eurozone_countries, ]  
erm_ii <- europe[europe$CNTR_ID %in% erm_ii_countries, ]  
non_euro <- europe[europe$CNTR_ID %in% non_euro_eu, ]  
fixed_exchange <- europe[europe$CNTR_ID %in% fixed_exchange_countries, ]  

# Save as PNG
png("Graphs/EU_Map_2.png", width = 34, height = 31, units = "cm", res = 600)  

# Base plot
plot(st_geometry(europe), col = "lightgray", border = "black", main = "The Eurozone and Fixed-Exchange Countries")  

# Add country groups with distinct colors
plot(st_geometry(eurozone), col = "#003399", border = "black", add = TRUE)  # EU Dark Blue  
plot(st_geometry(erm_ii), col = "lightpink", border = "black", add = TRUE)  
plot(st_geometry(non_euro), col = "lightgray", border = "black", add = TRUE)  
plot(st_geometry(fixed_exchange), col = "lavender", border = "black", add = TRUE)  # Denmark in lavender  

# Add legend
legend("topright", legend = c("Eurozone", "ERM II (pre-member)", "Non-euro",  
                              "Fixed Exchange Rate to Euro"),  
       fill = c("#003399", "lightpink", "lightgray", "lavender"),  # EU Dark Blue for Eurozone  
       border = "black", cex = 0.9)  

# Close PNG device
dev.off()  
