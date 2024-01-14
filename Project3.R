install.packages("DatabionicSwarm")
install.packages("DataVisualizations")
library(DatabionicSwarm)
library(DataVisualizations)



file_path <- "Project3-Data/01WorldGDP70to2010lrn.sec"

# Load the data, skipping the initial comment lines
gdp_data <- read.table(file_path, skip = 4, header = TRUE, sep = "\t", fill = TRUE, check.names = FALSE)
# TEMP: Inspect the first few rows to ensure everything looks correct
head(gdp_data)

# Transpose the dataset
gdp_data_transposed <- t(gdp_data)

# Set the first row as column names
colnames(gdp_data_transposed) <- gdp_data_transposed[1,]
# Remove the first row
gdp_data_transposed <- gdp_data_transposed[-1,]
# Convert to a data frame
gdp_data_transposed <- as.data.frame(gdp_data_transposed)
# Convert columns to numeric (excluding the first column if it's non-numeric)
gdp_data_transposed[, -1] <- apply(gdp_data_transposed[, -1], 2, as.numeric)

# Apply the signed log transformation to the data (excluding the first column if it's non-numeric)
signedLog <- function(x) {
  sign(x) * log1p(abs(x))
}
gdp_data_log <- gdp_data_transposed
gdp_data_log[, -1] <- apply(gdp_data_transposed[, -1], 2, signedLog)


# Convert all columns (except the first) to numeric explicitly
for (col in 2:ncol(gdp_data_log)) {
  gdp_data_log[[col]] <- as.numeric(gdp_data_log[[col]])
}
# Initialize the distance matrix
n_countries <- nrow(gdp_data_log)
distance_matrix <- matrix(nrow = n_countries, ncol = n_countries)
# Compute distances

for (i in 1:n_countries) {
  for (j in 1:n_countries) {
    if (i != j) {
      data_i <- as.numeric(gdp_data_log[i, -1])
      data_j <- as.numeric(gdp_data_log[j, -1])
      
      # Check for NA values in data_i and data_j
      if (any(is.na(data_i)) || any(is.na(data_j))) {
        distance_matrix[i, j] <- NA  # Assign NA to distance_matrix if there are NA values
      } else {
        distance_matrix[i, j] <- msmDist(data_i, data_j, c=1)
      }
    }
  }
}


# WARNING: BROKEN AFTER THIS POINT. WORK IN PROGRESS. 
# Just testing stuff, might just delete stuff below here and restart. 

# Perform clustering using DBSclustering
k_clusters <- 5  # adjust this number based on requirement or analysis
InputDistances <- as.matrix(distance_matrix)

# Running the Pswarm projection
projection <- Pswarm(InputDistances)

# Generating the visualization 
genUmatrixList <- GeneratePswarmVisualization(Data = gdp_data_log, 
                                              ProjectedPoints = projection$ProjectedPoints, 
                                              LC = projection$LC)

# Running the DBSclustering with the required arguments
Cls <- DBSclustering(k = k_clusters, 
                     DataOrDistance = gdp_data_log,
                     BestMatches = genUmatrixList$Bestmatches, 
                     LC = genUmatrixList$LC, 
                     PlotIt = TRUE)

