
# Install the dplyr package if not installed
if (!requireNamespace("dplyr", quietly = TRUE)) {
  install.packages("dplyr")
}
library(dplyr)

# Install the TSAT package if not installed
if (!requireNamespace("TSAT", quietly = TRUE)) {
  install.packages("remotes")
  remotes::install_github("Mthrun/TSAT")
}

library(TSAT)

# Read the time series data from file using the ReadTS function
# Make sure to change the original filename to "Energy_blend_rawdata2784724.ts"
# Change the name of your directory
ur_file_dir <- "C:/Users/elmop/Downloads"
File <- ReadTS(FileName = "Energy_blend_rawdata2784724", InDirectory = ur_file_dir)

# Combine 'Series' and 'FurtherTexts' from File into a single dataframe
combined_data <- as.data.frame(cbind(File$Series, File$FurtherTexts))

# Process filtered_data by arranging and summarizing
filtered_data <- combined_data %>%
  
  # Sort the data by UnixTime in ascending order. If there are duplicates,
  # sort these by EntryID in descending order, then by TimeInserted in descending order.
  arrange(UnixTime, desc(EntryID), desc(TimeInserted)) %>%
  
  # Group the data by UnixTime to process each time point separately.
  group_by(UnixTime) %>%
  
  # Create a summary for each group (each unique UnixTime).
  summarise(
  
      # Determine the Price for each UnixTime.
    
    # If there's only one entry or all entries have the same price, use the first price.
    Price = ifelse(n() == 1 || length(unique(Price)) == 1,
                   
                   first(Price),
                   
                   # If all EntryID values are NA or the same, then check TimeInserted.
                   ifelse(all(is.na(EntryID)) || length(unique(EntryID)) == 1,
                          
                          # If all TimeInserted values are NA or the same, then check price differences.
                          ifelse(all(is.na(TimeInserted)) || length(unique(TimeInserted)) == 1,
                                 
                                 # If the price difference is less than 0.05, calculate the mean; otherwise, set as NA.
                                 ifelse(abs(diff(Price)) < 0.05, 
                                        mean(Price, na.rm = TRUE), 
                                        NA
                                        ),
                                 
                                 # Select the first non-NA price.
                                 first(Price[!is.na(Price)])
                                 ),
                          
                          # Select the first non-NA price if the EntryID check doesn't lead to a conclusion.
                          first(Price[!is.na(Price)])
                          )
                   ),
    
    # Select the first non-NA EntryID for each UnixTime. If all are NA, set as NA.
    EntryID = ifelse(all(is.na(EntryID)), 
                     NA, 
                     first(EntryID[!is.na(EntryID)])
                     ),
    
    # Select the first non-NA TimeInserted for each UnixTime. If all are NA, set as NA.
    TimeInserted = ifelse(all(is.na(TimeInserted)), 
                          NA, 
                          first(TimeInserted[!is.na(TimeInserted)])
                          ),
    
    # Select the first non-NA Volume for each UnixTime. If all are NA, set as NA.
    Volume = ifelse(all(is.na(Volume)), 
                    NA, 
                    first(Volume[!is.na(Volume)])
                    )
  
  ) %>%
  
  # Remove the grouping structure from the data.
  ungroup()


# Convert UnixTime from seconds since epoch to actual date-time in UTC, then convert to Date format.
filtered_data <- filtered_data %>% 
  mutate(Date = as.Date(as.POSIXct(UnixTime, origin = "1970-01-01", tz = "UTC")))

# Group the data by the new Date column to perform operations on each day's data.
daily_data <- filtered_data %>%
  
  group_by(Date) %>%
  
  summarise(
    # Get the opening price and volume (first entries of the day)
    OpeningPrice = first(Price),
    OpeningVolume = first(Volume),
    # Get the closing price and volume (last entries of the day)
    ClosingPrice = last(Price),
    ClosingVolume = last(Volume)
  ) %>%
  
  # Remove the group by structure from the data to return it to a regular dataframe.
  ungroup()

# Change name of your output directory
out_directory <- "C:/Users/elmop/Downloads"

# Save the univariate time series (all dataset) in daily resolution using WriteDates 
# as .csv with first column in Date format
TSAT::WriteDates(
  FileName = "preprocessed_data",
  TSdata = daily_data,
  Key = 1:nrow(daily_data),
  OutDirectory = out_directory
)

daily_data$Date <- as.numeric(difftime(daily_data$Date, as.Date("1970-01-01"), units = "secs"))

# Save the preprocessed data in the same format using the TSAT::WriteTS function
# as a .ts file with the first column of dates in Unix format
TSAT::WriteTS(FileName = "ts_opening_price",
              SeriesVal = as.character(daily_data$OpeningPrice),
              Time = daily_data$Date,
              OutDirectory = out_directory)

TSAT::WriteTS(FileName = "ts_opening_volume",
              SeriesVal = as.character(daily_data$OpeningVolume),
              Time = daily_data$Date,
              OutDirectory = out_directory)

TSAT::WriteTS(FileName = "ts_closing_price",
              SeriesVal = as.character(daily_data$ClosingPrice),
              Time = daily_data$Date,
              OutDirectory = out_directory)

TSAT::WriteTS(FileName = "ts_closing_volume",
              SeriesVal = as.character(daily_data$ClosingVolume),
              Time = daily_data$Date,
              OutDirectory = out_directory)

# Read the preprocessed data (Just for testing)
preprocessed_data <- ReadTS(FileName = "ts_opening_price", 
                            InDirectory = out_directory)






# Compute MSM distance between two time series, c = cost parameter
msmDist <- function(ts1, ts2, c) {
  m <- length(ts1)
  n <- length(ts2)
  
  # Extend the time series adding a first entry set to infinity
  ts1 <- c(Inf, ts1)
  ts2 <- c(Inf, ts2)
  
  # Initialize the temporary array to infinity
  tmpArray <- rep(Inf, n + 1)
  
  # calculate cost of Split/Merge operation
  C <- function(new_point, x, y, c) {
    #new_point point to merge/ split to
    #x         xcoord
    #y         ycoord
    if (new_point < min(x, y) || new_point > max(x, y)) {
      return(c + min(abs(new_point - x), abs(new_point - y)))
    }
    return(c)
  }
  
  # Initialize the temporary value to 0
  tmp <- 0
  # Main loop to compute MSM distance
  for (i in 2:(m + 1)) { #start at 2nd index in array, to refer to the first position [1] in the time series we refer to j-1
    
    for (j in 2:(n + 1)) {
      d1 <- tmp + abs(ts1[i] - ts2[j]) 
      d2 <- tmpArray[j] + C(ts1[i], ts1[i - 1], ts2[j], c)
      d3 <- tmpArray[j - 1] + C(ts2[j], ts1[i], ts2[j - 1], c)

      
      # Store old entry before overwriting
      tmp <- tmpArray[j]
      tmpArray[j] <- min(c(d1, d2, d3))
    }
    # After last entry in row set to inf, otherwise the move cost for the first entry in the next row are wrong
    tmp <- Inf
  }
  
  return(tmpArray[n + 1])
}

#testing
a = daily_data$OpeningPrice
b = daily_data$ClosingPrice
#msmDist(a[1:(length(a) - 1)],b[1:(length(b) - 1)],1) #running this took like 10 mins
