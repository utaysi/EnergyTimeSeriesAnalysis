
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
File <- ReadTS(FileName = "Energy_blend_rawdata2784724ts", InDirectory = "ur file path ")
 #Make sure to change the original filename to "Energy_blend_rawdata2784724.ts"

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
                                 ifelse(abs(diff(Price)) < 0.05, mean(Price, na.rm = TRUE), NA),
                                 # Select the first non-NA price.
                                 first(Price[!is.na(Price)])),
                          # Select the first non-NA price if the EntryID check doesn't lead to a conclusion.
                          first(Price[!is.na(Price)]))),
    # Select the first non-NA EntryID for each UnixTime. If all are NA, set as NA.
    EntryID = ifelse(all(is.na(EntryID)), NA, first(EntryID[!is.na(EntryID)])),
    # Select the first non-NA TimeInserted for each UnixTime. If all are NA, set as NA.
    TimeInserted = ifelse(all(is.na(TimeInserted)), NA, first(TimeInserted[!is.na(TimeInserted)])),
    # Select the first non-NA Volume for each UnixTime. If all are NA, set as NA.
    Volume = ifelse(all(is.na(Volume)), NA, first(Volume[!is.na(Volume)]))
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


# Save the univariate time series in daily resolution using WriteDates
WriteDates(
  FileName = "the_univariate_time_series",
  TSdata = daily_data,
  Key = 1:ncol(daily_data),
  OutDirectory = "C:/Users/Dell1/Downloads"
)

# Insert NaN for days with no opening price
daily_data$OpeningPrice[is.na(daily_data$OpeningPrice)] <- NaN

daily_data$TimeInserted <- as.numeric(as.POSIXct(daily_data$Date, origin = "1970-01-01", tz = "UTC"))

# Save the preprocessed data in the same format using the TSAT::WriteTS function
TSAT::WriteTS(FileName = "preprocessed_data",SeriesVal = as.character(daily_data$OpeningPrice),Time = daily_data$TimeInserted, OutDirectory = "C:/Users/Dell1/Downloads" #change it to your path)
              ,
              # Read the preprocessed data
              preprocessed_data <- ReadTS(FileName = "preprocessed_data", InDirectory = "C:/Users/Dell1/Downloads") #change it to your path
)

