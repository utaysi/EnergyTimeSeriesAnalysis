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
File <- ReadTS(FileName = "Energy_blend_rawdata2784724ts", InDirectory = "UR PATH TO THAT FILE")

# Combine 'Series' and 'FurtherTexts' from File into a single dataframe
combined_data <- as.data.frame(cbind(File$Series, File$FurtherTexts))

# Process filtered_data by arranging and summarizing
filtered_data <- combined_data %>%
  arrange(UnixTime, desc(EntryID), desc(TimeInserted)) %>%
  group_by(UnixTime) %>%
  summarise(
    Count = n(),
    Price = if (Count == 1 || length(unique(Price)) == 1) {
      first(Price)
    } else if (all(is.na(EntryID)) || length(unique(EntryID)) == 1) {
      if (all(is.na(TimeInserted)) || length(unique(TimeInserted)) == 1) {
        if (max(Price, na.rm = TRUE) - min(Price, na.rm = TRUE) < 0.05) {
          mean(Price, na.rm = TRUE)
        } else {
          NA
        }
      } else {
        first(Price[!is.na(Price)])
      }
    } else {
      first(Price[!is.na(Price)])
    },
    EntryID = if (all(is.na(EntryID))) {
      NA
    } else {
      first(EntryID[!is.na(EntryID)])
    },
    TimeInserted = if (all(is.na(TimeInserted))) {
      NA
    } else {
      first(TimeInserted[!is.na(TimeInserted)])
    },
    Volume = if (all(is.na(Volume))) {
      NA
    } else {
      first(Volume[!is.na(Volume)])
    }
    
  ) %>%
  ungroup() %>%
  select(-Count)

# Convert UnixTime from seconds since epoch to actual date-time in UTC, then convert to Date format.
filtered_data <- filtered_data %>%
  mutate(Date = as.Date(as.POSIXct(UnixTime, origin = "1970-01-01", tz = "UTC")))

# Group the data by the new Date column to perform operations on each day's data.
daily_data <- filtered_data %>%
  group_by(Date) %>%
  summarise(
    OpeningPrice = first(Price),
    OpeningVolume = first(Volume),
    ClosingPrice = last(Price),
    ClosingVolume = last(Volume)
  ) %>%
  ungroup()

# Save the univariate time series in daily resolution using WriteDates
WriteDates(
  FileName = "the_univariate_time_series",
  TSdata = daily_data,
  Key = 1:ncol(daily_data),
  OutDirectory = "UR PATH TO THAT FILE"
)

# Insert NaN for days with no opening price
daily_data$OpeningPrice[is.na(daily_data$OpeningPrice)] <- NaN

daily_data$TimeInserted <- as.numeric(as.POSIXct(daily_data$Date, origin = "1970-01-01", tz = "UTC"))

# Save the preprocessed data in the same format using the TSAT::WriteTS function
TSAT::WriteTS(FileName = "preprocessed_data",SeriesVal = as.character(daily_data$OpeningPrice),Time = daily_data$TimeInserted, OutDirectory = "C:/Users/Dell1/Downloads" #change it to your path)
              ,
              # Read the preprocessed data
              preprocessed_data <- ReadTS(FileName = "preprocessed_data", InDirectory = "UR PATH TO THAT FILE") #change it to your path
)
