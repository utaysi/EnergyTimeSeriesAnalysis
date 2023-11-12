# Skip initial lines, set separator, manually set column names, read/store the data
data <- read.table("Energy_blend_rawdata2784724ts.sec", 
                   sep = "\t", 
                   skip = 4, 
                   header = FALSE, 
                   col.names = c("UnixTime", "Price", "TimeInserted", "EntryID", "Volume"))

#View Structure
str(data)

# Filter NaN values
data_cleaned <- data %>%
  filter(!is.na(UnixTime) & !is.na(Price))

# Create a copy of the original data
data_TimeConverted = data_cleaned
#test
data_TimeConverted2 = data_cleaned

# Convert UnixTime to POSIXct (R Date-Time class) with Chicago timezone
data_TimeConverted$UnixTime <- as.POSIXct(data_TimeConverted$UnixTime, origin = "1970-01-01", tz = "America/Chicago")
#test
data_TimeConverted2$UnixTime <- as.POSIXct(data_TimeConverted2$UnixTime, origin = "1970-01-01", tz = "America/Chicago")

# Convert the time zone to UTC
data_TimeConverted$UnixTime <- with_tz(data_TimeConverted$UnixTime, tzone = "UTC")
