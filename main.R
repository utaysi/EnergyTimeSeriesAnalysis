# Read File
#File = ReadTS(FileName = "Energy_blend_rawdata2784724", InDirectory = "G:\\My Drive\\School\\Winter 2023\\Temporal Data Mining\\Project\\EnergyTimeSeriesAnalysis")
File = ReadTS(FileName = "Energy_blend_rawdata2784724") #Make sure to change the original filename to "Energy_blend_rawdata2784724.ts"

# Skip initial lines, set separator, manually set column names, read/store the data
#data <- read.table("Energy_blend_rawdata2784724ts.ts",  sep = "\t", skip = 4, header = FALSE, col.names = c("UnixTime", "Price", "TimeInserted", "EntryID", "Volume"))

#splitting the columns
File$Series[1:7,]
File$FurtherTexts[1:7,]
Prices = File$Series[,2]
UnixTime =File$Series[,1]
UnixTime1= order(File$Series[,1])

#View Structure
str(File)
#removing duplicated indexes
Uniqueindexes= which(!duplicated(x = UnixTime1))
##########################################################

# Filter NaN values
data_cleaned <- File %>%filter(!is.na(UnixTime) & !is.na(Price))

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
