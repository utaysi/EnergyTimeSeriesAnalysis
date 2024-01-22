install.packages("ScatterDensity")
install.packages("parallelDist")
install.packages("MBA")
install.packages("devtools")
install.packages("remotes")
remotes::install_github("Mthrun/RHmm")
library(ScatterDensity)
library(readr)
library(parallelDist)
library(MBA)
library(DataVisualizations)
library(plyr)
library(RHmm)
library(ggplot2)


file_path <- "Project3-Data/01WorldGDP70to2010lrn.sec"
train <- read_table("Project3-Data/ClosingPriceVsVolume_train_D2_N13319.csv", 
                                                   col_types = cols(`%` = col_skip(), Price = col_double(), 
                                                                    Volume = col_skip(), X6 = col_skip()),skip = 3)
colnames(train) <- c("Time", "Price", "Volume")
na_indices <- which(is.na(train$Volume))
print(na_indices)
na_count <- sum(is.na(train$Volume))
print(na_count)

DataVisualizations::InspectVariable(train$Price)

# Plot Price vs Time
ggplot(train, aes(x = Time, y = Price)) + 
  geom_line(color = "blue") +
  ggtitle("Price over Time") +
  xlab("Time") + ylab("Price")

# Plot Volume vs Time
ggplot(train, aes(x = Time, y = Volume)) + 
  geom_line(color = "red") +
  ggtitle("Volume over Time") +
  xlab("Time") + ylab("Volume")
train_no_na <- na.omit(train) # Drop NA values

PDEscatter(train_no_na$Price,train_no_na$Volume) # This plot is lopsided, will threshold to get better view
##help(package = ScatterDensity)


train_thresholded <- train_no_na[train_no_na$Price <= 80,]
train_thresholded <- train_thresholded[train_thresholded$Volume <= 200000,]
PDEscatter(train_thresholded$Price,train_thresholded$Volume)
## clearly 2 clusters, but there appeared to be one also higher up:

train_thresholded2 <- train_no_na[train_no_na$Price <= 80,]
train_thresholded2 <- train_thresholded2[train_thresholded2$Volume >= 250000,]
PDEscatter(train_thresholded2$Price,train_thresholded2$Volume)
## Volumes so low, I'll go with 2. COuld be 3 clusters tho

#################################################################################################
#Probably need to make the TS stationary here, I'm not 100% sure
#################################################################################################
train$Price_smoothed <- stats::filter(train$Price, rep(1/32, 32), sides = 2)
ggplot(train, aes(x = Time, y = Price_smoothed)) + 
  geom_line(color = "blue") +
  ggtitle("smoothed price") +
  xlab("Time") + ylab("Price")

train$RelDiffPercent=RelDiffWithYesterday(train$Price_smoothed,na.rm=TRUE)*100 #I think this step can be skipped if its alr stationary


ggplot(train, aes(x = Time, y = RelDiffPercent)) + 
  geom_line(color = "blue") +
  ggtitle("relative differences") +
  xlab("Time") + ylab("Price")

RelDiffPercent <- na.omit(train$RelDiffPercent)
HMMmodel <- HMMFit(obs=RelDiffPercent, nStates=2,
                    control=list(verbose=1, init= 'KMEANS'),# K-means initialisierung
                    asymptCov=TRUE)




HMMmodel
summary(HMMmodel)

HMMmeans = round(HMMmodel$HMM$distribution$mean,1)     
print(paste0('HMMmeans ',HMMmeans ))
HMMsdev  = round(sqrt(HMMmodel$HMM$distribution$var),1)
print(paste0('HMMsdev  ',HMMsdev ))

TransitionMatrix = HMMmodel$HMM$transMat
TransitionMatrixPercent = round(TransitionMatrix*100)
TransitionMatrixPercent
HMMmeans


VitPath <- viterbi(HMMmodel, RelDiffPercent)
HMMcls <- VitPath$states # the predicted classes of the HMM
train$HMMcls <- HMMcls
head(HMMcls)
HMMGraphicDiag(VitPath, HMMmodel, RelDiffPercent)
HMMPlotSerie(RelDiffPercent, VitPath)

# Create the scatter plot
fig <- plot_ly(data = train, x = ~Price, y = ~Volume, type = 'scatter', mode = 'markers',
               marker = list(color = ~HMMcls, colorscale = 'Viridis', showscale = TRUE))

# Add layout details
fig <- fig %>% layout(title = 'Scatter plot of Price vs Volume',
                      xaxis = list(title = 'Price'),
                      yaxis = list(title = 'Volume'))

# Show the plot
fig

######################################################################################################
#testing
#################################################################################################

data(SunSpots)
SunSpots$Time <- seq_along(SunSpots$SmothedMonSunspot)
data <- data.frame(Time = SunSpots$Time, Value = SunSpots$SmothedMonSunspot)
ggplot(data, aes(x = Time, y = Value)) + 
  geom_line() + 
  ggtitle("Time Series Visualization") +
  xlab("Time (Sequential Order)") + 
  ylab("Value")


library(datasets)
data("sunspot.month")
plot(sunspot.month, main = "Monthly Sunspots Data", xlab = "Month", ylab = "Number of Sunspots")
sunspot_df <- data.frame(Time=seq_along(sunspot.month), Value = sunspot.month)
ggplot(sunspot_df, aes(x = Time, y = Value)) + 
  geom_line() + 
  ggtitle("Time Series Visualization") +
  xlab("Time (Sequential Order)") + 
  ylab("Value")

RelDiffProzent=RelDiffWithYesterday(SunSpots$SmothedMonSunspot,na.rm=TRUE)*100
plot(RelDiffProzent, type='l')
plot(train$RelDiffPercent, type='l')
qqnorm(train$RelDiffPercent)
qqline(train$RelDiffPercent, col = "red")
