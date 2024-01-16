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


file_path <- "Project3-Data/01WorldGDP70to2010lrn.sec"
train <- read_table("Project3-Data/ClosingPriceVsVolume_train_D2_N13319.csv", 
                                                   col_types = cols(`%` = col_skip(), Price = col_double(), 
                                                                    Volume = col_skip(), X6 = col_skip()),skip = 3)
colnames(train) <- c("Time", "Price", "Volume")


train <- na.omit(train) # Drop NA values

PDEscatter(train$Price,train$Volume) # This plot is lopsided, will threshold to get better view
##help(package = ScatterDensity)


train_thresholded <- train[train$Price <= 80,]
train_thresholded <- train_thresholded[train_thresholded$Volume <= 200000,]
PDEscatter(train_thresholded$Price,train_thresholded$Volume)
## clearly 2 clusters, but there appeared to be one also higher up:

train_thresholded2 <- train[train$Price <= 80,]
train_thresholded2 <- train_thresholded2[train_thresholded2$Volume >= 250000,]
PDEscatter(train_thresholded2$Price,train_thresholded2$Volume)
## Volumes so low, I'll go with 2. COuld be 3 clusters tho

#################################################################################################
#"Probably need to make the TS stationary here, I'm not 100% sure"
#################################################################################################

RelDiffPercent=RelDiffWithYesterday(train$Volume,na.rm=TRUE)*100 #I think this step can be skipped if its alr stationary

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
