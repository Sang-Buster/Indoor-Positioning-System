#----------------------------------------------------------#
#------------------Step 0: Load Libraries -----------------#
#----------------------------------------------------------#

library(tidyverse)  # Load core packages: 
# ggplot2,   for data visualization.
# dplyr,     for data manipulation.
# tidyr,     for data tidying.
# purrr,     for functional programming.
# tibble,    for tibbles, a modern re-imagining of data frames.
# stringr,   for strings.
# forcats,   for factors.
# lubridate, for date/times.
# readr,     for reading .csv, .tsv, and .fwf files.
# readxl,    for reading .xls, and .xlxs files.
# feather,   for sharing with Python and other languages.
# haven,     for SPSS, SAS and Stata files.
# httr,      for web apis.
# jsonlite   for JSON.
# rvest,     for web scraping.
# xml2,      for XML.
# modelr,    for modelling within a pipeline
# broom,     for turning models into tidy data
# hms,       for times.

library(magrittr)   # Pipeline operator
library(lobstr)     # Visualizing abstract syntax trees, stack trees, and object sizes
library(pander)     # Exporting/converting complex pandoc documents, EX: df to Pandoc table
library(ggforce)    # More plot functions on top of ggplot2
library(ggpubr)     # Automatically add p-values and significance levels  plots. 
# Arrange and annotate multiple plots on the same page. 
# Change graphical parameters such as colors and labels.
library(sf)         # Geo-spatial vector manipulation: points, lines, polygons
library(kableExtra) # Generate 90 % of complex/advanced/self-customized/beautiful tables
library(latex2exp)  # Latex axis titles in ggplot2
library(ellipse)    # Simultaneous confidence interval region to check C.I. of 2 slope parameters
library(plotly)     # User interactive plots

library(fields)     # Used for drawing heat maps

set.seed(27)        # make random results reproducible

WD <- getwd()
setwd(WD)
remove(WD)

#--------------------------------------------------------#
#-----------------Step 1: Data Cleaning------------------#
#--------------------------------------------------------#

# Load txt files
offline_data_txt <- readLines("raw_data/offline.final.trace.txt")
online_data_txt  <- readLines("raw_data/online.final.trace.txt")
AP_Loc           <- read.table("raw_data/accessPointLocations.txt", header=T)

# To split the text into rows, then piece together column names and rows of data
processLine <- function(x) {
  #Use Regex to split data on ;=, characters (determined by looking at data)
  tokens = strsplit(x, "[;=,]")[[1]]
  #If no signals are recorded (token length is 10) then remove the row
  if (length(tokens) == 10)
    return(NULL)
  #For each signal recording, tokens 1, 3, 5, and 9 are column names
  tmp = matrix(tokens[-(1:10)], ncol=4, byrow=T)
  #Column bind column names with the other rows in matrix form
  cbind(matrix(tokens[c(2,4,6:8,10)], nrow=nrow(tmp), ncol=6, byrow=T), tmp)
}

# Run the processLine function over the entire dataset to build dataframe
offlines <- offline_data_txt[substr(offline_data_txt, 1,1) != "#"]  #Removes comments from data
offline_tmp   <- lapply(offlines, processLine)
onlines <- online_data_txt[substr(online_data_txt, 1,1) != "#"]  #Removes comments from data
online_tmp   <- lapply(onlines, processLine)

IPS_offline_Data <- as.data.frame(do.call("rbind", offline_tmp), stringsAsFactors=F)
IPS_online_Data  <- as.data.frame(do.call("rbind", online_tmp), stringsAsFactors=F)

# Add column names
names(IPS_offline_Data) <- c("timeStamp", "scanedMAC", "posX", "posY", "posZ", "orientation", "MAC", "RSSI", "frequency", "type")
names(IPS_online_Data) <- c("timeStamp", "scanedMAC", "posX", "posY", "posZ", "orientation", "MAC", "RSSI", "frequency", "type")

#--------------------------------------------------------#
#----------------Step 2: Data Adjustment-----------------#
#--------------------------------------------------------#

# Take only access point = 3, device in Ad-hoc mode = 1
IPS_offline_Data <- IPS_offline_Data %>% filter(type=='3')
IPS_offline_Data$type <- NULL
IPS_online_Data <- IPS_online_Data %>% filter(type=='3')
IPS_online_Data$type <- NULL

#keep the top 7 Macs with over 10k obs
goodMacs <- names(sort(table(IPS_offline_Data$MAC), decreasing=T))[1:7]
IPS_offline_Data <- IPS_offline_Data[IPS_offline_Data$MAC %in% goodMacs,]

# Data coercion chr -> numeric
varList <- c("timeStamp", "posX", "posY", "posZ", "orientation", "RSSI")
IPS_offline_Data[varList] <- lapply(IPS_offline_Data[varList], as.numeric)
IPS_online_Data[varList] <- lapply(IPS_online_Data[varList], as.numeric)

# Convert timeStamp to millisecond for Unix time conversions
IPS_offline_Data$timeStamp <- IPS_offline_Data$timeStamp/1000 
IPS_offline_Data$timeStamp <- as.POSIXct(IPS_offline_Data$time, tz="UTC", origin = "1970-01-01")

IPS_online_Data$timeStamp <- IPS_online_Data$timeStamp/1000 
IPS_online_Data$timeStamp <- as.POSIXct(IPS_online_Data$time, tz="UTC", origin = "1970-01-01")

# length(unique(IPS_offline_Data$scanedMAC))  #Output: 1
# length(unique(IPS_offline_Data$posZ))       #Output: 1
# length(unique(IPS_online_Data$scanedMAC))   #Output: 1
# length(unique(IPS_online_Data$posZ))        #Output: 1

#Since the scanedMAC and posZ are constant, they can be dropped
IPS_offline_Data$scanedMAC <- NULL
IPS_offline_Data$posZ <- NULL


IPS_online_Data$scanedMAC <- NULL
IPS_online_Data$posZ <- NULL


# Orientation -> Angle # 
Ori.to.Angle <- function(angles) {
  refs = seq(0, by=45, length=9)
  q = sapply(angles, 
             function(o){
               if(o>0){
                 which.min(abs(o-refs))
               }else{
                 which.min(abs(o+360-refs))
               }})
  c(refs[1:8], 0)[q]
}

# Orientation -> Direction 
Ori.to.Direction <- function(orientation) {
  angles = seq(from=0, to=360, by=45) 
  q = sapply(orientation, 
             function(o){
               if(o>0){
                 which.min(abs(o-angles))
               }else{
                 which.min(abs(o+360-angles))
               }})
  directions_index <- c(1:8, 1)[q]
  directions = c("E→", "NE↗", "N↑", "NW↖", "W←", "SW↙", "S↓", "SE↘") 
  directions[directions_index] 
}

# Apply function from above to orientation data
IPS_offline_Data$angle <- Ori.to.Angle(IPS_offline_Data$orientation) 
IPS_online_Data$angle <- Ori.to.Angle(IPS_online_Data$orientation) 

IPS_offline_Data$direction <- Ori.to.Direction(IPS_offline_Data$orientation) 
IPS_online_Data$direction <- Ori.to.Direction(IPS_online_Data$orientation) 

# Number of MAC addreses = number of frequency channels, should be 6 MAC for 6 WAP, with 6 Freq
IPS_offline_Data <- IPS_offline_Data[IPS_offline_Data$MAC %in% AP_Loc$Macs,]
IPS_online_Data <- IPS_online_Data[IPS_online_Data$MAC %in% AP_Loc$Macs,]

# Since there is a 1:1 relationship between Macs and frequencies, drop freq channel
IPS_offline_Data$frequency <- NULL
IPS_online_Data$frequency <- NULL

# Paste all combos of x and y
IPS_offline_Data$posXY <- paste(IPS_offline_Data$posX, IPS_offline_Data$posY, sep=", ") 
IPS_online_Data$posXY <- paste(IPS_online_Data$posX, IPS_online_Data$posY, sep=", ") 

# Clear unneeded objects
remove(offlines)
remove(onlines)
remove(offline_tmp)
remove(online_tmp)
remove(offline_data_txt)
remove(online_data_txt)
remove(varList)
remove(goodMacs)


#-----------------------------------------------------------#
#------------------- Find Training Data Set ----------------#
#-----------------------------------------------------------#  

# Create a list of dfs for every combo of posXY, angle, and AP
Offline_RSSI.statCompute <- with(IPS_offline_Data, by(IPS_offline_Data, list(posXY, angle, MAC), function(x) x))

# Get signal stats on each df
offline.signalStat <- lapply(Offline_RSSI.statCompute,
                             function (oneLoc.Angle.AP) {
                               stats = oneLoc.Angle.AP[1, ]
                               stats$medSignal = median(oneLoc.Angle.AP$RSSI)
                               stats$avgSignal = mean(oneLoc.Angle.AP$RSSI)
                               stats$num = length(oneLoc.Angle.AP$RSSI)
                               stats$sdSignal = sd(oneLoc.Angle.AP$RSSI)
                               stats$iqrSignal = IQR(oneLoc.Angle.AP$RSSI)
                               stats
                             })

IPS_trainingData <- do.call("rbind", offline.signalStat)

# Left join apX and apY
AP_Loc <- AP_Loc %>% rename(MAC=Macs)

IPS_trainingData <- left_join(IPS_trainingData, AP_Loc, by="MAC") 
IPS_trainingData <- IPS_trainingData %>% rename(apX=x, apY=y)

# Calculate Euclidean distances from the device to APs
diffs <- IPS_trainingData[ , c("posX", "posY")] - IPS_trainingData[ , c("apX", "apY")]
IPS_trainingData$dist <- sqrt(diffs[,1]^2 + diffs[,2]^2)

# Clear unneeded objects
remove(Offline_RSSI.statCompute)
remove(offline.signalStat)
remove(diffs)

#----------------------------------------------------------#
#------------------- Find Testing Data Set ----------------#
#----------------------------------------------------------#  

# Creates a 6 columns of signal strengths with respect to each APs
keepVars <- c("posX", "posY", "posXY", "orientation", "angle", "direction")
online_RSSI.statCompute <- with(IPS_online_Data,
                                by(IPS_online_Data, list(posXY),
                                   function(x) {
                                     stats = x[1, keepVars]
                                     avgSS = tapply(x$RSSI, x$MAC, mean)
                                     y = matrix(avgSS, nrow=1, ncol=6,
                                                dimnames = list(stats$posXY, names(avgSS)))
                                     cbind(stats, y)
                                   }))
IPS_testingData <- do.call("rbind", online_RSSI.statCompute)

# Clear unneeded objects
remove(online_RSSI.statCompute)
remove(keepVars)

#  length(unique(IPS_online_Data$posXY))
# [1] 60
#  length(unique(IPS_offline_Data$posXY))
# [1] 166
# In deed, we have 60 locations and 166 locations observed in offline and online data sets.

#----------------------------------------------------------#
#------------------Step 4: Data Analysis ------------------#
#----------------------------------------------------------#

### Selected TrainSS Example
# For m angles, find the closest desired orientations to the new observations
m <- 3
angleNewObs <- -45
refs <- seq(0, by=45, length=8)
nearestAngle <- Ori.to.Angle(angleNewObs)

if ( m%%2 == 1) {
  angles = seq(-45*(m-1)/2, 45*(m-1)/2, length=m)
} else {
  m = m + 1
  angles = seq(-45*(m-1)/2, 45*(m-1)/2, length=m)
  if (sign(angleNewObs - nearestAngle) > -1)
    angles = angles[-1]
  else
    angles = angles[-m]
}

# Map the angles to values in refs (-45 maps to 315 and 405 maps to 45)
angles <- angles + nearestAngle
angles[angles < 0] <- angles [angles <0] + 360
angles[angles > 360] <- angles[angles >360] - 360

trainSubset <- IPS_trainingData[IPS_trainingData$angle %in% angles, ]

# Aggregate RSSI with respect to 6 APs
reshapeSS <- function (data, varSignal = "signal", keepVars = c("posXY", "orientation", "direction")) {
  byLocation = with(data, by(data, list(posXY),
                             function(x) {
                               ans = x[1, keepVars]
                               avgSS = tapply(x[,varSignal], x$MAC, mean)
                               y = matrix(avgSS, nrow=1, ncol=6, dimnames = list(ans$posXY, names(avgSS)))
                               cbind(ans, y)
                             }))
  newDataSS <- do.call("rbind", byLocation)
  return(newDataSS)
}

# Summarize and reshape trainSubset
trainSS <- reshapeSS(trainSubset, varSignal = "avgSignal")


### Function for Selected TrainSS 
#Aggregate RSSI with respect to 6 APs
reshapeSS <- function (data, varSignal = "signal", keepVars = c("posXY", "posX", "posY")) {
  byLocation = with(data, by(data, list(posXY),
                             function(x) {
                               ans = x[1, keepVars]
                               avgSS = tapply(x[,varSignal], x$MAC, mean)
                               y = matrix(avgSS, nrow=1, ncol=6, dimnames = list(ans$posXY, names(avgSS)))
                               cbind(ans, y)
                             }))
  newDataSS <- do.call("rbind", byLocation)
  return(newDataSS)
}

selectTrain <- function(angle.newObs, data, m){
  # m is the number of angles to keep between 1 and 5
  refs = seq(0, by = 45, length  = 8)
  nearestAngle = Ori.to.Angle(angle.newObs)
  
  if (m %% 2 == 1) 
    angles = seq(-45 * (m - 1) /2, 45 * (m - 1) /2, length = m)
  else {
    m = m + 1
    angles = seq(-45 * (m - 1) /2, 45 * (m - 1) /2, length = m)
    if (sign(angle.newObs - nearestAngle) > -1) 
      angles = angles[ -1 ]
    else 
      angles = angles[ -m ]
  }
  angles = angles + nearestAngle
  angles[angles < 0] = angles[ angles < 0 ] + 360
  angles[angles > 360] = angles[ angles > 360 ] - 360
  angles = sort(angles) 
  
  trainSubset = data[ data$angle %in% angles, ]
  reshapeSS(trainSubset, varSignal = "avgSignal")
}

# Test function with new obs angle 130
train.at.angle <- selectTrain(angle.newObs=130, data=IPS_trainingData, m=3)
# length(train130[[1]])
# [1] 166 #Indeed we have 166 locations recorded

### KNN Estimation Example
#Build function findNN that finds the k nearest neighbors from any point
#Parameters are a numeric vector of 6 new signal strengths and the return value from selectTrain()

#Function returns the locations of the training observations in order of closeness to the new observation's signal strength
findNN <- function(newSignal, trainSubset) {
  diffs = apply(trainSubset[ , 4:9], 1, function(x) x - newSignal)
  dists = apply(diffs, 2, function(x) sqrt(sum(x^2)))
  closest = order(dists)
  return(trainSubset[closest, 1:3])
}

#Distance Weighted average the first k locations returned by findNN to estimate location of new observation
predXY = function(newSignals, newAngles, trainData, 
                  numAngles = 1, k = 3){
  
  closeXY = list(length = nrow(newSignals))
  
  for (i in 1:nrow(newSignals)) {
    trainSS = selectTrain(newAngles[i], trainData, m = numAngles)
    closeXY[[i]] = 
      findNN(newSignal = as.numeric(newSignals[i, ]), trainSS)
  }
  
  estXY = lapply(closeXY, 
                 function(x) sapply(x[ , 2:3], 
                                    function(x) mean(x[1:k])))
  estXY = do.call("rbind", estXY)
  return(estXY)
}

#Test function with 1 nearest neighbors and 3 orientations
estXYk1 <- predXY(newSignals = IPS_testingData[ , 7:12], 
                  newAngles = IPS_testingData[ , 5], 
                  IPS_trainingData, numAngles = 3, k = 1)
#Test function with 3 nearest neighbors and 3 orientations
estXYk3 <- predXY(newSignals = IPS_testingData[ , 7:12], 
                  newAngles = IPS_testingData[ , 5], 
                  IPS_trainingData, numAngles = 3, k = 3)
#Test function with 5 nearest neighbors and 3 orientations
estXYk5 <- predXY(newSignals = IPS_testingData[ , 7:12], 
                  newAngles = IPS_testingData[ , 5], 
                  IPS_trainingData, numAngles = 5, k = 1)

#Map of actual vs predicted locations of new signals shows model accuracy
#K=3 more accurate than k=1 because errors are shorter distance and less problematic because they follow the hallways
#Check website for plotting code

#Calculate SSE to measure fit
calcError <- function(estXY, actualXY) {
  sum(rowSums( (estXY - actualXY)^2) )
}
actualXY <- IPS_testingData[ , c("posX", "posY")]
sapply(list(estXYk1, estXYk3, estXYk5), calcError, actualXY)


### Find Optimal k
#Use k-fold cross validation to find optimal k for KNN

#Perform k-fold cross validation with k=11 so each fold has 15 locations randomly selected
v <- 11
permuteLocs <- sample(unique(IPS_trainingData$posXY))
permuteLocs <- matrix(permuteLocs, ncol = v, 
                      nrow = floor(length(permuteLocs)/v))

testFold <- subset(IPS_trainingData, posXY %in% permuteLocs[ , 1])

reshapeSS <- function(data, varSignal = "RSSI", 
                      keepVars = c("posXY", "posX","posY"),
                      sampleAngle = FALSE, 
                      refs = seq(0, 315, by = 45)) {
  byLocation =
    with(data, by(data, list(posXY), 
                  function(x) {
                    if (sampleAngle) {
                      x = x[x$angle == sample(refs, size = 1), ]}
                    ans = x[1, keepVars]
                    avgSS = tapply(x[ , varSignal ], x$MAC, mean)
                    y = matrix(avgSS, nrow = 1, ncol = 6,
                               dimnames = list(ans$posXY,
                                               names(avgSS)))
                    cbind(ans, y)
                  }))
  
  newDataSS = do.call("rbind", byLocation)
  return(newDataSS)
}

keepVars <- c("posXY", "posX","posY", "orientation", "angle", "direction")

testCVSummary <- reshapeSS(IPS_offline_Data, keepVars = keepVars, 
                           sampleAngle = TRUE)

testFold <- subset(testCVSummary, 
                   posXY %in% permuteLocs[ , 1])

trainFold <- subset(IPS_trainingData,
                    posXY %in% permuteLocs[ , -1])

estFold <- predXY(newSignals = testFold[ , 7:12], 
                  newAngles = testFold[ , 4], 
                  trainFold, numAngles = 3, k = 3)

actualFold <- testFold[ , c("posX", "posY")]
calcError(estFold, actualFold)


### Function for k-Fold CV
#Wrap the code above into loops over the folds and number of neighbors for K=20
K <- 30
err <- rep(0, K)

for (j in 1:v) {
  testFold = subset(testCVSummary, 
                    posXY %in% permuteLocs[ , j])
  trainFold = subset(IPS_trainingData,
                     posXY %in% permuteLocs[ , -j])
  actualFold = testFold[ , c("posX", "posY")]
  
  for (k in 1:K) {
    estFold = predXY(newSignals = testFold[ , 7:12],
                     newAngles = testFold[ , 4], 
                     trainFold, numAngles = 3, k = k)
    err[k] = err[k] + calcError(estFold, actualFold)
  }
}

pdf(file = "Geo_CVChoiceOfK.pdf", width = 10, height = 6)
oldPar = par(mar = c(4, 3, 1, 1))
plot(y = err, x = (1:K),  type = "l", lwd= 2,
     ylim = c(1200, 2100),
     xlab = "Number of Neighbors",
     ylab = "Sum of Square Errors")

rmseMin <- min(err)
kMin <- which(err == rmseMin)[1]
segments(x0 = 0, x1 = kMin, y0 = rmseMin, col = gray(0.4), 
         lty = 2, lwd = 2)
segments(x0 = kMin, x1 = kMin, y0 = 1100,  y1 = rmseMin, 
         col = grey(0.4), lty = 2, lwd = 2)

#mtext(kMin, side = 1, line = 1, at = kMin, col = grey(0.4))
text(x = kMin - 2, y = rmseMin + 40, 
     label = as.character(round(rmseMin)), col = grey(0.4))
par(oldPar)
dev.off()

#Since errors level out at k=5 and higher, use k= and apply it to training and test data
estXYk5 <- predXY(newSignals = testSummary[ , 6:11], 
                  newAngles = testSummary[ , 4], 
                  trainSummary, numAngles = 3, k = 5)

#Tally the errors
calcError(estXYk5, actualXY)

#Notice that number of angles is not optimized through cross validation, but it could be for a better model

#--------------------------------------------------------------#
#--------------------Step 5: Data Visualization ---------------#
#--------------------------------------------------------------#

floorErrorMap <- function(estXY, actualXY, trainPoints = NULL, AP = NULL){
  
  plot(0, 0, xlim = c(0, 35), ylim = c(-3, 15), type = "n",
       xlab = "", ylab = "", axes = FALSE)
  box()
  if ( !is.null(AP) ) points(AP, pch = 15)
  if ( !is.null(trainPoints) )
    points(trainPoints, pch = 19, col="grey", cex = 0.6)
  
  points(x = actualXY[, 1], y = actualXY[, 2], 
         pch = 19, cex = 0.8 )
  points(x = estXY[, 1], y = estXY[, 2], 
         pch = 4, col="red", cex = 0.8 )
  segments(x0 = estXY[, 1], y0 = estXY[, 2],
           x1 = actualXY[, 1], y1 = actualXY[ , 2],
           lwd = 2, col = "grey")
  segments(7.8, 2.3, 7.8, -3.4,
           lwd = 1, col = "black")
  segments(11.3,2.3, 11.3,-3.4, 
           lwd = 1, col = "black")
  segments(16.1,2.3, 16.1,-3.4,
           lwd = 1, col = "black")
  segments(19.8,2.3, 19.8,-3.4,
           lwd = 1, col = "black")
  segments(22.1,2.3, 22.1,-3.4,
           lwd = 1, col = "black")
  segments(24.4,2.3, 24.4,-3.4,
           lwd = 1, col = "black")
  segments(28,2.3, 28,-3.4,
           lwd = 1, col = "black")
  segments(30.3,2.3, 30.3,-3.4,
           lwd = 1, col = "black")
  segments(9,14.2, 9,9.7,
           lwd = 1, col = "black")
  segments(11.5,14.2, 11.5,9.7,
           lwd = 1, col = "black")
  segments(13.8,14.2, 13.8,9.7, 
           lwd = 1, col = "black")
  segments(16.2,14.2, 16.2,9.7,
           lwd = 1, col = "black")
  segments(13.8,14.2, 13.8,9.7, 
           lwd = 1, col = "black")
  segments(19.7,14.2, 19.7,9.7,
           lwd = 1, col = "black")
  segments(22.1,14.2, 22.1,9.7,
           lwd = 1, col = "black")
  segments(25.6,14.2, 25.6,9.7,
           lwd = 1, col = "black")
  segments(28.1,14.2, 28.1,9.7,
           lwd = 1, col = "black")
  segments(30.5,14.2, 30.5,9.7,
           lwd = 1, col = "black")
  segments(3,9.7, 33.8,9.7,
           lwd = 1, col = "black")
  segments(3,14.2, 33.8,14.2,
           lwd = 1, col = "black")
  segments(3,2.3, 33.8,2.3,
           lwd = 1, col = "black")
  segments(3,-3.4, 33.8,-3.4,
           lwd = 1, col = "black")
  segments(3,2.3, 3,-3.4,
           lwd = 1, col = "black")
  segments(3,9.7, 3,14.2,
           lwd = 1, col = "black")
  segments(33.8,-3.4, 33.8,14.2,
           lwd = 1, col = "black")
  polygon(x = c(3, 3, 7.9, 7.9),  
          y = c(4, 6.4, 6.4, 4),    
          border = "black",             
          lwd = 1)                   
  polygon(x = c(14, 14, 19.9, 19.9),  
          y = c(4, 6.4, 6.4, 4),    
          border = "black",             
          lwd = 1)                   
  polygon(x = c(27, 27, 31, 31),  
          y = c(4, 6.4, 6.4, 4),    
          border = "black",             
          lwd = 1)                   
  segments(-0.5,-1.2, -0.5,14.2,
           lwd = 1, col = "black")
  segments(-0.5,-1.2, 3, -1.2,
           lwd = 1, col = "black")
  segments(-0.5,14.2, 3,14.2,
           lwd = 1, col = "black")
}


#--------------------------------------------------------------#
#-------------------------Step 5: Data Plot--------------------#
#--------------------------------------------------------------#

trainPoints <- IPS_trainingData[IPS_trainingData$angle == 0 & 
                                  IPS_trainingData$MAC == "00:0f:a3:39:e1:c0", c("posX", "posY")]

pdf(file="Plot-K1FloorPlan.pdf", width = 10, height = 7)
oldPar <- par(mar = c(1, 1, 1, 1))
floorErrorMap(estXYk1, IPS_testingData[ , c("posX","posY")], 
              trainPoints = trainPoints, AP = AP_Loc[2:3])
par(oldPar)
dev.off()

pdf(file="Plot-K3FloorPlan.pdf", width = 10, height = 7)
oldPar <- par(mar = c(1, 1, 1, 1))
floorErrorMap(estXYk3, IPS_testingData[ , c("posX","posY")], 
              trainPoints = trainPoints, AP = AP_Loc[2:3])
par(oldPar)
dev.off()

pdf(file="Plot-K5FloorPlan.pdf", width = 10, height = 7)
oldPar <- par(mar = c(1, 1, 1, 1))
floorErrorMap(estXYk5, IPS_testingData[ , c("posX","posY")], 
              trainPoints = trainPoints, AP = AP_Loc[2:3])
par(oldPar)
dev.off()

#------------------------------------------------------------------#
#------------------------Step 7: RSSI Heat Map---------------------#
#------------------------------------------------------------------#

#View signal strength by location (pick an angle, say 0 degrees, and view a topographic heat map of signal strength)
oneAPAngle <- subset(IPS_trainingData, MAC==AP_Loc[1, 1] & angle == 0)

smoothSS <- Tps(oneAPAngle[, c("posX", "posY")], oneAPAngle$avgSignal)
#Predict the value for a fitted surface at a grid of observed positions
vizSmooth <- predictSurface(smoothSS)
#Plot the predicted signal trength
plot.surface(vizSmooth, type = "C")
#Add locations where the measurements were taken
points(oneAPAngle$posX, oneAPAngle$posY, pch=19, col="grey", cex = 0.5)
points(IPS_testingData$posX, IPS_testingData$posY, pch=19, col="black", cex = 0.5)
points(AP_Loc$x, AP_Loc$y, pch=15, cex = 1)

#Wrap all this into a function so that you can draw heat map for any angle and Mac address
surfaceSS <- function(d, m, a) {
  oneAPAngle = subset(d, MAC == AP_Loc[m, 1] & angle == a)
  smoothSS <- Tps(oneAPAngle[, c("posX", "posY")], oneAPAngle$avgSignal)
  vizSmooth <- predictSurface(smoothSS)
  plot.surface(vizSmooth, type = "C")
  points(oneAPAngle$posX, oneAPAngle$posY, pch=19, col="grey", cex = 0.5)
  points(IPS_testingData$posX, IPS_testingData$posY, pch=19, col="black", cex = 0.5)
  points(AP_Loc$x, AP_Loc$y, pch=15, cex = 1)
}

surfaceSS_oneMac <- function(d, m) {
  oneAPAngle = subset(d, MAC == AP_Loc[m, 1])
  smoothSS <- Tps(oneAPAngle[, c("posX", "posY")], oneAPAngle$avgSignal)
  vizSmooth <- predictSurface(smoothSS)
  plot.surface(vizSmooth, type = "C")
  points(oneAPAngle$posX, oneAPAngle$posY, pch=19, col="grey", cex = 0.5)
  points(IPS_testingData$posX, IPS_testingData$posY, pch=19, col="black", cex = 0.5)
  points(AP_Loc$x, AP_Loc$y, pch=15, cex = 1)
}

surfaceSS_oneMac(IPS_trainingData, 1)
surfaceSS_oneMac(IPS_trainingData, 2)
surfaceSS_oneMac(IPS_trainingData, 3)
surfaceSS_oneMac(IPS_trainingData, 4)
surfaceSS_oneMac(IPS_trainingData, 5)
surfaceSS_oneMac(IPS_trainingData, 6)

#Tell R to plot matrix
parCur <- par(mfrow=c(2,2), mar=rep(2,4))
#Call surfaceSS 4 times to draw 4 heatmaps
mapply(surfaceSS, 
       d=list(data=IPS_trainingData), 
       m =1, 
       a=0)
mapply(surfaceSS, 
       d=list(data=IPS_trainingData), 
       m =1, 
       a=45)
mapply(surfaceSS, 
       d=list(data=IPS_trainingData), 
       m =1, 
       a=90)
mapply(surfaceSS, 
       d=list(data=IPS_trainingData), 
       m =1, 
       a=135)
#Reset plotting parameters for future plots
par(parCur)

parCur <- par(mfrow=c(2,2), mar=rep(2,4))
mapply(surfaceSS, 
       d=list(data=IPS_trainingData), 
       m =1, 
       a=180)
mapply(surfaceSS, 
       d=list(data=IPS_trainingData), 
       m =1, 
       a=225)
mapply(surfaceSS, 
       d=list(data=IPS_trainingData), 
       m =1, 
       a=270)
mapply(surfaceSS, 
       d=list(data=IPS_trainingData), 
       m =1, 
       a=315)
par(parCur)


## Using Plotly Heat Map
fig <- plot_ly(data = IPS_trainingData, 
               x = ~posX, y = ~posY, 
               type = 'scatter', 
               mode = 'markers', 
               symbols = c('circle','circle','sqaure'),
               color = I("grey"),
               alpha = 0.65 ) %>% 
  add_trace(data = IPS_testingData, 
            x = ~posX, y = ~posY, 
            type = 'scatter', 
            mode = 'markers', 
            color = I("black"),
            alpha = 0.65 )%>% 
  add_trace(data = oneAPAngle, 
            x = ~posX, y = ~posY, z = ~avgSignal,   
            type = "heatmap") %>% 
  add_trace(data = AP_Loc, 
            x = ~x, y = ~y, 
            type = 'scatter', 
            mode = 'markers', 
            color = I('green'),
            alpha = 0.65 ) %>% 
  layout(showlegend = FALSE)

fig

surfaceSS_oneMac(IPS_trainingData, 1)
surfaceSS_oneMac(IPS_trainingData, 2)
surfaceSS_oneMac(IPS_trainingData, 3)
surfaceSS_oneMac(IPS_trainingData, 4)
surfaceSS_oneMac(IPS_trainingData, 5)
surfaceSS_oneMac(IPS_trainingData, 6)

