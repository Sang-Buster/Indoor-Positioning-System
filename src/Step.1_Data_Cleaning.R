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


#--------------------------------------------------------#
#------------------Step 3: Data Saving-------------------#
#--------------------------------------------------------#

save(IPS_offline_Data, file = "IPS_offline.RData")
save(IPS_trainingData, file = "IPS_trainingData.RData")
save(IPS_online_Data,  file = "IPS_online.RData")
save(IPS_testingData,  file = "IPS_testingData.RData")
save(AP_Loc,           file = "clean_data/IPS_AP_Locations.RData")
save(Ori.to.Angle,     file = "clean_data/Fun-Ori2Angle.RData")
