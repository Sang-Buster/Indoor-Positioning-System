#--------------------------------------------------------------#
#--------------------Step 5: Data Visualization ---------------#
#--------------------------------------------------------------#

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
library(plotly)     # User interactive plots
library(fields)     # Used for drawing heat maps

set.seed(27)        # make random results reproducible

WD <- getwd()
setwd(WD)
remove(WD)

# Load Data
load("clean_data/IPS_Offline.RData")
load("clean_data/IPS_Online.RData")
load("clean_data/IPS_trainingData.RData")
load("clean_data/IPS_testingData.RData")
load("clean_data/IPS_AP_Locations.RData")

# Load Functions
load("clean_data/Fun-Ori2Angle.Rdata")
load("clean_data/Fun-reshapeSS.RData")
load("clean_data/Fun-selectTrain.Rdata")
load("clean_data/Fun-findNN.Rdata")
load("clean_data/Fun-predXY.Rdata")

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

#Wrap all this into a function so that you can draw heat map for Mac address
surfaceSS_oneMac <- function(d, m) {
  oneAP = subset(d, MAC == AP_Loc[m, 1])
  smoothSS <- Tps(oneAP[, c("posX", "posY")], oneAP$avgSignal)
  vizSmooth <- predictSurface(smoothSS)
  plot.surface(vizSmooth, xlim=c(-1, 35), ylim=c(-3, 14.5))
  points(oneAP$posX, oneAP$posY, pch=19, col="grey", cex = 0.5)
  points(IPS_testingData$posX, IPS_testingData$posY, pch=19, col="black", cex = 0.5)
  points(AP_Loc$x[m], AP_Loc$y[m], pch=15, cex = 1)
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

surfaceSS_oneMac(IPS_trainingData, 1)
surfaceSS_oneMac(IPS_trainingData, 2)
surfaceSS_oneMac(IPS_trainingData, 3)
surfaceSS_oneMac(IPS_trainingData, 4)
surfaceSS_oneMac(IPS_trainingData, 5)
surfaceSS_oneMac(IPS_trainingData, 6)

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

varList=c("posX", "posY")

actualXY <- IPS_testingData[ , varList]

estXYk1 <- as.data.frame(estXYk1)
estXYk3 <- as.data.frame(estXYk3)
estXYk5 <- as.data.frame(estXYk5)

errorDist <- function(est_data, act_data){
  dist=sqrt((act_data$posX-est_data$posX)^2+(act_data$posY-est_data$posY)^2)
}

dist <- errorDist(est_data = estXYk1, act_data = actualXY)

sum(dist)/length(dist)
median(dist)
