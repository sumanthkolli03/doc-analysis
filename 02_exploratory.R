#Project #2 USGS DOC Data

##--------------------------------------
## Install and load any needed libraries
##--------------------------------------

library(fields)
library(lattice)


##--------------------------------------
## Load the data
##--------------------------------------

load("DOC_baseFlow_weightedAvg_and_predictors.RData")
ls()

#This is a really long name for a dataset, so this command just renames it.
data <- DOC_baseFlow_weightedAvg_and_predictors
dim(data)
colnames(data)

y_response <- data[,"meanDOC"]

year.obs <- table(data$Year)
plot(1984:2012, year.obs, type="b", pch=19, xlab="Year", ylab="Number of Observations")
barchart(year.obs, horizontal=FALSE, col="gray", xlab="Year", ylab="Number of Observations")

######################
#Plot the locations
######################

plot(data$Longitude, data$Latitude, pch=19, col=rgb(0,0,0,.1), xlim=c(-109,-102), ylim=c(37,42))
US(add=T)
text(-108, 37.5, "Colorado", cex=2)



######################
#Creating Matrix of Predictors
######################

#Aspect Ratio: indicates the predominant aspect, which is a categorical value with nine different classes: North, Northeast, East, Southeast, South, Southwest,West, Northwest, and Flat

xFactors <- model.matrix(y_response ~ as.factor(data[,"aspect_predominant"]))[, -1]

colNames_xFactors <- c("aspect_predominant_1")
for (index in 2:8)
{
  colNames_xFactors <- c(colNames_xFactors,paste("aspect_predominant_",as.character(index),sep=""))
}

colnames(xFactors) <- colNames_xFactors

#Collecting the aspect ratio variables with all of the others needed
#Using one fire predictor  (column numbers for fire predictors are 6, 7, and 8)
X_predictors_unStandardized <- as.matrix( cbind( DOC_baseFlow_weightedAvg_and_predictors[,c(2,6,9:30,32)], xFactors) )

head(X_predictors_unStandardized)
dim(X_predictors_unStandardized)
class(X_predictors_unStandardized)
dataFrame_X_predictors <- as.data.frame(X_predictors_unStandardized)


######################
#Standardizing the Predictors 
#(Generally a good idea before fitting a linear model)
######################

X_predictors_means <- colMeans(X_predictors_unStandardized)
X_predictors_sd <- apply(X_predictors_unStandardized, MARGIN=2, 'sd')

time_index <- 			1
fire_indices <- 		2
MPB_indecies <- 		3:5
landCover_indices <- 	6:8
temp_indices <- 		9:14
precipSnow_indices <- 	15:20
soil_indices <- 		21:23
elevation_index <- 		24
wasteWater_index <- 	25
aspec_indices <- 		26:33
X_indices_subtractMean <- c(time_index, temp_indices, precipSnow_indices, soil_indices, elevation_index, wasteWater_index)
X_indices_scaleBySD <- c(fire_indices)

X_predictors_standardized <- X_predictors_unStandardized
X_predictors_standardized[,X_indices_subtractMean] <- sweep(X_predictors_unStandardized[,X_indices_subtractMean],MARGIN=2,X_predictors_means[X_indices_subtractMean],'-')
  
colnames(X_predictors_standardized)
dataFrame_X_standardized <- as.data.frame(X_predictors_standardized)

######################
#Some scatterplots between DOC
#and standardized predictors
######################
  
plot(dataFrame_X_standardized$landCover_developedFraction, y_response, pch=19, col=rgb(0,0,0,.1), xlab="Fraction Developed Land Cover", ylab="DOC")  

plot(dataFrame_X_standardized$maxTemp_yearAvg, y_response, pch=19, col=rgb(0,0,0,.1), xlab="Yearly Average of Maximum Temperature", ylab="DOC")  


  