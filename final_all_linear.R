load("DOC_baseFlow_weightedAvg_and_predictors.RData")
data <- DOC_baseFlow_weightedAvg_and_predictors
y <- data[,"meanDOC"]
set.seed(0)


## create a function to calculate r^2
rsqr <- function(prediction, observed){
  residuals <- observed - prediction
  SSR <- sum(residuals ** 2)
  mean_doc <- mean(observed)
  SST <- sum((observed - mean_doc) ** 2)
  rsqr <- 1 - (SSR/SST)
}

## Make a testing/training set
n <- nrow(data)
p <- sample(1:n) 
train = data[p[1:(n/2)], ]
test = data[p[((n/2)+1):n], ]

## all vars linear model (probably the bad one)
allm <- lm(meanDOC ~ ., data=train)
summary(allm)
## r^2 of 0.6587, but as expected, there are a lot of variables that we have to
#remove according to significance alone

params <- colnames(data)
params <- params[
  ! params %in% c(
    "newLocationID", "Year", "Longitude",
    "MPB_greenFraction", "MPB_redFraction", "MPB_grayFraction",
    "landCover_agriculturalFraction", "minTemp_baseFlow", 
    "minTemp_nonBaseFlow", "minTemp_yearAvg", "maxTemp_baseFlow",
    "maxTemp_nonBaseFlow", "maxTemp_yearAvg", "precip_baseFlow",
    "precip_yearAvg", "soil_Afraction", "soil_Cfraction",
    "SWE_baseFlow", "SWE_nonBaseFlow", "SWE_yearAvg"
  )
]

## now we rerun the model on only the initially significant variables
## we will cross-validate using training/testing, then try to eliminate
## more variables using background information

signifm <- lm(meanDOC ~ ., data=train[, params])
summary(signifm)

## r^2 of 0.6417 on training

test_pred_sig <- predict(signifm,newdata=test[, params])
rsqr_test_sig<- rsqr(test_pred_sig, test$meanDOC)

## r^2 is 0.5375 on testing data,
## training set r^2 is much higher than testing set r^2, which
## means the model is overfitted

#now we will try to remove variables that are both insignificant AND
#that we feel aren't necessary by reading the background info
params <- colnames(data)
params <- params[
  ! params %in% c(
    "newLocationID", "Year", "Longitude", "Latitude",
    "fire_normRdNBR_5yDecay", "fire_normRdNBR_15yDecay",
    "aspect_predominant",
    "precip_nonBaseFlow", "landCover_forestFraction",
    "MPB_greenFraction", "MPB_redFraction", "MPB_grayFraction",
    "landCover_agriculturalFraction", "minTemp_baseFlow", 
    "minTemp_nonBaseFlow", "minTemp_yearAvg", "maxTemp_baseFlow",
    "maxTemp_nonBaseFlow", "maxTemp_yearAvg", "precip_baseFlow",
    "precip_yearAvg", "soil_Afraction", "soil_Cfraction",
    "SWE_baseFlow", "SWE_nonBaseFlow", "SWE_yearAvg"
  )
]

## What we removed:
## aspect_predominant, 
## two of the fire_norms (now only using the 10 year average),
## removing forest fraction and keeping land fraction (add to equal 1)
## precipitation_nonBaseFlow and Latitude (seemingly unneeded)

# we will also be standardizing the variables at this point, to try and get
# a clearer model

cutdata <- data[, params]
cuttrain = cutdata[p[1:(n/2)], ]
cuttest = cutdata[p[((n/2)+1):n], ]

dep_ndx <- which(colnames(cutdata) == 'meanDOC')

for (v in 1:ncol(cutdata)) {
  if (v == dep_ndx) { next }
  s <- sd(cuttrain[,v])
  cuttrain[,v] <- cuttrain[,v] / s
  cuttest[,v]  <- cuttest[,v] / s
}

cutm <- lm(meanDOC ~ ., data=cuttrain)
summary(cutm)
test_pred_cut <- predict(cutm,newdata=cuttest)
(rsqr_test_cut<- rsqr(test_pred_cut, cuttest$meanDOC))

##overall it seems the fire_norm is inconsistent in whether or not it is useful.
##r^2 for this model is 0.6148, and still 0.5685 for the testing, meaning the 
##model is still slightly overfitted, but better than the all variables.
