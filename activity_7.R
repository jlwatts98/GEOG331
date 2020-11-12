##### Activity 7 #####
##### load in the data #####
#install.packages(c("caret","randomForest"))
library(raster)
library(sp)
library(rgdal)
library(caret)
library(randomForest)
# set the working directory
dirR <- setwd("~/R/GitHub/GEOG331/oneida/oneida")

#read in Sentinel data

rdatB2 <- raster(paste0(dirR,"/sentinel/T18TVN_20190814T154911_B02_20m.tif"))
rdatB3 <- raster(paste0(dirR,"/sentinel/T18TVN_20190814T154911_B03_20m.tif"))
rdatB4 <- raster(paste0(dirR,"/sentinel/T18TVN_20190814T154911_B04_20m.tif"))
rdatB5 <- raster(paste0(dirR,"/sentinel/T18TVN_20190814T154911_B05_20m.tif"))
rdatB6 <- raster(paste0(dirR,"/sentinel/T18TVN_20190814T154911_B06_20m.tif"))
rdatB7 <- raster(paste0(dirR,"/sentinel/T18TVN_20190814T154911_B07_20m.tif"))
rdatB8 <- raster(paste0(dirR,"/sentinel/T18TVN_20190814T154911_B08_20m.tif"))
rdatB11 <- raster(paste0(dirR,"/sentinel/T18TVN_20190814T154911_B11_20m.tif"))
rdatB12 <- raster(paste0(dirR,"/sentinel/T18TVN_20190814T154911_B12_20m.tif"))
clouds <- raster(paste0(dirR,"/sentinel/MSK_CLDPRB_20m.tif"))

#stack all raster data
allbands <- stack(rdatB2,rdatB3,rdatB4,rdatB5,rdatB6,rdatB7, rdatB8,rdatB11, rdatB12,clouds)

#read in validation data
#here verbose=FALSE hiddes
algae <- readOGR(paste0(dirR,"/Oneida/algae.shp"), verbose=FALSE)
agri <- readOGR(paste0(dirR,"/Oneida/agriculture.shp"), verbose=FALSE)
built <- readOGR(paste0(dirR,"/Oneida/built.shp"), verbose=FALSE)
forest <- readOGR(paste0(dirR,"/Oneida/forest.shp"), verbose=FALSE)
water <- readOGR(paste0(dirR,"/Oneida/water.shp"), verbose=FALSE)
wetlands <- readOGR(paste0(dirR,"/Oneida/wetlands.shp"), verbose=FALSE)

# get rid of the clouds
#create function called clouds F
cloudsF <- function(rast){ #define function & parenthesis gives names of function arguments for the user to input
  ifelse(rast > 60,NA,1) #function will run ifelse statement for the rast input
  #outputs will be NA if the raster value is more than 60 and 1 if not
  
} #end 
#now apply this function to every cell in the raster using calc
CloudFlag <- calc(allbands[[10]], cloudsF)

# multiply this raster by all the other rasters
allbandsCloud <- list()
for(i in 1:9){
  allbandsCloud[[i]] <- CloudFlag * allbands[[i]]
}
#new stack
allbandsCloudf <- stack(allbandsCloud[[1]],allbandsCloud[[2]],allbandsCloud[[3]],allbandsCloud[[4]],
                        allbandsCloud[[5]],allbandsCloud[[6]],allbandsCloud[[7]],allbandsCloud[[8]],
                        allbandsCloud[[9]])

##### Create validation and Training data #####
#if I run this without setting the seed it will be different every time
#randomly choose 60 elements in the vector of 120 elements
sample(seq(1,120),60)
sample(seq(1,120),60)
#set seed so samples always the same
set.seed(12153)
#randomly choose 60 elements in the vector of 120 elements
sample(seq(1,120),60)
set.seed(12153)
#randomly choose 60 elements in the vector of 120 elements
sample(seq(1,120),60)
#set seed so samples always the same
set.seed(12153)
#randomly select the data in each dataset to be  used
sampleType <- rep("train",120)
#samples to randomly convert to validation data
sampleSamp <- sample(seq(1,120),60)
#convert these random samples from training to validation
sampleType[sampleSamp] <- "valid"

#set up table with coordinates and data type (validate or train) for each point
landExtract <-  data.frame(landcID = rep(seq(1,6),each=120),
                           x=c(algae@coords[,1],water@coords[,1],agri@coords[,1],built@coords[,1],forest@coords[,1],wetlands@coords[,1] ),
                           y=c(algae@coords[,2],water@coords[,2],agri@coords[,2],built@coords[,2],forest@coords[,2],wetlands@coords[,2] ))
#add sample type
landExtract$sampleType <- rep(sampleType, times=6)

#create id table that gives each landcover an ID
landclass <- data.frame(landcID= seq(1,6),
                        landcover = c("algal bloom", "open water","agriculture","built","forest","wetlands"))

#extract raster data at each point
#using point coordinates
rasterEx <- data.frame(extract(allbandsCloudf,landExtract[,2:3]))
#give names of bands
colnames(rasterEx) <- c("B2","B3","B4","B5","B6","B7","B8","B11","B12")

#combine point information with raster information
dataAll <- cbind(landExtract,rasterEx)
#preview
head(dataAll)

#remove missing data
dataAlln <- na.omit(dataAll)

#subset into two different data frames
trainD <- dataAlln[dataAlln$sampleType == "train",]
validD <- dataAlln[dataAlln$sampleType == "valid",]

#Kfold cross validation
tc <- trainControl(method = "repeatedcv", # repeated cross-validation of the training data
                   number = 10, # number 10 fold
                   repeats = 10) # number of repeats
#####random forests#####
#Typically square root of number of variables
rf.grid <- expand.grid(mtry=1:sqrt(9)) # number of variables available for splitting at each tree node

# Train the random forest model to the Sentinel-2 data
#note that caret:: will make sure we use train from the caret package
rf_model <- caret::train(x = trainD[,c(5:13)], #digital number data
                         y = as.factor(trainD$landcID), #land class we want to predict
                         method = "rf", #use random forest
                         metric="Accuracy", #assess by accuracy
                         trainControl = tc, #use parameter tuning method
                         tuneGrid = rf.grid) #parameter tuning grid
#check output
rf_model
##### Predict for the whole raster #####
# Change name in raster stack to match training data
names(allbandsCloudf) <- c("B2","B3","B4","B5","B6","B7","B8","B11","B12")
# Apply the random forest model to the Sentinel-2 data
rf_prediction <- raster::predict(allbandsCloudf, model=rf_model)
#view predictions
plot(rf_prediction)

#landcover class names
landclass

# hard to look at this without the names of the predictors

#set up categorical colors
landclass$cols <-c("#a6d854","#8da0cb","#66c2a5",
                   "#fc8d62","#ffffb3","#ffd92f")
#make plot and hide legend
plot(rf_prediction,
     breaks=seq(0,6), 
     col=landclass$cols ,
     legend=FALSE, axes=FALSE)
legend("bottomleft", paste(landclass$landcover),
       fill=landclass$cols ,bty="n",horiz = T) 

## Let's validate the prediction
#get validation data from raster by extracting 
#cell values at the cell coordinates
rf_Eval <- extract(rf_prediction, validD[,2:3])

#make the confusion matrix
rf_errorM <- confusionMatrix(as.factor(rf_Eval),as.factor(validD$landcID))
#add landcover names
colnames(rf_errorM$table) <- landclass$landcover
rownames(rf_errorM$table) <- landclass$landcover
#view the matrix
rf_errorM$table

#look at the overall accuracy
rf_errorM$overall


##### Questions #####
# 1. Sample selects randomly from a vector. All you need to give to the function is the vector
# and the number you would like to sample from the vector.
# Set.seed sets the exact way the the sample (or any other randomized function) works.
# Extract works to take values from a raster or group of rasters at certain points.
# all you need to input is the raster layers and points that you want to extract.

# 2. To implement a machine learning classification, you must first prepare the data such
# it is all of the proper type, all of the proper GCS and PCS. Then the data need to be split
# into training and validating subsets to avoid overfitting the data and overestimating the
# accuracy of the model. Then the machine learning model needs to be calibrated (tuned) by
# slightly changing the parameters of the model until it predicts most accurately. This can be
# done by hand or by using a package that will do it for you. The the tuned model must be run.
# After running the model, one must use the validation data to test the accuracy of the model.

# 3. The producer's accuracy is calculated by dividing the number of correctly classified
# predictions by the sum of all the other values in the same column. The user's accuracy
# is calculated by dividing the correctly classified predictions by the sum of the all other
# values in the same row. Producer's accuracy represents how well the real world is classified.
# ie. how the producer did in creating the map. The User's accurracy reflects how well the map
# performs on the ground. How well the classifications reflect things on the ground. Ie. how
# useful is the map for a user.

# 4. The open water and the algal bloom are highly accurately classified, with 100% producer's
# and user's accuracy, which is good considering the reason for creating the map. But
# forest, agriculture, and wetlands, which are more spectrally similar, are a little more
# error prone. However, there does not seem to be a systematic bias toward any one of the
# landcover types. This is because we tuned the model well.

##### GITHUB Link #####

# https://github.com/jlwatts98/GEOG331/blob/master/activity_7.R
