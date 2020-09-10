##### Activity 2 #####

# working with data in R
# intro to probability and summary statitistics
# characterize weather events

## working with data in R ##

#make a vector of tree heights in meters
heights <- c(30,41,20,22)
#convert to cm
heights_cm <- heights*100
heights_cm

#look at the first tree height
heights[1]

#look at the 2nd and 3rd tree heights
heights[2:3]

# R uses matrices for a lot of datasets and functions

#Functions(argument)

#set up a matrix with 2 columns and fill in by rows
#first argument is the vector of numbers to fill in the matrix
Mat<-matrix(c(1,2,3,4,5,6), ncol=2, byrow=TRUE)
Mat

#set up a matrix that fills in by columns
#first argument is the vector of numbers to fill in the matrix
Mat.bycol<-matrix(c(1,2,3,4,5,6), ncol=2, byrow=FALSE)
Mat.bycol

#subset the matrix to look at row 1, column2
Mat.bycol[1,2]
# [row, column] always
# if comma without number then looks at everything example
Mat.bycol[1,]
# shows all numbers in row one
Mat.bycol[,2]
# shows all numbers in column one

##### Working with Dataframes #####
#Dataframes are matrices where columns have names and
#rows contain observations for the same entity.

# data downloaded from NOAA for extreme weather events

datW <- read.csv("C:\\Users\\Chloe\\Documents\\R\\GitHub\\GEOG331\\noaa_weather\\2011124.csv")
datW

#get more information about the dataframe
str(datW)
ncol(datW)
nrow(datW)

# change the variable type to official date for date column

datW$dateF <- as.Date(datW$DATE, "%Y-%m-%d")

# add column for date as year

datW$year <- as.numeric(format(datW$dateF, "%Y"))

##### Question 2 #####

# create vectors of type factor, integer, numeric, and character

numeric <- as.numeric(c(13.3,20.5,30.4,65.8,100.3))
integer <- as.integer(c(13.3,20.5,30.4,65.8,100.3))
character <- as.character(c("yes", "no", "yes", "no", "yes"))
facter <- as.factor(character)

##### Descriptive and Summary Statistics #####
# find all unique sites
levels(datW$NAME)
unique(datW$NAME)

# find the mean max temperature for Aberdeen
mean(datW$TMAX[datW$NAME == "ABERDEEN, WA US"])

# receieve an NA result because there are missing data points, remove them
mean(datW$TMAX[datW$NAME == "ABERDEEN, WA US"],na.rm= "T")

# calculate the average daily temperature
# find the mean of the min and max
#calculate the average daily temperature
#This temperature will be halfway between the minimum and maximum temperature
datW$TAVE <- datW$TMIN + ((datW$TMAX-datW$TMIN)/2)

#get the mean across all sites
#the by function is a list of one or more variables to index over.
#FUN indicates the function we want to use
#if you want to specify any function specific arguments use a comma and add them after the function
#here we want to use the na.rm arguments specific to 
averageTemp <- aggregate(datW$TAVE, by=list(datW$NAME), FUN="mean",na.rm=TRUE)
averageTemp

#change the automatic output of column names to be more meaningful
#note that MAAT is a common abbreviation for Mean Annual Air Temperature
colnames(averageTemp) <- c("NAME","MAAT")
averageTemp

# names are long, let's change them to a factor number to reference
#convert level to number for factor data type
#will have to reference the level output or look at the row of data to see character
datW$siteN <- as.numeric(as.factor(datW$NAME))

# making a histogram

#make a histogram for the first site in our levels
#main= is the title name argument.
#Here you want to paste the actual name of the factor not the numeric index
#since that will be more meaningful. 
hist(datW$TAVE[datW$siteN == 1],
     freq=FALSE, 
     main = paste(levels(datW$NAME)[1]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="grey50",
     border="white")
### Question 3

help(hist)
#make a histogram for the first site in our levels, Aberdeen
#main= is the title name argument.
#Here you want to paste the actual name of the factor not the numeric index
#since that will be more meaningful. 
par(mfrow=c(2,2))
print(unique(datW$NAME)[1])
hist(datW$TAVE[datW$siteN == 1],
     freq=FALSE, 
     main = paste(unique(datW$NAME)[1]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="grey50",
     border="white")
#add mean line with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "tomato3",
       lwd = 3)
#add standard deviation line below the mean with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE) - sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)
#add standard deviation line above the mean with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE) + sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)

# SITE 2

hist(datW$TAVE[datW$siteN == 2],
     freq=FALSE, 
     main = paste(unique(datW$NAME)[2]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="grey50",
     border="white")
#add mean line with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 2],na.rm=TRUE), 
       col = "tomato3",
       lwd = 3)
#add standard deviation line below the mean with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 2],na.rm=TRUE) - sd(datW$TAVE[datW$siteN == 2],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)
#add standard deviation line above the mean with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 2],na.rm=TRUE) + sd(datW$TAVE[datW$siteN == 2],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)

# SITE 3

hist(datW$TAVE[datW$siteN == 3],
     freq=FALSE, 
     main = paste(unique(datW$NAME)[3]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="grey50",
     border="white")
#add mean line with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 3],na.rm=TRUE), 
       col = "tomato3",
       lwd = 3)
#add standard deviation line below the mean with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 3],na.rm=TRUE) - sd(datW$TAVE[datW$siteN == 3],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)
#add standard deviation line above the mean with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 3],na.rm=TRUE) + sd(datW$TAVE[datW$siteN == 3],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)

# SITE 4

hist(datW$TAVE[datW$siteN == 4],
     freq=FALSE, 
     main = paste(unique(datW$NAME)[4]),
     xlab = "Average daily temperature (degrees C)", 
     ylab="Relative frequency",
     col="grey50",
     border="white")
#add mean line with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 4],na.rm=TRUE), 
       col = "tomato3",
       lwd = 3)
#add standard deviation line below the mean with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 4],na.rm=TRUE) - sd(datW$TAVE[datW$siteN == 4],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)
#add standard deviation line above the mean with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$TAVE[datW$siteN == 4],na.rm=TRUE) + sd(datW$TAVE[datW$siteN == 4],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)

# normal distribution
par(mfrow=c(1,1))
#make a histogram for the first site in our levels
#main= is the title name argument.
#Here you want to paste the actual name of the factor not the numeric index
#since that will be more meaningful. 
#note I've named the histogram so I can reference it later
h1 <- hist(datW$TAVE[datW$siteN == 1],
           freq=FALSE, 
           main = paste(unique(datW$NAME)[1]),
           xlab = "Average daily temperature (degrees C)", 
           ylab="Relative frequency",
           col="grey50",
           border="white")
#the seq function generates a sequence of numbers that we can use to plot the normal across the range of temperature values
x.plot <- seq(-10,30, length.out = 100)
#the dnorm function will produce the probability density based on a mean and standard deviation.

y.plot <-  dnorm(seq(-10,30, length.out = 100),
                 mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
                 sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))
#create a density that is scaled to fit in the plot  since the density has a different range from the data density.
#!!! this is helpful for putting multiple things on the same plot
#!!! It might seem confusing at first. It means the maximum value of the plot is always the same between the two datasets on the plot. Here both plots share zero as a minimum.
y.scaled <- (max(h1$density)/max(y.plot)) * y.plot

#points function adds points or lines to a graph  
#the first two arguements are the x coordinates and the y coordinates.

points(x.plot,
       y.scaled, 
       type = "l", 
       col = "royalblue3",
       lwd = 4, 
       lty = 2)


#### calculating probabilities based on normal distributions

pnorm(0,
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))

pnorm(5,
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))

# find the probability of a temperature between 0-5
pnorm(5,
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE)) - pnorm(0,
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))

## all temperatures above 20
# note that pnorm(20) gives below 20, so subtract from 1 to get true probability
1- pnorm(20,
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))

# qnorm gives the value at which a certain probability is met

qnorm(0.95,
      mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
      sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))


### Question 6
1- pnorm(qnorm(0.95,
               mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE),
               sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE)),
         mean(datW$TAVE[datW$siteN == 1],na.rm=TRUE) +4,
         sd(datW$TAVE[datW$siteN == 1],na.rm=TRUE))

##### Question 7 #####
names(datW)
par(mfrow=c(1,1))
print(unique(datW$NAME)[1])
hist(datW$PRCP[datW$siteN == 1],
     freq=FALSE, 
     main = paste(unique(datW$NAME)[1]),
     xlab = "Average daily Rainfall (mm)", 
     ylab="Relative frequency",
     col="grey50",
     border="white")
#add mean line with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$PRCP[datW$siteN == 1],na.rm=TRUE), 
       col = "tomato3",
       lwd = 3)
#add standard deviation line below the mean with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$PRCP[datW$siteN == 1],na.rm=TRUE) - sd(datW$PRCP[datW$siteN == 1],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)
#add standard deviation line above the mean with red (tomato3) color
#and thickness of 3
abline(v = mean(datW$PRCP[datW$siteN == 1],na.rm=TRUE) + sd(datW$PRCP[datW$siteN == 1],na.rm=TRUE), 
       col = "tomato3", 
       lty = 3,
       lwd = 3)

##### QUestion 8 - 10 #####

# sum function to get total precipitation by site by year
names(datW)

library(dplyr)
datW.nona <- na.omit(datW)
precip <- datW.nona %>% group_by(siteN, year) %>% summarise(precip_sum=sum(PRCP))
precip
paste(unique(datW$NAME)[2])
hist(precip$precip_sum[precip$siteN == 2],
     freq=FALSE, breaks=15,
     main = "LIVERMORE, CA US",
     xlab = "Yearly Rainfall (mm)", 
     ylab="Relative frequency",
     col="grey50",
     border="white")

#add mean line with red (tomato2) color
#and thickness of 2
abline(v = mean(precip$precip_sum[precip$siteN == 2],na.rm=TRUE), 
       col = "tomato2",
       lwd = 2)
#add standard deviation line below the mean with red (tomato2) color
#and thickness of 2
abline(v = mean(precip$precip_sum[precip$siteN == 2],na.rm=TRUE) - sd(precip$precip_sum[precip$siteN == 2],na.rm=TRUE), 
       col = "tomato2", 
       lty = 2,
       lwd = 2)
#add standard deviation line above the mean with red (tomato2) color
#and thickness of 2
abline(v = mean(precip$precip_sum[precip$siteN == 2],na.rm=TRUE) + sd(precip$precip_sum[precip$siteN == 2],na.rm=TRUE), 
       col = "tomato2", 
       lty = 2,
       lwd = 2)

##### Question 9 #####

meanprecip <- aggregate(precip$precip_sum, by=list(precip$siteN), FUN="mean",na.rm=TRUE)
meanprecip
averageTemp
