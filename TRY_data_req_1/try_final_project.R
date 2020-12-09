##### Final Project #####

library(taxize)
library(devtools)
library(CoordinateCleaner)
library(dplyr)
library(ggplot2)
library(rgbif)
library(sp)

# read the data
setwd("~/R/GitHub/GEOG331/TRY_data_req_1")
try <- read.table("12256.txt", sep = "\t", header = T, comment.char = "",quote = "\"")
# group data
names(try)
unique(try$TraitID)
# subset for stomatal conductivity
try_cond <- dplyr::filter(try, TraitID == 45)
# summarize data and biuld a bar plot
try_condsumm <- try_cond %>% group_by(AccSpeciesName, TraitName) %>% summarise_all(funs(mean, sd), 
                                  na.rm = TRUE) %>%
  ggplot(aes(x = AccSpeciesName, y = StdValue_mean, fill = AccSpeciesName)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  labs(
    x = "Species",
    y = "Mean Stomatal Conductance",
    title = paste(
      "Stomatal Conductance by Species"
    )
  )
try_condsumm

# leaf dry mass/ freshleaf mass
try_mass <- dplyr::filter(try, TraitID == 47)
# summarize data and biuld a bar plot
try_masssumm <- try_mass %>% group_by(AccSpeciesName, TraitName) %>% summarise_all(funs(mean, sd), 
                                                                                   na.rm = TRUE) %>%
  ggplot(aes(x = AccSpeciesName, y = StdValue_mean, fill = AccSpeciesName)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  labs(
    x = "Species",
    y = "Mean Dry Mass/Leaf Fresh Mass",
    title = paste(
      "SDry Mass/Leaf Fresh Mass by Species"
    )
  )
try_masssumm
library(tidyverse)
# 719 xylem hydraulum vulnerability
try_xyl <- dplyr::filter(try, TraitID == 719)
try_xyl$StdValue <- as.integer(try_xyl$StdValue)
try_xyl <- drop_na(try_xyl$StdValue)
# summarize data and biuld a bar plot
try_xylsumm <- try_xyl %>% group_by(AccSpeciesName, TraitName) %>% summarise_all(funs(mean, sd), 
                                                                                   na.rm = TRUE) %>%
  ggplot(aes(x = AccSpeciesName, y = StdValue_mean, fill = AccSpeciesName)) +
  geom_bar(stat = "identity") +
  theme_classic() +
  labs(
    x = "Species",
    y = "Xylem Vulnerability",
    title = paste(
      "Xylem Vulnerability by Species"
    )
  )
try_xylsumm
library(tidyr)
try_s <- spread(try, ObservationID, StdValue)
try_s
# https://r4ds.had.co.nz/tidy-data.html#spreading-and-gathering

## change projects and data ###

##### Stomata Data #####
#https://nph.onlinelibrary.wiley.com/doi/full/10.1111/nph.15422
setwd("~/R/GitHub/GEOG331")
stomata <- read.table("stomata.txt", sep = "\t", header = T, comment.char = "",quote = "\"")
# Reorganize data
names(stomata)

stomden1 = stomata %>% dplyr::filter(DataID == 216)
stomden = stomata %>% dplyr::filter(TraitID == 63)
stomdist1 = stomata %>% dplyr::filter(DataID == 217)
stomdist = stomata %>% dplyr::filter(TraitID == 781)
stomcond1 = stomata %>% dplyr::filter(DataID == 314)
stomcond = stomata %>% dplyr::filter(TraitID == 106)
treatco2 = stomata %>% dplyr::filter(DataID == 324)
treatrh = stomata %>% dplyr::filter(DataID == 325)
radmeas = stomata %>% dplyr::filter(DataID == 340)
vpdmeas = stomata %>% dplyr::filter(DataID == 338)
tempmeas = stomata %>% dplyr::filter(DataID == 322)
co2meas = stomata %>% dplyr::filter(DataID == 323)
lat = stomata %>% dplyr::filter(DataID == 59)
lon = stomata %>% dplyr::filter(DataID == 60)

##### Stomatal Density #####


### change row names and join the data by observationid
stomden$density = stomden$StdValue
lat$lat = lat$StdValue
lon$lon = lon$StdValue
# adding latitude
denlat = inner_join(stomden, lat, by = "ObservationID")
# select and rename
denlat$species = denlat$SpeciesName.x
denlat$trait = denlat$TraitName.x
denlat$lat = denlat$StdValue.y
denlat = dplyr::select(denlat, c("ObservationID", "density", "species", "trait",
                                 "lat"))


# adding longitude
denloc = inner_join(denlat, lon, by = "ObservationID")
# reselect
denloc = dplyr::select(denloc, c("ObservationID", "density", "species", "trait",
                                 "lat", "lon"))
denloc = na.omit(denloc)


hist(denloc$density)
unique(denloc$species)
# a new dataframe for spatial
denloc1 = denloc

## plot lat and lon locations
library(maptools)
par(mfrow=c(1,1))
data(wrld_simpl)
plot(wrld_simpl)
plot(wrld_simpl, axes=TRUE, col="light yellow")
#restore the box around the map
box()
points(denloc$lon, denloc$lat, col= 'blue', pch = 20, cex = 1)


##### Stomatal Conductivity #####

# adding latitude
condlat = inner_join(stomcond, lat, by = "ObservationID")
# select and rename
condlat$species = condlat$SpeciesName.x
condlat$trait = condlat$TraitName.x
condlat$lat = condlat$StdValue.y
condlat$conduct = condlat$StdValue.x
condlat = dplyr::select(condlat, c("ObservationID", "conduct", "species", "trait",
                                 "lat"))
# adding longitude
condloc = inner_join(condlat, lon, by = "ObservationID")
# reselect
condloc = dplyr::select(condloc, c("ObservationID", "conduct", "species", "trait",
                                 "lat", "lon"))

condloc = subset(condloc, conduct > 0)

unique(condloc$species)
hist(condloc$conduct)

# plot

plot(wrld_simpl, axes=TRUE, col="light yellow")
# restore the box around the map
box()
points(condloc$lon, condloc$lat, 
       col= 'orange', pch = 20, cex = 1)

### add other important variables
condall = inner_join(condloc, vpdmeas, by = "ObservationID")

# rename
condall$vpd = condall$OrigValueStr

# reselect
condall = dplyr::select(condall, c("ObservationID", "conduct", "species", "vpd", "trait",
                                   "lat", "lon"))
# adding radiation
condall = inner_join(condall, radmeas, by = "ObservationID")

# rename
condall$par = condall$OrigValueStr

# reselect
condall = dplyr::select(condall, c("ObservationID", "conduct", "species", 
                                   "vpd", "par", "trait",
                                   "lat", "lon"))

# plot

plot(wrld_simpl, axes=TRUE, col="light yellow")
# restore the box around the map
box()
points(condall$lon, condall$lat, 
       col= 'orange', pch = 20, cex = 1)

hist(condall$conduct)

# found in the comments of the stomata dataset that the -9999 equals 2000 PAR
condall$par[condall$par == -9999] <- 2000 
# scatter plots
plot(condall$par, condall$vpd)
plot(condall$par, condall$conduct)
plot(condall$vpd, condall$conduct)




##### Stomatal Distribution #####

# adding latitude
distlat = inner_join(stomdist, lat, by = "ObservationID")
# select and rename
distlat$species = distlat$SpeciesName.x
distlat$trait = distlat$TraitName.x
distlat$lat = distlat$StdValue.y
distlat$distrib = distlat$OrigValueStr.x
distlat = dplyr::select(distlat, c("ObservationID", "distrib", "species", "trait",
                                   "lat"))
# adding longitude
distloc = inner_join(distlat, lon, by = "ObservationID")
# reselect
distloc = dplyr::select(distloc, c("ObservationID", "distrib", "species", "trait",
                                   "lat", "lon"))


unique(distloc$species)

# plot
# BETTER MAPS HERE
#https://www.r-spatial.org/r/2018/10/25/ggplot2-sf.html
plot(wrld_simpl, axes=TRUE, col="light yellow")
# restore the box around the map
box()
points(distloc$lon, distloc$lat, 
       col= "orange", pch = 20, cex = 1)

##### Climate Variables Extract #####
# importing climate rasters
library(raster)
predictors <- stack("predictors.tif")
# creating a spatial data frame
coordinates(denloc1) <- ~lon+lat
# set CRS
projection(denloc1) <- CRS('+proj=longlat +datum=WGS84')
# extract values of raster at each point
denclim <- extract(predictors, denloc1)
denclim = data.frame(denclim)

dentot = cbind(denclim, denloc)
# make into two different dataframes for high and low stomata
# make a vector of all cool
dentot$den_fact = rep("Low", 6676)
# find mean of temperature values
mean(dentot$density)
# change vactor value to high if greater than mean
dentot$den_fact[dentot$density > mean(dentot$density)] = "High"
dentothigh = subset(dentot, den_fact == "High")
dentotlow = subset(dentot, den_fact == "Low")
library(maptools)
par(mfrow=c(1,1))
data(wrld_simpl)
plot(wrld_simpl)
plot(wrld_simpl, axes=TRUE, col="light yellow")
#restore the box around the map
box()
points(dentotlow$lon, dentotlow$lat, col= 'blue', pch = 20, cex = 0.5)
points(dentothigh$lon, dentothigh$lat, col= 'red', pch = 20, cex = 0.5)



## stomatal distribution
distloc1 = distloc

# creating a spatial data frame
coordinates(distloc1) <- ~lon+lat
# set CRS
projection(distloc1) <- CRS('+proj=longlat +datum=WGS84')
# extract values of raster at each point
distclim <- extract(predictors, distloc1)
distclim = data.frame(distclim)

disttot = cbind(distclim, distloc)
# create new dataset without missing data
disttot <- na.omit(disttot)
# make distribution a factor
disttot$distrib = as.factor(disttot$distrib)
levels(disttot$distrib)
# make new dataframe with only no and yes
disttot = subset(disttot, distrib == "no" | distrib == "yes")
levels(disttot$distrib)
levels(disttot$distrib)[1] <- "Not specified"
str(disttot)
table(disttot$distrib)
disttot$distrib <- factor(disttot$distrib)
table(disttot$distrib)
### stomatal conductivity
condloc1 = condloc
# creating a spatial data frame
coordinates(condloc1) <- ~lon+lat
# set CRS
projection(condloc1) <- CRS('+proj=longlat +datum=WGS84')
# extract values of raster at each point
condclim <- extract(predictors, condloc1)
condclim = data.frame(condclim)

condtot = cbind(condclim, condloc)
# create new dataset without missing data
condtot <- na.omit(condtot)

# ONLY ONE SPECIES
albacond = subset(condtot, species == "Quercus alba")
albglm = lm(conduct ~ predictors.1+ predictors.2+ predictors.3 + predictors.4+ predictors.12,
            data = albacond)
summary(albglm)

# combine density and conductivity by species
dencond = inner_join(condtot, dentot, by = "species")
names(dencond)
# remove duplicate observations
dups <- duplicated(dencond[ ,"ObservationID.x"])
# remove duplicates
dencond <- dencond[!dups,]
# remove duplicate observations
dups <- duplicated(dencond[ ,48])
# remove duplicates
dencond <- dencond[!dups,]
dencond = na.omit(dencond)
# scatterplot of data
plot(dencond$density, dencond$conduct)
##### Analysis #####

# stomatal density #

### correlation matrix
library(corrplot)
library(caret)
library(Hmisc)
# use a dataframe with only numeric values
den_data.cor <- dentot[, c(1:20,22)]
# compute the correlation matrix
den_corr = cor(den_data.cor)
den_corr
mydata.rcorr = rcorr(as.matrix(den_corr))
mydata.coeff = mydata.rcorr$r
mydata.p = mydata.rcorr$P
corrplot(mydata.coeff)
palette = colorRampPalette(c("green", "white", "red")) (20)
heatmap(x = mydata.coeff, col = palette, symm = TRUE)

# plot
plot(dentot$predictors.12, dentot$density)
plot(dentot$predictors.14, dentot$density)
# preliminary regression
pre_lm = lm(density ~ predictors.14 * predictors.12, data = dentot)
summary(pre_lm)

# Assumptions and Diagnostics
hist(pre_lm$residuals, main="", xlab = "Residuals", freq = FALSE)
curve(dnorm(x,mean(pre_lm$residuals, sd(pre_lm$residuals))), add=TRUE)
shapiro.test(pre_lm$residuals)

# Plotting residuals
plot(fitted.values(pre_lm),residuals(pre_lm),
     xlab="Fitted Values", ylab="Residuals")
abline(h=0, lty=2)

# Plotting standardized residuals
plot(fitted.values(pre_lm),rstandard(pre_lm), ylim=c(-3.5,3.5),
     xlab="Fitted Values", ylab="Standardized Residuals")
abline(h=2, col="red", lty=2)
abline(h=-2, col="red", lty=2)
abline(h=3, col="red", lty=1)
abline(h=-3, col="red", lty=1)

# Plotting Cook's Distance
plot(fitted.values(pre_lm),cooks.distance(pre_lm),
     xlab="Fitted Values", ylab="Cook's Distance")
abline(h=1, col="red", lty=2)
abline(h=4/6672, col="blue", lty=2)
text(800, 0.0015, "4/n")
text(37.5, 1.1, "1")

library(gam)
library(mgcv)
dengam = gam(density ~ predictors.1 + predictors.2 + predictors.3 + predictors.4 +
                predictors.12 + predictors.14 + predictors.16, data = dentot)
summary(dengam)

# predict raster
predictors.subset = subset(predictors, c(1,2,3,4,12,14,16))

# run predict function
denpred = raster::predict(predictors.subset, dengam)
raster::plot(denpred)

# scatter plots
dentot$pc <- predict(prcomp(~predictors.14 + density, dentot))[,1]
ggplot(dentot, aes(predictors.14, density, color = pc)) +
  geom_point(shape = 16, size = 5, show.legend = FALSE, alpha = .4) +
  theme_minimal() +
  scale_color_gradient(low = "#0091ff", high = "#f0650e") +
  xlab("Precipitation of the Driest Month (mm)") +
  ylab("Stomatal Density (#/mm^2)")

dentot$pc <- predict(prcomp(~predictors.12 + density, dentot))[,1]
ggplot(dentot, aes(predictors.12, density, color = pc)) +
  geom_point(shape = 16, size = 5, show.legend = FALSE, alpha = .4) +
  theme_minimal() +
  scale_color_gradient(low = "#0091ff", high = "#f0650e") +
  xlab("Annual Precipitation (mm)") +
  ylab("Stomatal Density (#/mm^2)")

dentot$pc <- predict(prcomp(~predictors.1 + density, dentot))[,1]
ggplot(dentot, aes(predictors.1, density, color = pc)) +
  geom_point(shape = 16, size = 5, show.legend = FALSE, alpha = .4) +
  theme_minimal() +
  scale_color_gradient(low = "#0091ff", high = "#f0650e") +
  xlab("Annual Mean Temperature (.1 degrees C)") +
  ylab("Stomatal Density (#/mm^2)")


### stomatal conductance ###

# use a dataframe with only numeric values
cond_data.cor <- condtot[, c(1:20,22)]
# compute the correlation matrix
cond_corr = cor(cond_data.cor)
cond_corr
mydata.rcorr = rcorr(as.matrix(cond_corr))
mydata.coeff = mydata.rcorr$r
mydata.p = mydata.rcorr$P
corrplot(mydata.coeff)
palette = colorRampPalette(c("green", "white", "red")) (20)
heatmap(x = mydata.coeff, col = palette, symm = TRUE)

library(mgcv)
condgam = gam(conduct ~ predictors.1 + predictors.2 + predictors.3 + predictors.4 +
               predictors.12 + predictors.14, data = condtot)
summary(condgam)

# predict raster
predictors.subset = subset(predictors, c(1,2,3,4,12,14))

# run predict function
condpred = raster::predict(predictors.subset, condgam)
raster::plot(condpred)

# get prediction rasters
library(raster)
future = raster::getData("CMIP5", var='bio', res=2.5, rcp=85, model='AC', year=70)
future.subset = subset(future, c(1,2,3,4,12,14))
names(future.subset) = c("predictors.1", "predictors.2", "predictors.3", "predictors.4", "predictors.12", "predictors.14")
condfut = raster::predict(future.subset, condgam)
raster::plot(condfut)
# future minus present
conddiff = condfut - condpred
raster::plot(conddiff)
cellStats(conddiff, stat='mean')

# plot
condtot$pc <- predict(prcomp(~predictors.14 + conduct, condtot))[,1]
ggplot(condtot, aes(predictors.14, conduct, color = pc)) +
  geom_point(shape = 16, size = 5, show.legend = FALSE, alpha = .4) +
  theme_minimal() +
  scale_color_gradient(low = "#0091ff", high = "#f0650e") +
  xlab("Precipitation of the Driest Month (mm)") +
  ylab("Stomatal conductance")

condtot$pc <- predict(prcomp(~predictors.12 + conduct, condtot))[,1]
ggplot(condtot, aes(predictors.12, conduct, color = pc)) +
  geom_point(shape = 16, size = 5, show.legend = FALSE, alpha = .4) +
  theme_minimal() +
  scale_color_gradient(low = "#0091ff", high = "#f0650e") +
  xlab("Annual Precipitation (mm)") +
  ylab("Stomatal conductance")

condtot$pc <- predict(prcomp(~predictors.1 + conduct, condtot))[,1]
ggplot(condtot, aes(predictors.1, conduct, color = pc)) +
  geom_point(shape = 16, size = 5, show.legend = FALSE, alpha = .4) +
  theme_minimal() +
  scale_color_gradient(low = "#0091ff", high = "#f0650e") +
  xlab("Annual Mean Temperature (.1 degrees C)") +
  ylab("Stomatal conductance")

### stomatal distribution ###

# binomial linear regression and diagnostics model on the whole dataset
distglm <- glm(distrib ~ predictors.1 + predictors.2 + predictors.4 +
               predictors.5 + predictors.8  + predictors.10 + predictors.11 +
               predictors.13 + predictors.16 + predictors.19 + predictors.20, 
               family = binomial, data = disttot)
summary(distglm)
# redo model on training and testing data
set.seed(1)
smp_sz = floor(0.5*nrow(disttot))
smp_sz
train = sample(seq_len(nrow(disttot)), size=smp_sz)
training = disttot[train,]
testing = disttot[-train,]
# run model on training data
distglm <- glm(distrib ~ predictors.2 + predictors.4 +
                 predictors.5 + predictors.8  + predictors.10 + predictors.11 +
                 predictors.13 + predictors.16 + predictors.19 + predictors.20, 
               family = binomial, data = training)
summary(distglm)

# testing  the logistic regression error rate on the test data
dist.test <- testing$distrib
glm.probs = predict(distglm, testing, type = "response")
glm.pred<-rep("no", 1331)
glm.pred[glm.probs > 0.15] = "yes"
table(glm.pred,dist.test)
# Error rate (ERR) is the total number of incorrect classifications/the total
1-(1024+50)/(1024+67+190+50)
# The logistic regression has an 19% error rate
# plot some significant variables
attach(disttot)
dev.off()
par(mfrow=c(1,1))
plot(distrib, predictors.20, xlab = "Stomata On Both Surfaces", ylab = "Elevation (m)")
plot(distrib, predictors.4, xlab = "Stomata On Both Surfaces",
     ylab = "Temperature Seasonality (daily range/annual range)")
plot(distrib, predictors.10, xlab = "Stomata On Both Surfaces",
     ylab = "Mean Temperature Of Warmest Quarter")
plot(distrib, predictors.5, xlab = "Stomata On Both Surfaces",
     ylab = "Maximum Temperature")


##### Taxonomic Data #####
# see this website for more
#https://rdrr.io/cran/taxize/man/class2tree.html
# other very helpful source!
library(Taxonstand)
library(taxize)
# example
library(taxize)

## Not run: 
spnames <- c('Quercus robur', 'Iris oratoria', 'Arachis paraguariensis',
             'Helianthus annuus','Madia elegans','Lupinus albicaulis',
             'Pinus lambertiana')
out <- classification(spnames, db='itis')
tr <- class2tree(out)
plot(tr)

spnames <- c('Klattia flava', 'Trollius sibiricus',
             'Arachis paraguariensis',
             'Tanacetum boreale', 'Gentiana yakushimensis','Sesamum schinzianum',
             'Pilea verrucosa','Tibouchina striphnocalyx','Lycium dasystemum',
             'Berkheya echinacea','Androcymbium villosum',
             'Helianthus annuus','Madia elegans','Lupinus albicaulis',
             'Pinus lambertiana')
out <- classification(spnames, db='ncbi')
tr <- class2tree(out)
plot(tr)

# trying with conductivitiy data
# stringr package to extract first two words (no authorities)
library(stringr)

condtot$species1 = stringr::word(condtot$species, 1, 2, sep = " ")
cond_sp = unique(condtot$species1)
out = classification(cond_sp, db = 'ncbi')
tr <- class2tree(out)
par(mfrow=c(1,1))
plot(tr)

## trying another tutorial's suggestion
library(myTAI)
densp = dentot
densp$species1 = stringr::word(densp$species, 1, 2, sep = " ")
str(unique(densp$species1))
# For many species
species <- c(unique(densp$species1))

classnames <- sapply(species, tax_name, get = "class", USE.NAMES = F)



#https://rdrr.io/cran/taxize/man/class2tree.html
# other very helpful source!
library(ape)
library(geiger)
library(phytools)

# make an object of class phylo
tree = tr$phylo
str(tree)
plot(tree, edge.width = 0.5, show.tip.label = FALSE, type = "fan")
add.scale.bar(cex = 0.7, font = 2, col = "red")
# check if ultametric and binary
is.ultrametric(tree)
# true

# look at conductivity dataframe for concordant data
# make new for analysis
cond_tree = condtot

rownames(cond_tree) <- make.names(cond_tree$species1, unique = TRUE) %>% gsub(
                  pattern = "(\\.)+",
                  replacement = " ")
head(cond_tree)
cond_tree = cond_tree[, c(1:22, 24:27)]
head(cond_tree)

# We will drop and create a common tree and data (i.e., concordant) using ‘treedata’
cond_phylo_data <- treedata(tree, cond_tree)

# save as a pruned tree
cond_pruned <- cond_phylo_data$phy

# save the tree
cond_data_pruned <- cond_phylo_data$data
write.table(cond_data_pruned, file = "cond_data_pruned.txt", sep ="\t")


# plot the reduced tree
plot(cond_pruned, edge.width = 0.5, show.tip.label = FALSE, type = "fan")
add.scale.bar(cex = 0.7, font = 2, col = "red")

# read back reduced data
cond_data_pruned1 <- read.table("cond_data_pruned.txt", header = TRUE, sep = "\t")
cond_data_pruned1

# make data into factors via mean cutoffs
# most important data: annual temperature; annual rainfall predictors.1 + predictors.12

# make a vector of all cool
cond_data_pruned1$temp = rep("Cool", 1000)
# find mean of temperature values
mean(cond_data_pruned1$predictors.1)
# change vactor value to hot if greater than mean
cond_data_pruned1$temp[cond_data_pruned1$predictors.1 > mean(cond_data_pruned1$predictors.1)] = "Warm"

# change preciptation data in same way
# make a vector of all cool
cond_data_pruned1$precip = rep("Dry", 1000)
# find mean of temperature values
mean(cond_data_pruned1$predictors.12)
# change vactor value to hot if greater than mean
cond_data_pruned1$precip[cond_data_pruned1$predictors.12 > mean(cond_data_pruned1$predictors.12)] = "Wet"

#  change conductance in same way
# make a vector of all cool
cond_data_pruned1$conduct_fact = rep("Low", 1000)
# find mean of temperature values
mean(cond_data_pruned1$conduct)
# change vactor value to hot if greater than mean
cond_data_pruned1$conduct_fact[cond_data_pruned1$conduct > mean(cond_data_pruned1$conduct)] = "High"

## sort the data frame to match the phylogeny
match_cond = match(cond_pruned$tip.label, rownames(cond_data_pruned1))
match_cond_data = as.data.frame(cond_data_pruned1[match_cond,])

# prepare the data for plotting
#Prepare data for plotting. Climate and Food. We are going to use colored labels for discrete traits

#Climate
temp_cond_label <- character(length(match_cond_data$temp))
names(temp_cond_label) <- rownames(match_cond_data)
#get the unique states for Climate
unique(match_cond_data$temp)
# warm and cool
#label states with colors for plot
temp_cond_label[match_cond_data$temp == "Warm"] = "red"
# do blue for cool
temp_cond_label[match_cond_data$temp == "Cool"] = "blue"
temp_cond_label

# precipitation
precip_cond_label <- character(length(match_cond_data$precip))
names(precip_cond_label) <- rownames(match_cond_data)
#get the unique states for Climate
unique(match_cond_data$precip)
#label states with colors for plot
precip_cond_label[match_cond_data$precip == "Wet"] = "green"
# do blue for cool
precip_cond_label[match_cond_data$precip == "Dry"] = "brown"
precip_cond_label

# conductance
conduct_cond_label <- character(length(match_cond_data$conduct_fact))
names(conduct_cond_label) <- rownames(match_cond_data)
#get the unique states for Climate
unique(match_cond_data$conduct_fact)
#label states with colors for plot
conduct_cond_label[match_cond_data$conduct_fact == "High"] = "blue"
# do blue for cool
conduct_cond_label[match_cond_data$conduct_fact == "Low"] = "light blue"
conduct_cond_label


# Plot phylogram and the concordant data
windows.options(width=400, height=400)

plot(cond_pruned, cex=1, show.tip.label = FALSE, y.lim=c(0,250), x.lim=c(0,55)) #Plot a tree that leaves some room
# between the tree tips and taxon labels so that we can plot habitat use in this space
points(rep(55, nrow(match_cond_data)), 1:nrow(match_cond_data), pch=22,
       bg=temp_cond_label[cond_pruned$tip.label], lwd = 0.05, cex=3)
points(rep(50, nrow(match_cond_data)), 1:nrow(match_cond_data), pch=22,
       bg=precip_cond_label[cond_pruned$tip.label], lwd = 0.05, cex=3)
points(rep(52.5, nrow(match_cond_data)), 1:nrow(match_cond_data), pch=22,
       bg=conduct_cond_label[cond_pruned$tip.label], lwd = 0.05, cex=3)


# Plot phylogram and the concordant data
windows.options(width=400, height=400)

plot(cond_pruned, cex=1, show.tip.label = FALSE, y.lim=c(250,500), x.lim=c(0,60)) #Plot a tree that leaves some room
# between the tree tips and taxon labels so that we can plot habitat use in this space
points(rep(55, nrow(match_cond_data)), 1:nrow(match_cond_data), pch=22,
       bg=temp_cond_label[cond_pruned$tip.label], lwd = 0.05, cex=3)
points(rep(50, nrow(match_cond_data)), 1:nrow(match_cond_data), pch=22,
       bg=precip_cond_label[cond_pruned$tip.label], lwd = 0.05, cex=3)
points(rep(52.5, nrow(match_cond_data)), 1:nrow(match_cond_data), pch=22,
       bg=conduct_cond_label[cond_pruned$tip.label], lwd = 0.05, cex=3)


# Plot phylogram and the concordant data
windows.options(width=400, height=400)

plot(cond_pruned, cex=1, show.tip.label = FALSE, y.lim=c(500, 750), x.lim=c(0,60)) #Plot a tree that leaves some room
# between the tree tips and taxon labels so that we can plot habitat use in this space
points(rep(55, nrow(match_cond_data)), 1:nrow(match_cond_data), pch=22,
       bg=temp_cond_label[cond_pruned$tip.label], lwd = 0.05, cex=3)
points(rep(50, nrow(match_cond_data)), 1:nrow(match_cond_data), pch=22,
       bg=precip_cond_label[cond_pruned$tip.label], lwd = 0.05, cex=3)
points(rep(52.5, nrow(match_cond_data)), 1:nrow(match_cond_data), pch=22,
       bg=conduct_cond_label[cond_pruned$tip.label], lwd = 0.05, cex=3)


# Plot phylogram and the concordant data
windows.options(width=400, height=400)

plot(cond_pruned, cex=1, show.tip.label = FALSE, y.lim=c(750, 1000), x.lim=c(0,60)) #Plot a tree that leaves some room
# between the tree tips and taxon labels so that we can plot habitat use in this space
points(rep(55, nrow(match_cond_data)), 1:nrow(match_cond_data), pch=22,
       bg=temp_cond_label[cond_pruned$tip.label], lwd = 0.05, cex=3)
points(rep(50, nrow(match_cond_data)), 1:nrow(match_cond_data), pch=22,
       bg=precip_cond_label[cond_pruned$tip.label], lwd = 0.05, cex=3)
points(rep(52.5, nrow(match_cond_data)), 1:nrow(match_cond_data), pch=22,
       bg=conduct_cond_label[cond_pruned$tip.label], lwd = 0.05, cex=3)


#### reduce the number to 100 randomly selected to show better
set.seed(1)
# sample 10%
cond_10 <- cond_data_pruned1 %>% sample_frac(0.05)

# We will drop and create a common tree and data (i.e., concordant) using ‘treedata’
cond_phylo_data <- treedata(tree, cond_10)

## sort the data frame to match the phylogeny
match_cond = match(cond_pruned$tip.label, rownames(cond_10))
match_cond_data = as.data.frame(cond_data_pruned1[match_cond,])

# save as a pruned tree
cond_pruned <- cond_phylo_data$phy

# save the data
cond_data_pruned <- cond_phylo_data$data

# ## sort the data frame to match the phylogeny
match_cond = match(cond_pruned$tip.label, rownames(cond_10))
match_cond_data = as.data.frame(cond_data_pruned1[match_cond,])

# plot the tree
plot(cond_pruned, edge.width = 1, show.tip.label = T, cex = 0.9, y.lim=c(0,55), x.lim=c(0,82))
# add the data
points(rep(78, nrow(match_cond_data)), 1:nrow(match_cond_data), pch= 22,
       bg=temp_cond_label[cond_pruned$tip.label], lwd = 0.05, cex=1.9)
points(rep(75, nrow(match_cond_data)), 1:nrow(match_cond_data), pch=22,
       bg=precip_cond_label[cond_pruned$tip.label], lwd = 0.05, cex=1.9)
points(rep(76.5, nrow(match_cond_data)), 1:nrow(match_cond_data), pch=22,
       bg=conduct_cond_label[cond_pruned$tip.label], lwd = 0.05, cex=1.9)
text(74, nrow(match_cond_data)+6, "Precipitation", pos=1, cex=1)
text(76, nrow(match_cond_data)+5, "Conductance", pos=1, cex=1)
text(78, nrow(match_cond_data)+4, "Temperature", pos=1, cex=1)
text(63.7, nrow(match_cond_data)+7.5, "High Low", pos=1, cex=1)
points(65, nrow(match_cond_data)+4.5, pch= 15, col = "brown", lwd = 0.05, cex=1.9)
points(63, nrow(match_cond_data)+4.5, pch= 15, col = "green", lwd = 0.05, cex=1.9)
points(65, nrow(match_cond_data)+3.5, pch= 15, col = "light blue", lwd = 0.05, cex=1.9)
points(63, nrow(match_cond_data)+3.5, pch= 15, col = "blue", lwd = 0.05, cex=1.9)
points(65, nrow(match_cond_data)+2.5, pch= 15, col = "blue", lwd = 0.05, cex=1.9)
points(63, nrow(match_cond_data)+2.5, pch= 15, col = "red", lwd = 0.05, cex=1.9)

# reset the window
windows.options(reset = T)
help(windows.options)



