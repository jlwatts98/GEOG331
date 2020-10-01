#use built in iris dataset
#take a look at it 
head(iris)
#load in some tidyverse packages
library(dplyr)
library(ggplot2)

#####################################
##### Part 1: for loops         #####
#####################################

#Using only data for iris versicolor
#write a for loop
#that produces a regression table
#for each of the following relationships
#1. iris  sepal length x width
#2. iris  petal length x width
#3. iris sepal length x petal length

# create a list in R
iris_versicolor <- dplyr::filter(iris, Species == "versicolor")
iris_list <- list(iris_versicolor$Sepal.Length ~ iris_versicolor$Sepal.Width, 
                  iris_versicolor$Petal.Length ~ iris_versicolor$Petal.Width,
                  iris_versicolor$Sepal.Length ~ iris_versicolor$Petal.Length)
#check the list
iris_list [1]

# make an empty vector

regressions <- 0

# make a for loop

for(i in 1:3) {regressions <-
  summary(lm(iris_list[[i]]))
  print(regressions) }



#####################################
##### Part 2: data in dplyr     #####
#####################################

#use dplyr to join data of maximum height
#to a new iris data frame
height <- data.frame(Species = c("virginica","setosa","versicolor"),
					Height.cm = c(60,100,11.8))
iris_height <- full_join(iris,height,by='Species')
View(iris_height)


#####################################
##### Part 3: plots in ggplot2  #####
#####################################
#look at base R scatter plot
plot(iris$Sepal.Length,iris$Sepal.Width)

#3a. now make the same plot in ggplot
iris_plot <- ggplot(iris, aes(Sepal.Length, Sepal.Width))
iris_plot + geom_point()

#3b. make a scatter plot with ggplot and get rid of busy grid lines
library(ggpubr)
iris_plot + geom_point() + theme_pubr()

#3c.make a scatter plot with ggplot and get rid of grid lines,
#show species by color, and increase the point size
iris_plot + geom_point(aes(color = Species, size = 1.5)) + theme_pubr()

#####################################
##### Question: how did         #####
##### arguments differ between  #####
##### plot and ggplot?          #####
#####################################

# plot used the iris$ notation whereas ggplot made the user define the data beforehand.
# in ggplot, the major information such as the data used are put in aes().
# ggplot has a lot more add ons such as theme_pubr() which all for more customization.
# ggplot uses "+" between elements of the plot. plot does not have this option.