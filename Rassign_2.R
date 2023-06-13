
#----------------------------------QUES-a---------------------------------------

data("iris")

#part1--------
boxplot(iris$Sepal.Length ~ iris$Species, col = c("red", "green", "blue"),
        xlab = "Species", ylab = "Sepal Length", main = "Boxplot of Sepal Length by Species")

boxplot(iris$Petal.Length ~ iris$Species, col = c("red", "green", "blue"),
        xlab = "Species", ylab = "Petal Length", main = "Boxplot of Petal Length by Species")

boxplot(iris$Sepal.Width ~ iris$Species, col = c("red", "green", "grey"),
        xlab = "Species", ylab = "Sepal Width", main = "Boxplot of Sepal Width by Species")

boxplot(iris$Petal.Width ~ iris$Species, col = c("red", "green", "grey"),
        xlab = "Species", ylab = "Sepal Width", main = "Boxplot of Sepal Width by Species")

#part2-------
plot(iris$Sepal.Length,iris$Petal.Length,main="Scatterplot of Sepal Length vs. Petal Length",
     xlab="Length of Sepals",ylab="Length of Petals",col=iris$Species,pch=19)
legend("topleft", legend =levels(iris$Species), col = 1:3,pch=19, 
       title = "Species")

#----------------------------------QUES-b---------------------------------------

library(imager)

#defined function flip()
flip <- function(image){
  
#converted image to matrix of pixels
  img.mat<-as.matrix(image) 
  dims <- dim(img_mat)

#reversed the row elements of matrix to flip vertically
  flip.mat <- img.mat[dims[1]:1,1:dims[2],]
#converted matrix to image   
  flip.img <- as.cimg(flip.mat)
  plot(flip.img)
}


#----------------------------------QUES-c---------------------------------------

library(MASS)
data<-data.frame(ships)
str(data)
plot(data$type,data$incidents, main = "Plot representing ship accident data",xlab="Ship type",ylab="No. of incidents")

#----------------------------------QUES-d---------------------------------------

library(tidyverse)
library(rvest)
library(stringr)
library(dplyr)

html <- read_html("https://stats.stackexchange.com/questions?tab=Votes")
span <- c()
span <- html %>% html_elements(".s-post-summary--stats-item-number") %>% html_text()
str(span)
mat <- matrix(span,nrow = 3,ncol=15)
mat <- t(mat)

votes<- mat[,1]
ans <- mat[,2]
views <- mat[,3]
ques<-html %>% html_elements(".s-link") %>% html_text()
ques<-ques[2:16]
df <- data.frame("Title of question"= ques,"number of views"=views,"number of answers"=ans, "number of votes"=votes)

#----------------------------------QUES-e----------------------------------------

half_tab <- function() {
  count <- 0  
  wholetab <- 100
  halftab <- 0
  while (TRUE) {
    count <- count + 1
    item_pull <- sample(c(rep("whole", wholetab), rep("half",halftab)), 1, replace = TRUE)
    if (item_pull == "half") {
      return(count)
    }
    if (item_pull == "whole") {
     wholetab <- wholetab - 1
      halftab <- halftab + 2
    }
  }
}

simulations <- 10000
total_days <- 0

for (i in 1:simulations) {
  total_days <- total_days + half_tab()
}

average_days <- total_days / simulations

cat("on average, it will take about", average_days, "days.")


