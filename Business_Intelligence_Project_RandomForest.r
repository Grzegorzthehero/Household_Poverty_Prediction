setwd("/Users/willemzents/Desktop/RWork/CS")

library(dplyr)
library(ggplot2)
library(mlbench)
library(caret)
library(party)
library(Boruta)
library(ordinalForest)
library(randomForest)
library(party)

train_data <- read.csv("train_data.csv", header = T)
test_data <- read.csv("test_data.csv", header = T)
train_data2 <- read.csv("train_data2.csv", header = T)
test_data2 <- read.csv("test_data2.csv", header = T)

summary(train_data)

############



summary(train_data)