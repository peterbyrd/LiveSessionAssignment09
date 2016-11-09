## Author: Peter Byrd
## Load the data

## Set the working directory and load packages
setwd("/Users/pbyrd/Git/LiveSessionAssignment09")
library(plyr)
library(dplyr)
library(ggplot2)

## Read CSV input file
chf <- read.csv("Data/CHF.csv", header=TRUE)