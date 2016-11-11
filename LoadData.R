## Author: Peter Byrd
## Load the data

## Set the working directory and load packages
setwd("/Users/pbyrd/Git/LiveSessionAssignment09")

## Need to install the following packages: ggplot2 and plotly
library('ggplot2')
library('plotly')
library('reshape2')

## Read CSV input file
chf <- read.csv("Data/CHF.csv", header=TRUE)