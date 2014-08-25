setwd("~/repos/gunfire-locator/")
library(tuneR)
library(seewave)
library(zoo)

source("functions.R")

load("data/gunfire.rda")
load("data/noise.rda")

f <- 44100

data.shots <- do.call(rbind.data.frame, apply(waves, 1, extract_features, f))
data.shots$status <- "1"

data.noise <- do.call(rbind.data.frame, apply(noise, 1, extract_features, f))
data.noise$status <- "0"

dataset <- rbind(data.shots, data.noise)
dataset <- dataset[complete.cases(dataset),]

save(dataset, file="data/dataset.rda")