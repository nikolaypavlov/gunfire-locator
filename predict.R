setwd("~/repos/gunfire-locator/")
library(tuneR)
library(seewave)
library(zoo)
library(caret)
library(randomForest)
library(nnet)
library(doMC)
library(zoo)
registerDoMC()

load("data/prediction_model.rda")

wl <- 1024
ovlp <- 512

rec1 <- readWave("~/Documents/GunTest_26_06_14/AK_545_40/AK_545_40_Записано/Трек 1_001 (7).wav")
rec2 <- readWave("~/Documents/GunTest_26_06_14/AK_545_40/AK_545_40_Записано/Трек 2_001 (7).wav")
rec3 <- readWave("~/Documents/GunTest_26_06_14/AK_545_40/AK_545_40_Записано/Трек 3_001 (7).wav")
rec4 <- readWave("~/Documents/GunTest_26_06_14/AK_545_40/AK_545_40_Записано/Трек 4_001 (7).wav")

f <- rec1@samp.rate
rec1 <- normalize(rec1)
rec2 <- normalize(rec2)
rec3 <- normalize(rec3)
rec4 <- normalize(rec4)

data <- lapply(list(rec1, rec2, rec3, rec4), function(x) rollapply(x@left, width=wl, extract_features, f, by=ovlp))
data <- do.call(rbind.data.frame, data)

pred <- predict(model, data)
