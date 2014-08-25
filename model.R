setwd("~/repos/gunfire-locator/")
library(tuneR)
library(seewave)
library(zoo)
library(caret)
library(randomForest)
library(nnet)
library(doMC)
registerDoMC()

#
# Load and assamble the dataset
#
load("data/dataset.rda")

#
# Convert some features to factors
#
dataset$status <- as.factor(dataset$status)

#
# Balance classes of the dataset. 
# Remove negative classes with low energy, but include negative samples that
# has include_into_model flag set to one 
#
#dataset <- dataset[-which(is.na(dataset$status)),]
positive_samples <- dataset[dataset$status==1, ]
negative_samples <- dataset[dataset$status==0, ]

set.seed(1)
rows <- sample(1:nrow(negative_samples), nrow(positive_samples))
dataset <- rbind(negative_samples[rows,], positive_samples)

#
# Split dataset into Train, Validation and Test
#
set.seed(214)
qualification_set_idx <- sample(1:nrow(dataset), floor(nrow(dataset)*0.4))
set.seed(212)
validation_set_idx <- sample(qualification_set_idx, floor(length(qualification_set_idx)*0.5))
test_set_idx <- qualification_set_idx[!qualification_set_idx %in% validation_set_idx]
train <- dataset[-qualification_set_idx,]
validation <- dataset[validation_set_idx,]
test <- dataset[test_set_idx,]

#
# Exclude some features
#
excluded <- !names(dataset) %in% c("spec.m0", "spec.m1", "spec.m2", "spec.m3")

#
# Caluclate base line model
#
set.seed(121)
baseline_model <- randomForest(status ~ . , data=train[,excluded], importance=TRUE)
baseline_pred <- predict(baseline_model, validation[,excluded])
print(confusionMatrix(baseline_pred, validation$status, positive="1"))

# Get importance of the features
importance <- importance(baseline_model)
importance <- importance[order(-importance[,3], -importance[,4]),]

# Use cross validation for tunning
control <- trainControl(method="cv", number=10)

set.seed(528)
# Other models RF
hyperparams_rf <- expand.grid(.mtry=c(3,4,5,7,8,9))
model <- train(status ~ . , train[,excluded], method="rf", tuneGrid=hyperparams_rf, trControl=control)

print(model)

#
# Predictions
#
pred <- predict(model, validation[,excluded])
print(confusionMatrix(pred, validation$status, positive="1"))

save(model, baseline_pred, file="data/prediction_model.rda")
