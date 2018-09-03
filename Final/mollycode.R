# Package for fitting SVM

library(e1071)



# New model libraries
library(class) # new library for KNN modeling
## Libraries for Plotting our Results
library(ggplot2)
library(gridExtra)
## Library for confusion matrix
library(dplyr)
library(caret)

#plot ROC curves
library(ROCR)
library(rpart)
library(rpart.plot)

frac = 0.8

rdf = read.csv (
  file = "2018-08-24_AGGREGATE_AAPL.csv",
  header = TRUE,
  stringsAsFactors = FALSE,
  sep = ","
)

#rdf = read.csv (file = "2018-08-25_AGGREGATE_T.csv", header=TRUE, stringsAsFactors = FALSE, sep=",")

#rdf = read.csv (file = "2018-08-28_AGGREGATE_TSLA.csv", header=TRUE, stringsAsFactors = FALSE, sep=",")

dim(rdf)

sum(is.na(rdf))



#column names to delete

xcols = c(
  "Date",
  "open",
  "high",
  "low",
  "close",
  "adj.",
  "volume",
  "macd",
  "fastD.1",
  "fastK.1",
  "wikicnts",
  "d.hits",
  "wk.hits",
  "adj.d",
  "sd.month.d1",
  "sd.qrtr.d1",
  "sd.annual.d1",
  "Delt.1",
  "mrtg.d.hits",
  "trueHigh"
)

#xcols = list(NULL)

cols = colnames(rdf)

rem_cols = match(xcols, cols)

rem_cols



raw.predictors = rdf[, c(1:55)]



raw.predictors[, rem_cols] = list(NULL)

colnames(raw.predictors)



#split responses from predictors

raw.responses = rdf[, c(56:59)]

#raw.predictors = rdf[,c(1:55)]



#split test and training data

set.seed(7770777)

idx = seq(1, nrow(raw.predictors))

train_idx = sample(idx, round(frac * nrow(raw.predictors)), replace = FALSE)

length(train_idx)



train_predictors = raw.predictors[train_idx, ]

train_responses = raw.responses[train_idx, ]

train_all = cbind(train_predictors, train_responses)



test_predictors = raw.predictors[-train_idx, ]

test_responses = raw.responses[-train_idx, ]

test_all = cbind(test_predictors, test_responses)



head(test_predictors)



#KNN Model development

##model for 5d ahead predicting 2 standard deviations

train.response <- as.factor(train_responses$Resp.5d.2sd)

test.response <- as.factor(test_responses$Resp.5d.2sd)



#find best k using cross vaildation

knn.cross <-
  tune.knn(
    x = train_predictors,
    y = train.response,
    k = 1:40,
    tunecontrol = tune.control(sampling = "cross"),
    cross = 10
  )

summary(knn.cross)

plot(knn.cross) #for 5d2sd, seems like k=16 is a good choice



#find best k using bootstrapping

knn.boot <-
  tune.knn(
    x = train_predictors,
    y = train.response,
    k = 1:40,
    tunecontrol = tune.control(sampling = "boot"),
    cross = 10
  )

summary(knn.boot)

plot(knn.boot) #k=22 seems like best, but has worse results than cross validation results



#since k=16 is best out of both methods, selected k=16

knn.best <-
  knn(train_predictors, test_predictors, train.response, k = 16)

sum(test.response == knn.best) / length(test.response)  # For knn = 16



# Confusion Matrix

confusionMatrix(knn.best, test.response)



# ROC for KNN

knn.best.prob <-
  knn(train_predictors,
      test_predictors,
      train.response,
      k = 16,
      prob = TRUE)

KNN.prob <- rep(0, length(knn.best.prob)) ## Initializing

### want to assign the probability  that the the market will vary by greater than 2 standard deviations (1, not 0)

### (attr(knn_obj, 'prob')  is the proportion of the votes for the winning class

### this means it will always be greater that 0.5 and may either be for greater or less than 2sd's or perished

for (i in 1:length(KNN.prob))
  
  KNN.prob[i] <-
  ifelse(knn.best.prob[i] != 0,
         attr(knn.best.prob, 'prob')[i],
         1 - attr(knn.best.prob, 'prob')[i])

predProbKNN <- prediction(KNN.prob, test.response)

KNN.ROC <- performance(predProbKNN, measure = "tpr", x.measure = "fpr")



# plot

#plot(attributes(KNN.ROC)$x.values[[1]], attributes(KNN.ROC)$y.values[[1]],

#     col="blue", lwd=2)

plot(KNN.ROC, lwd = 2, main = "ROC Comparison for Models on 5d.2sd Dataset")



#calculate AUC

KNN.AUC <- performance(prediction(KNN.prob, test.response),
                       
                       measure = "auc")@y.values

KNN.AUC





##model for 10d ahead predicting 2 standard deviations

train.response <- as.factor(train_responses$Resp.10d.2sd)

test.response <- as.factor(test_responses$Resp.10d.2sd)



#find best k using cross vaildation

knn.cross <-
  tune.knn(
    x = train_predictors,
    y = train.response,
    k = 1:40,
    tunecontrol = tune.control(sampling = "cross"),
    cross = 10
  )

summary(knn.cross)

plot(knn.cross) #for 5d2sd, seems like k=9 is a good choice



#find best k using bootstrapping

knn.boot <-
  tune.knn(
    x = train_predictors,
    y = train.response,
    k = 1:40,
    tunecontrol = tune.control(sampling = "boot"),
    cross = 10
  )

summary(knn.boot)

plot(knn.boot) #k=20 seems like best, and performs well with crossstrapping



#since k=21 is good using both methods, selected k=20

knn.best <-
  knn(train_predictors, test_predictors, train.response, k = 20)

sum(test.response == knn.best) / length(test.response)  # For knn = 20



# Confusion Matrix

confusionMatrix(knn.best, test.response)



# ROC for KNN

knn.best.prob <-
  knn(train_predictors,
      test_predictors,
      train.response,
      k = 20,
      prob = TRUE)

KNN.prob <- rep(0, length(knn.best.prob)) ## Initializing

### want to assign the probability  that the the market will vary by greater than 2 standard deviations (1, not 0)

### (attr(knn_obj, 'prob')  is the proportion of the votes for the winning class

### this means it will always be greater that 0.5 and may either be for greater or less than 2sd's or perished

for (i in 1:length(KNN.prob))
  
  KNN.prob[i] <-
  ifelse(knn.best.prob[i] != 0,
         attr(knn.best.prob, 'prob')[i],
         1 - attr(knn.best.prob, 'prob')[i])

predProbKNN <- prediction(KNN.prob, test.response)

KNN.ROC <- performance(predProbKNN, measure = "tpr", x.measure = "fpr")



# plot

#plot(attributes(KNN.ROC)$x.values[[1]], attributes(KNN.ROC)$y.values[[1]],

#     col="blue", lwd=2)

plot(KNN.ROC, lwd = 2, main = "ROC Comparison for Models on 10d.2sd Dataset")



#calculate AUC

KNN.AUC <- performance(prediction(KNN.prob, test.response),
                       
                       measure = "auc")@y.values

KNN.AUC