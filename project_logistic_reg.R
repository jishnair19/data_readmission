

myd <- read.csv(file.choose())

head(myd)

myd <- myd[5:20]

myd$admission_type_id <- ifelse(is.na(myd$admission_type_id), ave(myd$admission_type_id, FUN=function(x) mean(x, na.rm=T)), myd$admission_type_id)

myd$discharge_disposition_id <- ifelse(is.na(myd$discharge_disposition_id), ave(myd$discharge_disposition_id, FUN=function(x) mean(x, na.rm=T)), myd$discharge_disposition_id)

str(myd)

head(myd)

myd$readmitted <- factor(myd$readmitted, levels=c('NO', '<30'), labels=c(0, 1))


myd$diabetesMed <- as.numeric(factor(myd$diabetesMed, levels=c('No', 'Yes'), labels=c(1, 2)))

myd$change <- as.numeric(factor(myd$change, levels=c('No', 'Ch'), labels=c(1, 2)))

myd$insulin <- as.numeric(factor(myd$insulin, levels=c('Down', 'No', 'Steady', 'Up'), labels=c(1, 2, 3, 4)))

myd$gender <- as.numeric(factor(myd$gender, levels=c('Female', 'Male'), labels=c(1, 2)))

myd$age <- as.numeric(factor(myd$age, levels=c('[0-10)', '[10-20)', '[20-30)', '[30-40)', '[40-50)', '[50-60)', '[60-70)', '[70-80)', '[80-90)', '[90-100)'), labels=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)))

myd$weight <- as.numeric(factor(myd$weight, levels=c('[0-25)', '[100-125)', '[150-175)', '[175-200)', '[25-50)', '[50-75)', '[75-100)'), labels=c(1, 2, 3, 4, 5, 6, 7)))

head(myd)

library(caTools)

set.seed(1234)

split <- sample.split(myd$readmitted, SplitRatio = 0.80)

training_set <- subset(myd, split==T)

test_set <- subset(myd, split==F)

head(test_set)

head(training_set)

training_set[-16] <- scale(training_set[-16])

test_set[-16] <- scale(test_set[-16])

classifier <- glm(readmitted~., family=binomial, data=training_set)

prob_pred <- predict(classifier, type='response', newdata=test_set[-16])

y_pred <- ifelse(prob_pred>0.5, 1, 0)

cm <- table(test_set[, 16], y_pred>0.5)

library(caret)

folds <- createFolds(training_set$readmitted, k=10)
cv <- lapply(folds, FUN=function(x){
  training_fold = training_set[-x, ]
  test_fold = test_set[x, ]
  classifier <- glm(readmitted~., family=binomial, data=training_fold)
  prob_pred <- predict(classifier, type='response', newdata=test_fold[-16])
  y_pred <- ifelse(prob_pred>0.5, 1, 0)
  cm <- table(test_fold[, 16], y_pred>0.5)
  accuracy = (cm[1, 1] + cm[2, 2]) / (cm[1, 1] + cm[2, 2] + cm[2, 1] + cm[1, 2])
  return(accuracy)
})

accuracy = mean(as.numeric(cv))












