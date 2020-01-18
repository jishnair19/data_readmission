
# Logistic Regression

# Importing the dataset
dataset = read.csv('diabetic_data.csv')
dataset = dataset[5:20]

#Viewing data from the dataset
head(dataset, 5)
tail(dataset, 5)

#Check for missing data

sum(is.na(dataset))

#Imputation, replacing missing values with the comlumn's mean
dataset$admission_type_id <- ifelse(is.na(dataset$admission_type_id), ave(dataset$admission_type_id, FUN=function(x) mean(x, na.rm=T)), dataset$admission_type_id)
dataset$discharge_disposition_id <- ifelse(is.na(dataset$discharge_disposition_id), ave(dataset$discharge_disposition_id, FUN=function(x) mean(x, na.rm=T)), dataset$discharge_disposition_id)

#To see the structure of the dataset
str(dataset)
summary(dataset)

#To display the summary statistics
library(skimr)
skim(dataset)

# Encoding the target feature as factor
dataset$readmitted <- factor(mydataset$readmitted, levels=c('NO', '<30'), labels=c(0, 1))

#Encoding the required independent variables into int
dataset$diabetesMed <- as.numeric(factor(dataset$diabetesMed, levels=c('No', 'Yes'), labels=c(1, 2)))
dataset$change <- as.numeric(factor(dataset$change, levels=c('No', 'Ch'), labels=c(1, 2)))
dataset$insulin <- as.numeric(factor(dataset$insulin, levels=c('Down', 'No', 'Steady', 'Up'), labels=c(1, 2, 3, 4)))
dataset$gender <- as.numeric(factor(dataset$gender, levels=c('Female', 'Male'), labels=c(1, 2)))
dataset$age <- as.numeric(factor(dataset$age, levels=c('[0-10)', '[10-20)', '[20-30)', '[30-40)', '[40-50)', '[50-60)', '[60-70)', '[70-80)', '[80-90)', '[90-100)'), labels=c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)))
dataset$weight <- as.numeric(factor(dataset$weight, levels=c('[0-25)', '[100-125)', '[150-175)', '[175-200)', '[25-50)', '[50-75)', '[75-100)'), labels=c(1, 2, 3, 4, 5, 6, 7)))


# Splitting the dataset into the Training set and Test set
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Purchased, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling by exculding the dependent variable
training_set[-16] = scale(training_set[-16])
test_set[-16] = scale(test_set[-16])

# Fitting Logistic Regression to the Training set
classifier = glm(formula = Purchased ~ .,
                 family = binomial,
                 data = training_set)

# Predicting the Test set results
prob_pred = predict(classifier, type = 'response', newdata = test_set[-16])
y_pred = ifelse(prob_pred > 0.5, 1, 0)

# Making the Confusion Matrix
cm = table(test_set[, 16], y_pred > 0.5)

## Applying k-Fold Cross Validation
# install.packages('caret')
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












