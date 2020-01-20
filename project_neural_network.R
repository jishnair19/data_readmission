
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

# Decision Tree Classification

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

# Fitting ANN to the Training set
# install.packages('h2o')
library(h2o)
h2o.init(nthreads = -1)
model = h2o.deeplearning(y = 'Exited',
                         training_frame = as.h2o(training_set),
                         activation = 'Rectifier',
                         hidden = c(8,8),
                         epochs = 100,
                         train_samples_per_iteration = -2)

# Predicting the Test set results
y_pred = h2o.predict(model, newdata = as.h2o(test_set[-16]))
y_pred = (y_pred > 0.5)
y_pred = as.vector(y_pred)

# Making the Confusion Matrix
cm = table(test_set[, 16], y_pred)

# To shut down h2o
h2o.shutdown()