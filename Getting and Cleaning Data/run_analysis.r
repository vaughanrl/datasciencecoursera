# Getting and Cleaning Data, Johns Hopkins University, Coursera
# Course Project
# Author. Ron Vaughan

# 1. load libraries
# 2. collect raw dataset and unzip
# 3. read/load features and activities
# 4. read/load test datasets
# 5. merge datasets
# 6. extract measurements for mean and standard deviation for each meaurement
# 7. rename activity names
# 8. relabel data sets with descriptive variable names
# 9. using relabeled datasets, create a tidy data with average of each variable for 
#       each activity and each subject

# load libraries
library(data.table)
library(reshape2)

# collect raw dataset and unzip
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
datafile <- "data.zip"
download.file(url, datafile)
unzip(datafile)

# read unzipped datasets
activityLabels <- fread("./UCI HAR Dataset/activity_labels.txt", col.names = c("classLabels", "activityName"))
features <- fread("./UCI HAR Dataset/features.txt", col.names = c("index", "featureNames"))
featuresNeeded <- grep("(mean|std)\\(\\)", features[, featureNames])
measurements <- features[featuresNeeded, featureNames]
measurements <- gsub('[()]', '', measurements)

# load train and test datasets
trainData <- fread("./UCI HAR Dataset/train/X_train.txt")[, featuresNeeded, with=FALSE]
data.table::setnames(trainData, colnames(trainData), measurements)
trainLabels <- fread("./UCI HAR Dataset/train/y_train.txt", col.names = c("Activity"))
trainSubject <- fread("./UCI HAR Dataset/train/subject_train.txt", col.names = c("SubjectNum"))
trainData <- cbind(trainSubject, trainLabels, trainData)

testData <- fread("./UCI HAR Dataset/test/X_test.txt")[, featuresNeeded, with = FALSE]
data.table::setnames(testData, colnames(testData), measurements)
testLabels <- fread("./UCI HAR Dataset/test/y_test.txt", col.names = c("Activity"))
testSubject <- fread("./UCI HAR Dataset/test/subject_test.txt", col.names = c("SubjectNum"))
testData <- cbind(testSubject, testLabels, testData)

# merge datasets
combined <- rbind(trainData, testData, fill=TRUE)

# rename activity names
combined[["Activity"]] <- factor(combined[, Activity],
                          levels = activityLabels[["classLabels"]],
                          labels = activityLabels[["activityName"]])

# relabel datasets
combined[["SubjectNum"]] <- as.factor(combined[, SubjectNum])
combined <- reshape2::melt(data = combined, id = c("SubjectNum", "Activity"))

# extract measuresments for mean and standard deviation
combined <- reshape2::dcast(data = combined, SubjectNum + Activity ~ variable, fun.aggregate = mean)

# create tidy data set 
data.table::fwrite(x = combined, file = "tidyData.txt", quote = FALSE)
