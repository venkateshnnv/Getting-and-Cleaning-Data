# Refs
# http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 

# Tasks
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive activity names. 
# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

# a. cleanup 
rm(list = ls())

# b. fetch and unzip the data set
baseDir <- "."

# b.1 create data sub-directory if necessary
dataDir <- paste(baseDir, "data", sep="/")
if(!file.exists(dataDir)) {
  dir.create(dataDir)
}

# b.2 download original data if necessary (skip if exists already as it takes time)
zipFilePath <- paste(dataDir, "Dataset.zip", sep="/")
if (!file.exists(zipFilePath)) {
  zipFileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file (zipFileUrl, zipFilePath, method="curl")
  dateDownloaded <- date()
  cat ("Dataset downloaded on:", dateDownloaded,"\n")
}

# b.3 unzip and creates dataSetDir if necessary
dataSetDir <-  paste (baseDir, "UCI HAR Dataset", sep="/")
if (!file.exists(dataSetDir)) {
  unzip (zipFilePath, exdir=baseDir)
}
list.files(baseDir)

# c. read the data sets

# c.1 subjects IDs
trainSubjectsPath <- paste (dataSetDir, "train", "subject_train.txt", sep="/")
testSubjectsPath <-  paste (dataSetDir, "test", "subject_test.txt", sep="/")
trainSubjects <- read.table(trainSubjectsPath, header = FALSE) 
testSubjects  <- read.table(testSubjectsPath, header = FALSE)
str(trainSubjects)
str(testSubjects)
table(trainSubjects)
table(testSubjects)

# c.1 activities codes
trainLabelsPath <- paste (dataSetDir, "train", "y_train.txt", sep="/")
testLabelsPath <-  paste (dataSetDir, "test", "y_test.txt", sep="/")
trainLabels <- read.table(trainLabelsPath, header = FALSE) 
testLabels  <- read.table(testLabelsPath, header = FALSE)
str(trainLabels)
str(testLabels)
table(trainLabels)
table(testLabels)

# c.2 measurements
trainSetPath <- paste (dataSetDir, "train", "X_train.txt", sep="/")
testSetPath <-  paste (dataSetDir, "test", "X_test.txt", sep="/")
trainSet <- read.table(trainSetPath, header = FALSE) 
testSet  <- read.table(testSetPath, header = FALSE)
dim(trainSet)
dim(testSet)

# d. merge datasets vertically, adding rows but keeping the same columns

# d.1 subjects
mergedSubjects <- rbind(trainSubjects,testSubjects)
dim(mergedSubjects)
str(mergedSubjects)

# d.2 activity codes
mergedLabels <- rbind(trainLabels,testLabels)
dim(mergedLabels)
str(mergedLabels)

# d.3 measurements
mergedSet <- rbind(trainSet,testSet)
dim(mergedSet)
#str(mergedSet)

# e. read feature and activity labels

# e.1 read as-is
featuresPath <-  paste (dataSetDir, "features.txt", sep="/")
activitiesPath <-  paste (dataSetDir, "activity_labels.txt", sep="/")
features <- read.table(featuresPath, header = FALSE) 
activities  <- read.table(activitiesPath, header = FALSE)

# e.2 add column names and check
colnames(features) <- c("Feature_code","Feature_str")
colnames(activities) <- c("Activity_code","Activity_str")
str(features)
str(activities)
activities

# f. renames columns of the merged measurement dataset with the feature labels
colnames(mergedSet) <- features$Feature_str
#names(mergedSet)

# g. filter the merged dataset to keep names with mean() or std() in them

# g.1 select the columns to keep
mean_std <- names(mergedSet)[grep("mean\\(\\)|std\\(\\)", names(mergedSet))]
mean_std

# g.2 subset by keeping the columns
mergedSet <- mergedSet[,mean_std]
dim(mergedSet)

# g.3. remove the parenthesis from the names
colnames(mergedSet) <- sub("\\(\\)", "", names(mergedSet))
colnames(mergedSet)

# h. add Subject and Activity columns in front
mergedSet = cbind(Subject = mergedSubjects[,1], Activity = mergedLabels[,1], mergedSet)
str(mergedSet$Subject)
str(mergedSet$Activity)
dim(mergedSet)
table(mergedSet$Subject)
table(mergedSet$Activity)
colnames(mergedSet)

# i. add activity labels to the merged dataset
# Activity becomes a factor
# as Activity is a factor, we loose the initial coding in writing the final dataset
# we could create an additional variable instead
str(mergedSet$Activity)
mergedSet$Activity <- apply (mergedSet["Activity"],1,function(x) activities[x,2])
table(mergedSet$Activity)
str(mergedSet$Activity)
dim(mergedSet)  

# j. aggregate and calculate the mean by subject and activity
tidy <- aggregate(. ~ Subject + Activity, data=mergedSet, mean)
dim(tidy)
names(tidy)

# k. save the tidy dataset in data
tidyPath <- paste(dataDir, "tidy.txt", sep="/")
write.table(tidy, tidyPath, sep="\t", col.names=T, row.names = T, quote=T)
# verify data
v <- read.table(tidyPath, sep="\t")
dim(v)
