## The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set. 
## 1) a tidy data set as described below
## 2) a link to a Github repository with your script for performing the analysis,
## 3) a code book that describes the variables, the data, and any transformations 
##    or work that you performed to clean up the data called CodeBook.md
## You should also include a README.md in the repo with your scripts. T
## This repo explains how all of the scripts work and how they are connected. 

## A full description is available at the site where the data was obtained: 
## http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 

## Library Declaration
library(RCurl)
library(dplyr)
library(data.table)
library(tidyr)
## Download zip file
## https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"

if (!file.exists("week3-assign")) {
    dir.create("week3-assign")
}
download.file(fileUrl, destfile = "./week3-assign/HARuSP.zip", method="curl", mode="wb")
unzip("./week3-assign/HARuSP.zip", overwrite = TRUE)
## Unzip the file

## Load Data
XTrain <- read.table("./UCI HAR Dataset/train/X_train.txt")
XTest <- read.table("./UCI HAR Dataset/test/X_test.txt")

features <- read.table("./UCI HAR Dataset/features.txt")

subjectTrain <- read.table("./UCI HAR Dataset/train/subject_train.txt")
subjectTest <- read.table("./UCI HAR Dataset/test/subject_test.txt")

activityTrain <- read.table("./UCI HAR Dataset/train/y_train.txt")
activityTest <- read.table("./UCI HAR Dataset/test/y_test.txt")
activityLabels <- read.table("./UCI HAR Dataset/activity_labels.txt")

str(XTrain)
str(activityTrain)
str(subjectTrain)

str(XTest)
str(activityTest)
str(subjectTest)

str(features)
tail(features)
head(features)

str(activityLabels)



## Assign Columns names X_train

colnames(activityTrain) <- c("Activity_ID")
colnames(activityTest) <- c("Activity_ID")
colnames(activityLabels) <- c("Activity_ID", "Activity")
colnames(subjectTrain) <- c("Subject_ID")
colnames(subjectTest) <- c("Subject_ID")

length(unique(features$V2))

## Select Mean and Std Deviations variable names from the training and test data sets
## Taking this approach as there are a number of duplicate variable names which is causing an
## issue if you try to assign the variable names first and then select the columns
meanStdColumns <- grep("mean|std", features$V2, value = FALSE)
str(meanStdColumns)
meanStdColumnNames <- grep("mean|std", features$V2, value = TRUE)
str(meanStdColumnNames)
meanStdColumnNames <- gsub("-", "_", meanStdColumnNames)
meanStdColumnNames <- gsub("[\\(\\)]", "", meanStdColumnNames)



## Pick out Mean and Std columns and Filter out other columns
XTrainMeanStd <- XTrain %>% select(meanStdColumns)
str(XTrainMeanStd)

XTestMeanStd <- XTest %>% select(meanStdColumns)
str(XTestMeanStd)

## Assign Columnn Names
colnames(XTrainMeanStd) <- meanStdColumnNames
colnames(XTestMeanStd ) <- meanStdColumnNames

str(XTrainMeanStd)
str(XTestMeanStd)


## Bind Subject and Training Activity to the Training Data list
## Assign Data Type before the Training and Test data can be merged
XTrainMeanStdActivitySubject <- bind_cols(subjectTrain, activityTrain, XTrainMeanStd)
XTrainMeanStdActivitySubject <- mutate(XTrainMeanStdActivitySubject, DataType = "Training")
str(XTrainMeanStdActivitySubject)


## Bind Subject and Test Activity to the Test Data list
## Assign Data Type before the Training and Test data can be merged
XTestMeanStdActivitySubject <- bind_cols(subjectTest, activityTest, XTestMeanStd)
XTestMeanStdActivitySubject <- mutate(XTestMeanStdActivitySubject, DataType = "Test")
str(XTestMeanStdActivitySubject)



## Merge Activity to the Activity Lables to pull in the meaningful activity labels
XTrainMeanStdActivitySubject <- merge(XTrainMeanStdActivitySubject, activityLabels, by="Activity_ID")
str(XTrainMeanStdActivitySubject)

## Merge Activity to the Activity Lables to pull in the meaningful activity labels
XTestMeanStdActivitySubject <- merge(XTestMeanStdActivitySubject, activityLabels, by="Activity_ID")
str(XTestMeanStdActivitySubject)

## Merge both training and test into one data set
mergedData <- bind_rows(XTrainMeanStdActivitySubject, XTestMeanStdActivitySubject) 
str(mergedData)


play <- mergedData %>% select(Activity, Activity_ID:tBodyAcc_mean_Z)
str(play)

gather(play, measurement_type_axis, value, -Activity, -Activity_ID, -Subject_ID)
## END ##

## 1. Merges the training and the test sets to create one data set.

## 2. Extracts only the measurements on the mean and standard deviation for each measurement.

## 3. Uses descriptive activity names to name the activities in the data set

## 4. Appropriately labels the data set with descriptive variable names. 

## 5, From the data set in step 4, creates a second, independent tidy data set with the average 
##    of each variable for each activity and each subject.