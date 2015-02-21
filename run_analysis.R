## The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set. 
## 1) a tidy data set as described below
## 2) a link to a Github repository with your script for performing the analysis,
## 3) a code book that describes the variables, the data, and any transformations 
##    or work that you performed to clean up the data called CodeBook.md
## You should also include a README.md in the repo with your scripts. T
## This repo explains how all of the scripts work and how they are connected. 

## A full description is available at the site where the data was obtained: 
## http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 

##----- STEP 0: PRELIMINARY STEPS BEFORE LOAD THE DATA ----------------------##
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

##----- STEP 1: LOAD THE DATA FROM FILE -------------------------------------##
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


##----- STEP 2: SELECT MEAN AND STD VARIABLES --------------------------------##
##------AND ASSIGN VARIABLES NAMES ACROSS LOADED DATASETS --------------------##

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

## Get the indices of the required column names to filter the columns
meanStdColumns <- grep("mean|std", features$V2, value = FALSE)
str(meanStdColumns)
## Get the Actual columns name to be assigned to the dataset after columns that are not required have been removed
meanStdColumnNames <- grep("mean|std", features$V2, value = TRUE)
str(meanStdColumnNames)

## Cleaning up the Variable names. 
## 1. Converting '()-' to '_'
## 2. Converting '()' to '_NA'
## 3. Converting '-' to '_'
meanStdColumnNames <- gsub("\\(\\)-", "_", meanStdColumnNames)
str(meanStdColumnNames)
meanStdColumnNames <- gsub("\\(\\)", "_NA", meanStdColumnNames)
str(meanStdColumnNames)
meanStdColumnNames <- gsub("-", "_", meanStdColumnNames)
str(meanStdColumnNames)
meanStdColumnNames



## Pick out Mean and Std columns and Filter out the other columns from Training and Test datasets
XTrainMeanStd <- XTrain %>% select(meanStdColumns)
str(XTrainMeanStd)

XTestMeanStd <- XTest %>% select(meanStdColumns)
str(XTestMeanStd)

## Assign Filtered Columnn Names to the Training and Test datasets
colnames(XTrainMeanStd) <- meanStdColumnNames
colnames(XTestMeanStd ) <- meanStdColumnNames

str(XTrainMeanStd)
str(XTestMeanStd)

##----- STEP 3: BIND ACTIVITY AND SUBJECT DATA TO TRAINING AND TEST DATASETS -##
##----- ASSIGN MEANINGFUL LABEL TO ACTIVITY ----------------------------------##

## Bind Subject and Training Activity data to the Training and Test Datasets
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

##----- STEP 4: COMBINE BOTH TRAINING AND TEST DATASETS ----------------------##


## Merge both training and test into one data set
mergedData <- bind_rows(XTrainMeanStdActivitySubject, XTestMeanStdActivitySubject) 
str(mergedData)


##----- STEP 4: MELT THE DATA AND SPLIT MEASUREMENT OUTCOME AND AXIS ---------##
##----- ASSIGN MEANINGFUL LABEL TO ACTIVITY ----------------------------------##

## Melt the data using the gather function from Tidyr
play <- mergedData %>% select(Activity, Activity_ID:tBodyAcc_mean_Z, tBodyGyroMag_mean_NA:tBodyGyroJerkMag_std_NA)
str(play)

play2 <- gather(play, Measurement_Outcome_Axis, value, -Activity, -Activity_ID, -Subject_ID)
str(play2)

## seperate measurement_outcome_axis
play3 <- separate(data = play2, col = Measurement_Outcome_Axis, into = c("Measurement", "Outcome", "Axis"))
str(play3)
head(play3, 100)
tail(play3, 100)
play3
play4 <- play3 %>% mutate(Domain = ifelse(startsWith(measurement, 't', ignore.case=TRUE), "Time", "Frequency"))
str(play4)
startsWith(str, pattern, trim=FALSE, ignore.case=FALSE)
## END ##

## 1. Merges the training and the test sets to create one data set.

## 2. Extracts only the measurements on the mean and standard deviation for each measurement.

## 3. Uses descriptive activity names to name the activities in the data set

## 4. Appropriately labels the data set with descriptive variable names. 

## 5, From the data set in step 4, creates a second, independent tidy data set with the average 
##    of each variable for each activity and each subject.