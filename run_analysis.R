## run_analysis.R

## data = https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

## Merges the training and the test sets to create one data set.
## Extracts only the measurements on the mean and standard deviation for each measurement. 
## Uses descriptive activity names to name the activities in the data set
## Appropriately labels the data set with descriptive variable names. 
## From the data set in step above, creates a second, independent tidy data set 
##        with the average of each variable for each activity and each subject.


## read in libraries needed 
library(data.table)
library(dplyr)
library(plyr)
library(reshape2)

## set working directory if needed.  Uncomment if needed.
## setwd("/Users/edlewis4/coursera/workingdirectory-R")

## GetZip file data from URL
fileUrl <- "http://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
if(!file.exists("./UCI HAR Dataset/")) { download.file(fileUrl, destfile="./getdata-projectfiles-UCI HAR Dataset.zip", method="curl") ;
unzip("./getdata-projectfiles-UCI HAR Dataset.zip", exdir="./") }

## Read in necessary datasets
## Read in train datasets
trainx <- read.table("./UCI HAR Dataset/train/X_train.txt")
train_y_activity <- read.table("./UCI HAR Dataset/train/y_train.txt")
train_subjects <- read.table("./UCI HAR Dataset/train/subject_train.txt")

## Read in test datasets
testx <- read.table("./UCI HAR Dataset/test/X_test.txt")
test_y_activity <- read.table("./UCI HAR Dataset/test/y_test.txt")
test_subjects <- read.table("./UCI HAR Dataset/test/subject_test.txt")

## Read in activity & feature labels datasets
activitylabels <- read.table("./UCI HAR Dataset/activity_labels.txt")
featurelabels <- read.table("./UCI HAR Dataset/features.txt")

## convert the featurelabels to a datatable to select necessary mean and std columns
column_names <- data.table(featurelabels)   ## convert to data.table

## subset data.table to get only those items that have std() or mean() in the feature name
## column_names <- column_names[column_names$V2 %like% "std()" | column_names$V2 %like% "mean()",]
column_names <- column_names[column_names$V2 %like% "std()" | column_names$V2 %like% "mean()" & !(column_names$V2 %like% "meanFreq"),]

keep_column_nums <- as.vector(column_names$V1)   ## vector of numbers to keep
keep_column_names <- as.vector(column_names$V2)  ## vector of names to keep

## create subset of trainx and testx - only keep columns that have std or mean data
## as determined above with keep_column_nums
trainx_subset <- trainx[,keep_column_nums]  
testx_subset <- testx[,keep_column_nums]


## Merge Train datasets into one and set column names
## train <- cbind(train_subjects, train_y_activity, trainx)
train <- cbind(train_subjects, train_y_activity, trainx_subset)
names(train) <- c("Subject_num","Activity", keep_column_names)

## Merge Test datasets into one and set column names
## test <- cbind(test_subjects, test_y_activity, testx)
test <- cbind(test_subjects, test_y_activity, testx_subset)
names(test) <- c("Subject_num","Activity", keep_column_names)


## Merge test and train datasets
merge_df <- rbind(test,train)


## Use the descriptive Activity names from the activitylabels table instead of numbers
## This is data pulled from the activity_labels.txt file into activitylabels table
## uses plyr function mapvalues to change values from numbers to Descriptive activities 
## This is step3 of assignment
merge_df$Activity <- mapvalues(merge_df$Activity, from = activitylabels$V1, to = as.character(activitylabels$V2))

## use dplyr melt function to make long form tidy dataset with Subject_num and Activiy
## All other observations will then be under a variable / value 
result <- melt(merge_df, id.vars = c("Subject_num","Activity"))

## use dply dcast function to create summary by Subject_num and Activity and the mean of all features
result <- dcast(result, Subject_num + Activity ~ variable, mean)
result$Activity <- as.factor(result$Activity)  ## Convert Activity back to a Factor 

## Write final summary table to output file - 
write.table(result, file="./samsung_HAR_out.txt", row.name=FALSE, col.names=TRUE)




