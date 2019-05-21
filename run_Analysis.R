#Author Vivek-v1

# this script focusses on getting and cleaning the data from a public dataset surrounding the activity

# Merges the training and the test sets to create one data set.
# Extracts only the measurements on the mean and standard deviation for each measurement.
# Uses descriptive activity names to name the activities in the data set
# Appropriately labels the data set with descriptive variable names.
# From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.


#declaration
library(data.table)
library(reshape2)
workingDir <- getwd()


#download data 
dataURL <-  "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(dataURL, file.path(workingDir, "dataFiles.zip"))
unzip(zipfile = "dataFiles.zip")


#read dataset into memory
activityLabels <- fread(file.path(workingDir, "UCI HAR Dataset/activity_labels.txt"), col.names=c("classLabels","activityName"))
features <- fread(file.path(workingDir, "UCI HAR Dataset/features.txt"), col.names = c("index", "featureNames"))
filteredFeatures <- grep("(mean|std)\\(\\)", features[, featureNames])
measurements <- features[filteredFeatures, featureNames]
measurements <- gsub('[()]', '', measurements)


# Load train datasets into memory
train <- fread(file.path(workingDir, "UCI HAR Dataset/train/X_train.txt"))[, filteredFeatures, with = FALSE]
data.table::setnames(train, colnames(train), measurements)
trainActivities <- fread(file.path(workingDir, "UCI HAR Dataset/train/Y_train.txt")
                         , col.names = c("Activity"))
trainSubjects <- fread(file.path(workingDir, "UCI HAR Dataset/train/subject_train.txt")
                       , col.names = c("SubjectNum"))
train <- cbind(trainSubjects, trainActivities, train)

# Load test datasets
test <- fread(file.path(workingDir, "UCI HAR Dataset/test/X_test.txt"))[, filteredFeatures, with = FALSE]
data.table::setnames(test, colnames(test), measurements) 

testActivities <- fread(file.path(workingDir, "UCI HAR Dataset/test/Y_test.txt"), col.names = c("Activity"))
testSubjects <- fread(file.path(workingDir, "UCI HAR Dataset/test/subject_test.txt"), col.names = c("SubjectNum"))
test <- cbind(testSubjects, testActivities, test)

# merge datasets
combined <- rbind(train, test)

# Convert class Labels to activity Names for more readability
combined[["Activity"]] <- factor(combined[, Activity]
                                 , levels = activityLabels[["classLabels"]]
                                 , labels = activityLabels[["activityName"]])

combined[["SubjectNum"]] <- as.factor(combined[, SubjectNum])
combined <- reshape2::melt(data = combined, id = c("SubjectNum", "Activity"))
combined <- reshape2::dcast(data = combined, SubjectNum + Activity ~ variable, fun.aggregate = mean)

#write data to the output file
data.table::fwrite(x = combined, file = "tidyData.csv", quote = FALSE)
