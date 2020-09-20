## Step 1. Merging train and test data into one file

##Downloading and unzipping the files
if(!file.exists("./data")){dir.create("./data")}
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(url, destfile = "./data/data.zip")
unzip(zipfile = "./data/data.zip", exdir = "./data")

## reading features data, as it will be used for column names

features <- read.table("./data/UCI HAR Dataset/features.txt")

## reading training data, assigning column names and merging columns together
subject_train <- read.table("./data/UCI HAR Dataset/train/subject_train.txt", col.names = "subjectID")
y_train <- read.table("./data/UCI HAR Dataset/train/y_train.txt", col.names = "activityID")
X_train <- read.table("./data/UCI HAR Dataset/train/X_train.txt", col.names = features[, 2])
data_train <- cbind(subject_train, y_train, X_train)

## reading test data, assigning column names and merging columns together
subject_test <- read.table("./data/UCI HAR Dataset/test/subject_test.txt", col.names = "subjectID")
y_test <- read.table("./data/UCI HAR Dataset/test/y_test.txt", col.names = "activityID")
X_test <- read.table("./data/UCI HAR Dataset/test/X_test.txt", col.names = features[, 2])
data_test <- cbind(subject_test, y_test, X_test)

##merging training and test data together
full_data <- rbind(data_train, data_test)

## Step 2. Extracting the measurements on the mean and standard deviation for 
## each measurement

library(dplyr)

## finding relevant column names with words "mean" and "std"
data_MeanAndSTD <- full_data %>%
    select(subjectID, activityID, contains("mean"), contains("std"))

## Step 3. Adding descriptive activity names to the activities in the data set

## reading activity labels and assigning column names
activity_labels <- read.table("./data/UCI HAR Dataset/activity_labels.txt", 
                              col.names = c("activityID", "activityLabel"))

# merging data from data_MeanAndSTD and activity_labels
data_withActivity <- merge(data_MeanAndSTD, activity_labels, by = "activityID", 
                           all.x = TRUE)

## rearranging columns so that subjectID goes first, followed by activityID, 
## activity label and all measures
data_withActivity <- subset(data_withActivity, 
                            select = c(subjectID, activityID, activityLabel, 
                                       tBodyAcc.mean...X:fBodyBodyGyroJerkMag.std..))

## Step 4. Labeling the data set with descriptive variable names

## using gsub function to change elements of column titles
data_NewNames <- data_withActivity
names(data_NewNames) <- gsub("^t", "Time", names(data_NewNames))
names(data_NewNames) <- gsub("tBody", "TimeBody", names(data_NewNames))
names(data_NewNames) <- gsub("^f", "Frequency", names(data_NewNames))
names(data_NewNames) <- gsub("-freq()", "Frequency", names(data_NewNames), 
                             ignore.case = TRUE)
names(data_NewNames) <- gsub("Acc", "Accelerometer", names(data_NewNames))
names(data_NewNames) <- gsub("angle", "Angle", names(data_NewNames))
names(data_NewNames) <- gsub("BodyBody", "Body", names(data_NewNames))
names(data_NewNames) <- gsub("gravity", "Gravity", names(data_NewNames))
names(data_NewNames) <- gsub("Gyro", "Gyroscope", names(data_NewNames))
names(data_NewNames) <- gsub("Mag", "Magnitude", names(data_NewNames))

## Step 5. Creating a new data set with the average of each variable for each 
## activity and each subject

##removing activityID column, grouping by subject and activity, 
##and calculating means for all variables
NewData <- data_NewNames %>%
    select(-activityID) %>%
    group_by(subjectID, activityLabel) %>%
    summarise_all(funs(mean))

##creating a file with new data
write.table(NewData, "NewData.txt", row.names = FALSE)

