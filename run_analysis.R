## Assignment 2
# 1) Merges the training and the test sets to create one data set.
# 2) Extracts only the measurements on the mean and standard deviation for each measurement.
# 3) Uses descriptive activity names to name the activities in the data set
# 4) Appropriately labels the data set with descriptive variable names.
# 5) From the data set in step 4, creates a second, independent tidy data set with the 
#    average of each variable for each activity and each subject.

library(data.table)

# PART 0 - Find and load datasets needed.
data_dir <- "UCI HAR Dataset/"
if (!file.exists(data_dir)) { 
    unzip("getdata-projectfiles-UCI HAR Dataset.zip")
}

features_meta_data <- read.table(paste(data_dir, "features.txt", sep = ""))
activity_labels <- read.table(paste(data_dir, "activity_labels.txt", sep = ""))

train_dir <- paste(data_dir, "train/", sep = "")
subject_train <- read.table(paste(train_dir, "subject_train.txt", sep = ""))
x_train <- read.table(paste(train_dir, "X_train.txt", sep = ""))
y_train <- read.table(paste(train_dir, "y_train.txt", sep = ""))

test_dir <- paste(data_dir, "test/", sep = "")
subject_test <- read.table(paste(test_dir, "subject_test.txt", sep = ""))
x_test <- read.table(paste(test_dir, "X_test.txt", sep = ""))
y_test <- read.table(paste(test_dir, "y_test.txt", sep = ""))


## PART 1 - Merge the training and the test sets to create one data set.
subject <- rbind(subject_train, subject_test)
features <- rbind(x_train, x_test)
activity <- rbind(y_train, y_test)

colnames(features) <- t(features_meta_data[2])
colnames(subject) <- "Subject"
colnames(activity) <- "Activity"
completeData <- cbind(features, activity, subject)


# Part 2 - Extracts only the measurements on the mean and standard deviation for each measurement.
columnsWithMeanSTD <- grep(".*Mean.*|.*Std.*", names(completeData), ignore.case = TRUE)
requiredColumns <- c(columnsWithMeanSTD, 562, 563)
dim(completeData)
data <- completeData[,requiredColumns]
dim(data)

# Part 3 - Uses descriptive activity names to name the activities in the data set
data$Activity <- as.character(data$Activity)
for (i in 1:6){
    data$Activity[data$Activity == i] <- as.character(activity_labels[i,2])
}

data$Activity <- as.factor(data$Activity)


# Part 4 - Appropriately labels the data set with descriptive variable names
names(data)

names(data)<-gsub("Acc", "Accelerometer", names(data))
names(data)<-gsub("Gyro", "Gyroscope", names(data))
names(data)<-gsub("BodyBody", "Body", names(data))
names(data)<-gsub("Mag", "Magnitude", names(data))
names(data)<-gsub("^t", "Time", names(data))
names(data)<-gsub("^f", "Frequency", names(data))
names(data)<-gsub("tBody", "TimeBody", names(data))
names(data)<-gsub("-mean()", "Mean", names(data), ignore.case = TRUE)
names(data)<-gsub("-std()", "STD", names(data), ignore.case = TRUE)
names(data)<-gsub("-freq()", "Frequency", names(data), ignore.case = TRUE)
names(data)<-gsub("angle", "Angle", names(data))
names(data)<-gsub("gravity", "Gravity", names(data))


# Part 5 - From the data set in step 4, creates a second, independent tidy data set with the average of 
# each variable for each activity and each subject
data$Subject <- as.factor(data$Subject)
data <- data.table(data)

tidyData <- aggregate(. ~Subject + Activity, data, mean)
tidyData <- tidyData[order(tidyData$Subject, tidyData$Activity),]
write.table(tidyData, file = "tidy_data.txt", row.names = FALSE)