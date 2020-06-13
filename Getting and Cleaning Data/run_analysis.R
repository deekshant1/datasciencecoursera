#Loadind dplyr package
library(dplyr)

#Downloading the dataset
filename <- "getdata_projectfiles_UCI HAR Dataset.zip"

#Checking it the file already exists
if(!file.exists(filename))
{
  fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(fileURL, filename, "curl")
}

#Checking if the folder already exists
if (!file.exists("UCI HAR Dataset"))
{
  unzip(filename)
}

#Assigning names to variables
features <- read.table("UCI HAR Dataset/features.txt", col.names = c("index", "feature"))
activities <- read.table("UCI HAR Dataset/activity_labels.txt", col.names = c("classLables", "activity"))
sub_test <- read.table("UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
x_test <-read.table("UCI HAR Dataset/test/X_test.txt", col.names = features$feature)
y_test <- read.table("UCI HAR Dataset/test/y_test.txt", col.names = "index")
sub_train <- read.table("UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
x_train <- read.table("UCI HAR Dataset/train/X_train.txt", col.names = features$feature)
y_train <- read.table("UCI HAR Dataset/train/y_train.txt", col.names = "index")

# 1.Merges the training and the test sets to create one data set.
X <- rbind(x_train, x_test)
Y <- rbind(y_train, y_test)
subject <- rbind(sub_train, sub_test)
Merged_data <- cbind(subject, Y, X)

# 2.Extracts only the measurements on the mean and standard deviation for each measurement. 
tidyData <- Merged_data %>% select(subject, index, contains("mean"), contains("std"))

# Tidy Data
write.table(tidyData, "TidyData.txt", row.name=FALSE)


# 3.Uses descriptive activity names to name the activities in the data set
tidyData$index <- activities[tidyData$index, 2]

# 4.Appropriately labels the data set with descriptive variable names.
names(tidyData)[2]="activity"
names(tidyData) <- gsub("Acc", "Accelerometer", names(tidyData))
names(tidyData) <- gsub("Gyro", "Gyroscope", names(tidyData))
names(tidyData) <- gsub("BodyBody", "Body", names(tidyData))
names(tidyData) <- gsub("Mag", "Magnitude", names(tidyData))
names(tidyData) <- gsub("^t", "Time", names(tidyData))
names(tidyData) <- gsub("^f", "Frequency", names(tidyData))
names(tidyData) <- gsub("tBody", "TimeBody", names(tidyData))
names(tidyData) <- gsub("-mean()", "Mean", names(tidyData), ignore.case = TRUE)
names(tidyData) <- gsub("-std()", "Standard Deviation", names(tidyData), ignore.case = TRUE)
names(tidyData) <- gsub("-freq", "Frequency", names(tidyData), ignore.case = TRUE)
names(tidyData) <- gsub("angle", "Angle", names(tidyData))
names(tidyData) <- gsub("gravity", "Gravity", names(tidyData))

# 5.From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
FinalData <- tidyData %>%
  group_by(subject, activity) %>%
  summarise_all(list(mean=mean))
write.table(FinalData, "FinalData.txt", row.name=FALSE)