# Project - Getting and Cleaning Data

- The run_analysis.R script does the data preparation and then the 5 steps required in the course project.

# Downloading the dataset
- Dataset is downloaded through "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip" and extracted under UCI HAR Dataset using unzip() function.

# Assigning each data to variables
### features <- features.txt
- This has 561 rows and 2 columns. The features selected for this database is from the accelerometer and gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ respectively.

### activities <- activities.txt
- This has 6 rows and 2 columns.
- List of activities performed when the corresponding measurements were taken and its codes (labels)

### subject_test <- test/subject_test.txt
- This contains 2947 rows and 1 column
- This contains the test data of 9/30 volunteer test subjects

### x_test <- test/X_test.txt
- It contains 2947 rows and 561 columns
- This contains recorded features of test data

### y_test <- test/y_test.txt
- It has 2947 rows and 1 column
- This contains test data of activities index labels

### subject_train <- test/subject_train.txt
- This has 7352 rows and 1 column
- It contains train data of 21/30 volunteer for train subjects

### x_train <- test/X_train.txt
- This has 7352 rows and 561 columns
- It contains recorded features of train data

### y_train <- test/y_train.txt
- This has 7352 rows and 1 column
- This contains train data of activities index labels

# Required Steps

## 1.Merges the training and the test sets to create one data set.
### X
- This contains 10299 rows and 561 columns
- Created by merging the x_train and x_test using rbind() function

### Y
- This has 10299 rows and 1 column
- Created by merging y_train and y_test using rbind() function

### subject
- This has 10299 rows and 1 column
- This is created by merging subject_train and subject_test using rbind() function

### Merged_data
- This contains 10299 rows and 563 columns
- Created by merging subject, Y and X using cbind() function

## 2. Extracts only the measurements on the mean and standard deviation for each measurement.
### tidyData
- This has 10299 rows and 88 columns
- It is created by subsetting Merged_data and selecting only the columns: subject, index and the measurements over the mean and standard deviation (std) for each measurement
- Then exporting tidyData to tidyData.txt file

## 3. Uses descriptive activity names to name the activities in the data set
- Entire numbers in index column of the tidyData is replaced with corresponding activity taken from second column of the activities variable

## 4. Appropriately labels the data set with descriptive variable names.
- 'index' column in the 'tidyData' is renamed into 'activities'
- 'Acc' is replaced by 'Accelerometer'
- 'Gyro' is replaced by 'Gyroscope'
- 'BodyBody' is replaced by 'Body'
- 'Mag' is replaced by 'Magnitude'
- characters that start with 'f' is replaced by 'Frequency'
- character that start with 't' is replaced by 'Time'

## 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
### newData
- This has 180 rows and 88 columns
- It is created by sumarizing tidyData, taking the means of each variable for each activity and each subject, after grouping by subject and activity.
- Then exporting newData to newData.txt file
