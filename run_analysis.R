run_analysis.R <- function() {
  setInternet2(TRUE)   
  library(dplyr)
  url_proj <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(url_proj, destfile="Dataset.zip", mode="wb")
  unzip("/home/rstudio/Dataset.zip")
  
  # 0. Labels the data sets.
  
  features <-read.table("/home/rstudio/UCI HAR Dataset/features.txt", col.names = c("n", "functions"))
  
  activity_labels <- read.table("/home/rstudio/UCI HAR Dataset/activity_labels.txt", col.names = c("code", "activity"))
  
  subject_train <- read.table("/home/rstudio/UCI HAR Dataset/train/subject_train.txt", col.names = "subject")
  subject_test <- read.table("/home/rstudio/UCI HAR Dataset/test/subject_test.txt", col.names = "subject")
  
  x_train <- read.table("/home/rstudio/UCI HAR Dataset/train/X_train.txt", col.names = features$functions)
  y_train <- read.table("/home/rstudio/UCI HAR Dataset/train/y_train.txt", col.names = "code")
  
  x_test <- read.table("/home/rstudio/UCI HAR Dataset/test/X_test.txt", col.names = features$functions)
  y_test <- read.table("/home/rstudio/UCI HAR Dataset/test/y_test.txt", col.names = "code")
  
  # 1. Merges the training and the test sets to create one data set.
  
  x_merged <- rbind(x_train, x_test)
  y_merged <- rbind(y_train, y_test)
  subject_merged <- rbind(subject_train, subject_test)
  all_merged <- cbind(subject_merged, x_merged, y_merged)
  
  # 2. Extracts only the measurements on the mean and standard deviation for each measurement.
  
  tidydata <- all_merged %>%
    select(subject, code, contains("mean"), contains("std"))
  
  # 3. Uses descriptive activity names to name the activities in the data set
  
  tidydata$code <- activity_labels[tidydata$code, 2]

  # 4. Appropriately labels the data set with descriptive variable names. 
  
  names(tidydata)[2] = "activity"
  names(tidydata)<-gsub("Acc", "Accelerometer", names(tidydata))
  names(tidydata)<-gsub("Gyro", "Gyroscope", names(tidydata))
  names(tidydata)<-gsub("BodyBody", "Body", names(tidydata))
  names(tidydata)<-gsub("Mag", "Magnitude", names(tidydata))
  names(tidydata)<-gsub("^t", "Time", names(tidydata))
  names(tidydata)<-gsub("^f", "Frequency", names(tidydata))
  names(tidydata)<-gsub("tBody", "TimeBody", names(tidydata))
  names(tidydata)<-gsub("-mean()", "Mean", names(tidydata), ignore.case = TRUE)
  names(tidydata)<-gsub("-std()", "STD", names(tidydata), ignore.case = TRUE)
  names(tidydata)<-gsub("-freq()", "Frequency", names(tidydata), ignore.case = TRUE)
  names(tidydata)<-gsub("angle", "Angle", names(tidydata))
  names(tidydata)<-gsub("gravity", "Gravity", names(tidydata))  

  # 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
  
  FinalData <- tidydata %>%
    group_by(subject, activity) %>%
    summarise_all(list(mean))
  write.table(FinalData, "FinalData.txt", row.name=FALSE)
  
  FinalData
}
