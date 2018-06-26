# Merges the training and the test sets to create one data set.
# Extracts only the measurements on the mean and standard deviation for each measurement.
# Uses descriptive activity names to name the activities in the data set
# Appropriately labels the data set with descriptive variable names.
# From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

# http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

# Here are the data for the project:
    
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
library(plyr)


if (!file.exists("SmartphoneData.zip")) {
    download.file("https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip","SmartphoneData.zip")
}

path<-paste(getwd(),"/SmartPhoneData", sep="")
unzip("SmartphoneData.zip",exdir=path)


Activities <-read.table("./SmartPhoneData/UCI HAR Dataset/activity_labels.txt",col.names=c("Code","Activity"))
Movement <-read.table("./SmartPhoneData/UCI HAR Dataset/features.txt",col.names=c("Code","Movement"))

# Read training and test data
X_train <- read.table("./SmartPhoneData/UCI HAR Dataset/train/X_train.txt")
Y_train <- read.table("./SmartPhoneData/UCI HAR Dataset/train/y_train.txt")
Subject_train <- read.table("./SmartPhoneData/UCI HAR Dataset/train/subject_train.txt")

X_test <- read.table("./SmartPhoneData/UCI HAR Dataset/test/X_test.txt")
Y_test <- read.table("./SmartPhoneData/UCI HAR Dataset/test/y_test.txt")
Subject_test <- read.table("./SmartPhoneData/UCI HAR Dataset/test/subject_test.txt")

# 1. Merge training and test data
X_Total<-rbind(X_train,X_test)
Y_Total<-rbind(Y_train,Y_test)
Subject_Total<-rbind(Subject_train,Subject_test)

# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
Selected_Movements<-Movement[grep("mean\\(\\)|std\\(\\)",Movement[,2]),2]
Selected_X<-X_Total[,Selected_Movements]

# apply variable names and Correct typos
names(Selected_X) <- gsub("BodyBody", "Body", Selected_Movements)
names(Selected_X)

# 3. Uses descriptive activity names to name the activities in the data set
names(Y_Total)<-"Code"
Activity_Names <- join(Y_Total,Activities)

# combine subject and activity to measurement data
All_Selected<-cbind(Subject_Total,Activity_Names[,2],Selected_X)
names(All_Selected)<-c("Subject","Activity",names(Selected_X))

# 5 Create second independent data set with the average of each variable for each activity and subject
Averaged_data <-ddply(All_Selected,.(Subject,Activity),function(x) colMeans(x[, 3:68]))
write.table(Averaged_data,"tidy.txt",row.names=FALSE,quote=FALSE)
