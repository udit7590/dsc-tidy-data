# Data Set Contains Following
# subject_id
# volunteer_type
# total-acc-x
# total-acc-y
# total-acc-z
# body-acc-x
# body-acc-y
# body-acc-z
# gyro-x
# gyro-y
# gyro-z
# Unit Acceleration
# Unit Gyro
# ActivityID
# Feature

# ------------------------------------------------------------------------------------------------------------------------------
# 1 : Merges the training and the test sets to create one data set
#     by Loading All Data Given into our workspace and merging them all.
# ------------------------------------------------------------------------------------------------------------------------------

# Activity labels - ActivityID, ActivityName
library(tidyr)
con <- file("activity_labels.txt", 'r')
activity_labels_filedata <- readLines(con)
close(con)
activityLabels <- separate(as.data.frame(activity_labels_filedata), activity_labels_filedata, c("activity_id", "activity_name"))[,c('activity_id', 'activity_name')]

# Feature labels - FeatureID, FeatureName
library(tidyr)
con <- file("features.txt", 'r')
feature_labels_filedata <- read.table(con)
featureLabels <- as.data.frame(feature_labels_filedata[,c("V1", "V2")])
close(con)

# test-data
con <- file("test/subject_test.txt", 'r')
testSubjects <- read.table(con)
close(con)
con <- file("test/Inertial Signals/body_acc_x_test.txt", 'r')
testBodyAcclerationsX <- read.table(con)
close(con)
con <- file("test/Inertial Signals/body_acc_y_test.txt", 'r')
testBodyAcclerationsY <- read.table(con)
close(con)
con <- file("test/Inertial Signals/body_acc_z_test.txt", 'r')
testBodyAcclerationsZ <- read.table(con)
close(con)
con <- file("test/Inertial Signals/body_gyro_x_test.txt", 'r')
testBodyGyrosX <- read.table(con)
close(con)
con <- file("test/Inertial Signals/body_gyro_y_test.txt", 'r')
testBodyGyrosY <- read.table(con)
close(con)
con <- file("test/Inertial Signals/body_gyro_z_test.txt", 'r')
testBodyGyrosZ <- read.table(con)
close(con)
con <- file("test/Inertial Signals/total_acc_x_test.txt", 'r')
testTotalAcclerationsX <- read.table(con)
close(con)
con <- file("test/Inertial Signals/total_acc_y_test.txt", 'r')
testTotalAcclerationsY <- read.table(con)
close(con)
con <- file("test/Inertial Signals/total_acc_z_test.txt", 'r')
testTotalAcclerationsZ <- read.table(con)
close(con)

con <- file("test/X_test.txt", 'r')
testFeaturesData <- read.table(con)
close(con)
con <- file("test/y_test.txt", 'r')
testActivitiesData <- read.table(con)
close(con)

testData <- data.frame(subject_id=testSubjects, features=testFeaturesData, activities=testActivitiesData,
            body_acc_x=testBodyAcclerationsX, body_acc_y=testBodyAcclerationsY, body_acc_z=testBodyAcclerationsZ,
            body_gyro_x=testBodyGyrosX, body_gyro_y=testBodyGyrosY, body_gyro_z=testBodyGyrosZ,
            total_acc_x=testTotalAcclerationsX, total_acc_y=testTotalAcclerationsY, total_acc_z=testTotalAcclerationsZ)
testData$subjectType <- c("Test") # 2947 rows

# train-data
con <- file("train/subject_train.txt", 'r')
trainSubjects <- read.table(con)
close(con)
con <- file("train/Inertial Signals/body_acc_x_train.txt", 'r')
trainBodyAcclerationsX <- read.table(con)
close(con)
con <- file("train/Inertial Signals/body_acc_y_train.txt", 'r')
trainBodyAcclerationsY <- read.table(con)
close(con)
con <- file("train/Inertial Signals/body_acc_z_train.txt", 'r')
trainBodyAcclerationsZ <- read.table(con)
close(con)
con <- file("train/Inertial Signals/body_gyro_x_train.txt", 'r')
trainBodyGyrosX <- read.table(con)
close(con)
con <- file("train/Inertial Signals/body_gyro_y_train.txt", 'r')
trainBodyGyrosY <- read.table(con)
close(con)
con <- file("train/Inertial Signals/body_gyro_z_train.txt", 'r')
trainBodyGyrosZ <- read.table(con)
close(con)
con <- file("train/Inertial Signals/total_acc_x_train.txt", 'r')
trainTotalAcclerationsX <- read.table(con)
close(con)
con <- file("train/Inertial Signals/total_acc_y_train.txt", 'r')
trainTotalAcclerationsY <- read.table(con)
close(con)
con <- file("train/Inertial Signals/total_acc_z_train.txt", 'r')
trainTotalAcclerationsZ <- read.table(con)
close(con)

con <- file("train/X_train.txt", 'r')
trainFeaturesData <- read.table(con)
close(con)
con <- file("train/y_train.txt", 'r')
trainActivitiesData <- read.table(con)
close(con)

trainData <- data.frame(subject_id=trainSubjects, features=trainFeaturesData, activities=trainActivitiesData,
            body_acc_x=trainBodyAcclerationsX, body_acc_y=trainBodyAcclerationsY, body_acc_z=trainBodyAcclerationsZ,
            body_gyro_x=trainBodyGyrosX, body_gyro_y=trainBodyGyrosY, body_gyro_z=trainBodyGyrosZ,
            total_acc_x=trainTotalAcclerationsX, total_acc_y=trainTotalAcclerationsY, total_acc_z=trainTotalAcclerationsZ)
trainData$subjectType <- c("Train") # 7352 rows

# SOLUTION TO Point 1: Merges the training and the test sets to create one data set
# Final Data
completeData <- rbind(testData, trainData) # 10299 rows
names(completeData) <- c("SubjectID", as.character(featureLabels[,"V2"]), "ActivityID", names(completeData)[564:1716])


# ------------------------------------------------------------------------------------------------------------------------------
# 2 : Extracts only the measurements on the mean and standard deviation for each measurement.
# ------------------------------------------------------------------------------------------------------------------------------
meanAndSDColumns <- grep("-mean\\(|-std\\(", as.vector(featureLabels$V2), value=TRUE)
extractedData <- completeData[,c("SubjectID", "ActivityID", "subjectType", meanAndSDColumns)]


# ------------------------------------------------------------------------------------------------------------------------------
# 3 : Uses descriptive activity names to name the activities in the data set
# ------------------------------------------------------------------------------------------------------------------------------
extractedData <- merge(extractedData, activityLabels, by.x="ActivityID", by.y="activity_id")


# ------------------------------------------------------------------------------------------------------------------------------
# 4 : Appropriately labels the data set with descriptive variable names.
# ------------------------------------------------------------------------------------------------------------------------------

# Already Done

# ------------------------------------------------------------------------------------------------------------------------------
# 5 : From the data set in step 4, creates a second, independent tidy data set with the
#     average of each variable for each activity and each subject.
# ------------------------------------------------------------------------------------------------------------------------------
library(dplyr)
groupedData <- group_by(extractedData, SubjectID, ActivityID)
summarizedData <- summarise_each(groupedData, funs(mean))
summarizedData <- select(summarizedData, -activity_name)
summarizedData <- select(summarizedData, -subjectType)

write.table(summarizedData, "handsetData.txt", row.names = FALSE)


