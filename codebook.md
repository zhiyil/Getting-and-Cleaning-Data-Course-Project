# Description of the dataset 
The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain. See 'features_info.txt' for more details. 

# Description of the variables in the tidy dataset
The features selected for this database come from the accelerometer and gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ. These time domain signals (prefix ‘t’ to denote time) were captured at a constant rate of 50 Hz. and the acceleration signal was then separated into body and gravity acceleration signals (tBodyAcc-XYZ and tGravityAcc-XYZ) – both using a low pass Butterworth filter.

The body linear acceleration and angular velocity were derived in time to obtain Jerk signals (tBodyAccJerk-XYZ and tBodyGyroJerk-XYZ). Also the magnitude of these three-dimensional signals were calculated using the Euclidean norm (tBodyAccMag, tGravityAccMag, tBodyAccJerkMag, tBodyGyroMag, tBodyGyroJerkMag).

A Fast Fourier Transform (FFT) was applied to some of these signals producing fBodyAcc-XYZ, fBodyAccJerk-XYZ, fBodyGyro-XYZ, fBodyAccJerkMag, fBodyGyroMag, fBodyGyroJerkMag. (Note the ‘f’ to indicate frequency domain signals).

## Description of abbreviations of measurements

leading t or f is based on time or frequency measurements.
Body = related to body movement.
Gravity = acceleration of gravity
Acc = accelerometer measurement
Gyro = gyroscopic measurements
Jerk = sudden movement acceleration
Mag = magnitude of movement
mean and SD are calculated for each subject for each activity for each mean and SD measurements.
The units given are g’s for the accelerometer and rad/sec for the gyro and g/sec and rad/sec/sec for the corresponding jerks.

These signals were used to estimate variables of the feature vector for each pattern:
‘-XYZ’ is used to denote 3-axial signals in the X, Y and Z directions. They total 33 measurements including the 3 dimensions - the X,Y, and Z axes.

tBodyAcc-XYZ
tGravityAcc-XYZ
tBodyAccJerk-XYZ
tBodyGyro-XYZ
tBodyGyroJerk-XYZ
tBodyAccMag
tGravityAccMag
tBodyAccJerkMag
tBodyGyroMag
tBodyGyroJerkMag
fBodyAcc-XYZ
fBodyAccJerk-XYZ
fBodyGyro-XYZ
fBodyAccMag
fBodyAccJerkMag
fBodyGyroMag
fBodyGyroJerkMag

## The set of variables that were estimated from these signals are:

mean(): Mean value
std(): Standard deviation


# Major tasks of this R script
## Dataset download

The dataset was downloaed onto local folder of 

"~/R_for_DScourses/getting and cleaning data/Project/UCI HAR Dataset"

All data tables are read from this folder. The data in "/Inertial Signals" folder was not used for this project.

## load libraries

library(data.table)
library(reshape2)

## Read subject files, i.e. subject_test.txt to tst.sub, and subject_train.txt to trn.sub
tst.sub <- fread("~/R_for_DScourses/getting and cleaning data/Project/UCI HAR Dataset/test/subject_test.txt")
trn.sub <- fread("~/R_for_DScourses/getting and cleaning data/Project/UCI HAR Dataset/train/subject_train.txt")

## Read activity files, i.e. y_test.txt to tst.y, and y_train.txt to trn.y
tst.y <- fread("~/R_for_DScourses/getting and cleaning data/Project/UCI HAR Dataset/test/y_test.txt")
trn.y <- fread("~/R_for_DScourses/getting and cleaning data/Project/UCI HAR Dataset/train/y_train.txt")

## Read data files, i.e. X_test.txt to tst.x, and X_train.txt to trn.x
tst.x <- fread("~/R_for_DScourses/getting and cleaning data/Project/UCI HAR Dataset/test/X_test.txt")
trn.x <- fread("~/R_for_DScourses/getting and cleaning data/Project/UCI HAR Dataset/train/X_train.txt")

## 1. Merge the training and the test sets to create one data set.

### Merge the datasets as subject (for subjects), y (for activity labels), and x (for datasets), change the column names to descriptive ones

subject <- rbind(tst.sub, trn.sub)
setnames(subject, "V1", "subjectID")
activity <- rbind(tst.y, trn.y)
setnames(activity, "V1", "activityLabel")
x <- rbind(tst.x, trn.x)

### Merge the subject, activity labels, and data sets (x) into one data.table, x

subject <- cbind(subject, activity)
x <- cbind(subject, x)

### sort by subjectID then by activityLabels
setkey(x, subjectID, activityLabel) 

## 2. Extract only the measurements on the mean and standard deviation for each measurement.

### Read the feature table to dtFeatures, and set the column names
dtFeatures <- fread("~/R_for_DScourses/getting and cleaning data/Project/UCI HAR Dataset/features.txt")
setnames(dtFeatures, c("V1", "V2"), c("featureCode", "measurement"))

### Extract the measurements with mean() and std()
extractedFeatures <- dtFeatures[grepl("mean\\(\\)|std\\(\\)", measurement)]

### Generate matching column numbers, colCode (ie. the V# for the dataset x) from extractedFeatures
extractedFeatures[, c("colCode") := paste0("V", featureCode)]

### Generate a vector containing all the extracted features
vec_extractedFeatures <- extractedFeatures[,colCode]

### Generate a vector called "subxCol" to contain all the required columns and extracted features
subxCol <- c("subjectID", "activityLabel", vec_extractedFeatures)

### Subset the whole dataset, x, with subxCol, and name it as "subx"
subx <- x[,subxCol, with=FALSE]

## 3. Use descriptive activity names to name the activities in the data set

### read the activity_label file
activityLabels <- fread("~/R_for_DScourses/getting and cleaning data/Project/UCI HAR Dataset/activity_labels.txt")
setnames(activityLabels, c("V1", "V2"), c("activityLabel", "activityName"))

### merge the activityLabels with subx by activityLabel (the numbering code 1, 2, 3, 4, 5, and 6)
newx <- merge(subx, activityLabels, by = "activityLabel", all.x = TRUE)
setkey(newx, subjectID, activityLabel, activityName)

### organize the columns such that "subjectID", "activityLabel", and "activityName" are the first three ones
orderedCol <- c(key(newx), grep("V", names(newx), value = TRUE))
newx <- newx[, orderedCol, with = FALSE]

## 4. Appropriately label the data set with descriptive variable names: Put in the descriptive names for the variable columns (ie. the V# columns)
setnames(newx, names(newx)[4:length(names(newx))], extractedFeatures$measurement)

## 5. Generate a tidy data table called tidy that carries out the averages required by the project
tidy <- newx[,lapply(.SD, mean), by = key(newx), .SDcols = 4:length(names(newx))]