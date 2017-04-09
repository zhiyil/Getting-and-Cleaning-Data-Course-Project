library(data.table)
library(reshape2)

## Read subject files, i.e. subject_test.txt to tst.sub, and subject_train.txt to trn.sub ##
tst.sub <- fread("~/R_for_DScourses/getting and cleaning data/Project/UCI HAR Dataset/test/subject_test.txt")
trn.sub <- fread("~/R_for_DScourses/getting and cleaning data/Project/UCI HAR Dataset/train/subject_train.txt")

## Read activity files, i.e. y_test.txt to tst.y, and y_train.txt to trn.y ##
tst.y <- fread("~/R_for_DScourses/getting and cleaning data/Project/UCI HAR Dataset/test/y_test.txt")
trn.y <- fread("~/R_for_DScourses/getting and cleaning data/Project/UCI HAR Dataset/train/y_train.txt")

## Read data files, i.e. X_test.txt to tst.x, and X_train.txt to trn.x ##
tst.x <- fread("~/R_for_DScourses/getting and cleaning data/Project/UCI HAR Dataset/test/X_test.txt")
trn.x <- fread("~/R_for_DScourses/getting and cleaning data/Project/UCI HAR Dataset/train/X_train.txt")

## Merge the datasets as subject (for subjects), y (for activity labels), and x (for datasets), change the column names to descriptive ones ##

subject <- rbind(tst.sub, trn.sub)
setnames(subject, "V1", "subjectID")
activity <- rbind(tst.y, trn.y)
setnames(activity, "V1", "activityLabel")
x <- rbind(tst.x, trn.x)

## Merge the subject, activity labels, and data sets (x) into one data.table, x ##

subject <- cbind(subject, activity)
x <- cbind(subject, x)

## sort by subjectID then by activityLabels ##
setkey(x, subjectID, activityLabel) 

## Read the feature table to dtFeatures, and set the column names ##
dtFeatures <- fread("~/R_for_DScourses/getting and cleaning data/Project/UCI HAR Dataset/features.txt")
setnames(dtFeatures, c("V1", "V2"), c("featureCode", "measurement"))

## Extract the measurements with mean() and std() ##
extractedFeatures <- dtFeatures[grepl("mean\\(\\)|std\\(\\)", measurement)]

## Generate matching column numbers, colCode (ie. the V# for the dataset x) from extractedFeatures
extractedFeatures[, c("colCode") := paste0("V", featureCode)]

## Generate a vector containing all the extracted features ##
vec_extractedFeatures <- extractedFeatures[,colCode]

## Generate a vector called "subxCol" to contain all the required columns and extracted features ##
subxCol <- c("subjectID", "activityLabel", vec_extractedFeatures)

## Subset the whole dataset, x, with subxCol, and name it as "subx" ##
subx <- x[,subxCol, with=FALSE]

## Put in the descriptive activity labels ##

## read the activity_label file ##
activityLabels <- fread("~/R_for_DScourses/getting and cleaning data/Project/UCI HAR Dataset/activity_labels.txt")
setnames(activityLabels, c("V1", "V2"), c("activityLabel", "activityName"))

## merge the activityLabels with subx by activityLabel (the numbering code 1, 2, 3, 4, 5, and 6) ##
newx <- merge(subx, activityLabels, by = "activityLabel", all.x = TRUE)
setkey(newx, subjectID, activityLabel, activityName)

## organize the columns such that "subjectID", "activityLabel", and "activityName" are the first three ones ##
orderedCol <- c(key(newx), grep("V", names(newx), value = TRUE))
newx <- newx[, orderedCol, with = FALSE]

## Put in the descriptive names for the variable columns (ie. the V# columns)
setnames(newx, names(newx)[4:length(names(newx))], extractedFeatures$measurement)

## Generate a tidy data table called tidy that carries out the averages required by the project ##
tidy <- newx[,lapply(.SD, mean), by = key(newx), .SDcols = 4:length(names(newx))]
write.table(tidy, file = "tidy.txt", row.names = FALSE)