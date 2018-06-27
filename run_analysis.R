#1. Merge the training and the test sets to create one data set.

## read the data general and training
features <- read.table("./features.txt", header=FALSE)
head(features)

activityLabel <- read.table("./activity_labels.txt", header=FALSE)
head(activityLabel)

subjectTrain <- read.table("./train/subject_train.txt", header=FALSE)
tail(subjectTrain)
xTrain <- read.table("./train/X_train.txt", header=FALSE)
yTrain <- read.table("./train/y_train.txt", header=FALSE)
head(xTrain)
head(yTrain)
## assign column names to the data above
colnames(activityLabel) <- c("activityId", "activityType")
colnames(subjectTrain) <- "subId"
colnames(xTrain) <- features[,2]
colnames(yTrain) <- "activityId"

# merge training data
trainData <- cbind(yTrain, subjectTrain, xTrain)
head(trainData)

# read the test data
subjectTest <- read.table("./test/subject_test.txt", header=FALSE)
xTest <- read.table("./test/X_test.txt", header=FALSE)
yTest <- read.table("./test/y_test.txt", header=FALSE)

# assign column names
colnames(subjectTest) <- "subId"
colnames(xTest) <- features[,2]
colnames(yTest) <- "activityId"

testData <- cbind(yTest, subjectTest, xTest)

# initial merged data
finalData <- rbind(trainData, testData)

# create a vector for column names to be used further
colNames <- colnames(finalData)


#2. Extract only the measurements on the mean and standard deviation for each measurement.

data_mean_std <- finalData[,grepl("mean|std|sub|activityId", colnames(finalData))]


#3. Use descriptive activity names to name the activities in the data set.
library(plyr)
head(activityLabel)
data_mean_std <- join(data_mean_std, activityLabel, by = "activityId", match = "first")
data_mean_std <- data_mean_std[,-1]

#4. Appropriately label the data set with descriptive variable names.
head(data_mean_std)
# remove parentheses
names(data_mean_std) <- gsub("\\(|\\)", "", names(data_mean_std), perl = TRUE)

# correct syntax in names
names(data_mean_std) <- make.names(names(data_mean_std))

# add descriptive names
names(data_mean_std) <- gsub("Acc", "Acceleration", names(data_mean_std))
names(data_mean_std) <- gsub("^t", "Time", names(data_mean_std))
names(data_mean_std) <- gsub("^f", "Frequency", names(data_mean_std))
names(data_mean_std) <- gsub("BodyBody", "Body", names(data_mean_std))
names(data_mean_std) <- gsub("mean", "Mean", names(data_mean_std))
names(data_mean_std) <- gsub("std", "Std", names(data_mean_std))
names(data_mean_std) <- gsub("Freq", "Frequency", names(data_mean_std))
names(data_mean_std) <- gsub("Mag", "Magnitude", names(data_mean_std))

#5. From the data set in step 4, create a second, independent tidy data set with the average of each variable for each activity and each subject.
tidydata_average_sub <- ddply(data_mean_std, c("subId", "activityType"), numcolwise(mean))

write.table(tidydata_average_sub, file="tidydata.txt", row.name = FALSE)


