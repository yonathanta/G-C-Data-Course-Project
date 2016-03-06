#Step 1. Getting the train and test datasets and merging them.

setwd("C:/R/Online_Coursera/Coursera_Getting_and_Cleaning_Data/peer_assessment")

# Reading the train dataset

trainData <- read.table("./data/train/X_train.txt")
dim(trainData) 
head(trainData)
trainLabel <- read.table("./data/train/y_train.txt")
table(trainLabel)
trainSubject <- read.table("./data/train/subject_train.txt")

#Reading the test dataset

testData <- read.table("./data/test/X_test.txt")
dim(testData) 
testLabel <- read.table("./data/test/y_test.txt") 
table(testLabel) 
testSubject <- read.table("./data/test/subject_test.txt")

# Merging the train and test datasets

joinData <- rbind(trainData, testData)
dim(joinData) 
joinLabel <- rbind(trainLabel, testLabel)
dim(joinLabel) 
joinSubject <- rbind(trainSubject, testSubject)
dim(joinSubject) 


# Step2. Extracts variable with only the measurements of the mean and standard deviation.

features <- read.table("./data/features.txt")
dim(features)  
meanStdIndices <- grep("mean\\(\\)|std\\(\\)", features[, 2])
length(meanStdIndices) 
joinData <- joinData[, meanStdIndices]
dim(joinData) 
names(joinData) <- gsub("\\(\\)", "", features[meanStdIndices, 2]) # remove "()"
names(joinData) <- gsub("mean", "Mean", names(joinData)) # capitalize M
names(joinData) <- gsub("std", "Std", names(joinData)) # capitalize S
names(joinData) <- gsub("-", "", names(joinData)) # remove "-" in column names 


# Step3. Uses descriptive activity names to name the activities in 
# the data set
activity <- read.table("./data/activity_labels.txt")
activity[, 2] <- tolower(gsub("_", "", activity[, 2]))
substr(activity[2, 2], 8, 8) <- toupper(substr(activity[2, 2], 8, 8))
substr(activity[3, 2], 8, 8) <- toupper(substr(activity[3, 2], 8, 8))
activityLabel <- activity[joinLabel[, 1], 2]
joinLabel[, 1] <- activityLabel
names(joinLabel) <- "activity"


# Step4. Appropriately labels the data set with descriptive activity 
# names. 
names(joinSubject) <- "subject"
cleanedData <- cbind(joinSubject, joinLabel, joinData)
dim(cleanedData) 
write.table(cleanedData, "merged_data.txt") # write out the 1st dataset


# Step5. Creates a second, independent tidy data set with the average of 
# each variable for each activity and each subject. 
subjectLen <- length(table(joinSubject))
activityLen <- dim(activity)[1] 
columnLen <- dim(cleanedData)[2]
result <- matrix(NA, nrow=subjectLen*activityLen, ncol=columnLen) 
result <- as.data.frame(result)
colnames(result) <- colnames(cleanedData)
row <- 1
for(x in 1:subjectLen) {
  for(y in 1:activityLen) {
    result[row, 1] <- sort(unique(joinSubject)[, 1])[x]
    result[row, 2] <- activity[y, 2]
    bool1 <- x == cleanedData$subject
    bool2 <- activity[y, 2] == cleanedData$activity
    result[row, 3:columnLen] <- colMeans(cleanedData[bool1&bool2, 3:columnLen])
    row <- row + 1
  }
}

head(result)
write.table(result, "tidy_data_with_averages.txt") # write out the 2nd dataset



