##############################################################################################
# Prepare the 'test' data, using the following steps:
# 1. Read the 'test' data from the file "X_test.txt"
# 2. Read the 'subject' values from the file "subject_test.txt"
# 3. Read the 'activity' values from the file "y_test.txt"
# 5. Add the 'subject' and 'activity code' data to the main data, as columns 1 and 2
##############################################################################################
dataFileName <- paste("test", "X_test.txt", collapse=NULL, sep="\\")
testFileData <- read.table(dataFileName)
subjectFileName <- paste("test","subject_test.txt", collapse=NULL, sep="\\")
subjectData <- read.table(subjectFileName)
activityFileName <- paste("test","y_test.txt", collapse=NULL, sep="\\")
activityDFrame <- read.table(activityFileName)
testFileData2 <- cbind(activityDFrame, testFileData)          #add activity column to main data
testFileData3 <- cbind(subjectData, testFileData2)            #add subject column to main data

###############################################################################################
# Perform the same steps [steps 1-4] with the 'train' dataset
###############################################################################################
trainFileData <- NULL
subjectFileData <- NULL
activityDFrame <- NULL
dataFileName <- paste("train", "X_train.txt", collapse=NULL, sep="\\")
trainFileData <- read.table(dataFileName)
subjectFileName <- paste("train","subject_train.txt", collapse=NULL, sep="\\")
subjectData <- read.table(subjectFileName)
activityFileName <- paste("train","y_train.txt", collapse=NULL, sep="\\")
activityDFrame <- read.table(activityFileName)
trainFileData2 <- cbind(activityDFrame, trainFileData)          #add activity columns to main data
trainFileData3 <- cbind(subjectData, trainFileData2)            #add subject column to main data

###############################################################################################
# Combine the 'test' and 'train' datasets and create one consolidated data-set
# Then read the 'features.txt' file, get the column names, and apply to the consolidated df
###############################################################################################
combinedData <- rbind(testFileData3, trainFileData3)            #combine the two data-sets

fileName <- "features.txt"
featuresDF <- read.table(fileName)
featureNames <- as.vector(featuresDF[,2])                       
featureNames2 <- c("Subject", "Activity_Code", featureNames)
colnames(combinedData) <- featureNames2

###############################################################################################
# Create a new data frame with just the required columns, ie, the mean and std. dev columns:
# 1. get indices of col names matching patterns "mean()" and "std()"
# 2. add indices 1,2 and 3 to the above, for the cols pertaining to subject and activity
# 3. subset the DF based on the above indices
###############################################################################################
featureNamesFinal <- names(combinedData)
stdIndices <- grep("std()", featureNamesFinal, value=FALSE, fixed=TRUE)
meanIndices <- grep("mean()", featureNamesFinal, value=FALSE, fixed=TRUE)
finalColIndices <- c(1,2, stdIndices, meanIndices)
finalDF <- combinedData[, finalColIndices]

###############################################################################################
# Group the data in finalDF by Subject and by Activity_Code, and determine the mean values
# for the group.  The resultant data set will have 180 rows and 68 columns.  Sort this 
# data set by Subject and Activity_Code
###############################################################################################
library(data.table)
finalDT <- as.data.table(finalDF)
groupCondition <- c("Subject", "Activity_Code")
sdCols <- names(finalDT)[4:69]
outDT <- finalDT[, lapply(.SD,mean),by=groupCondition]
sortedOutDT <- outDT[order(Subject, Activity_Code)]
sortedOutDF <- as.data.frame(sortedOutDT)

###############################################################################################
# Add a column for Activity descriptions, based on the activity codes in column 2.
# The Activity_Name column is added as the third column, for better readability
###############################################################################################
activityDescr <- NULL
for (i in 1:nrow(sortedOutDF))                               # set explicit 'activity name'
{
  caseVar <- as.character(sortedOutDF[i,2])
  switch (caseVar,
          "1" = Activity_Name[i] <- "Walking",
          "2" = Activity_Name[i] <- "Walking_Upstairs",
          "3" = Activity_Name[i] <- "Walking_Downstairs",
          "4" = Activity_Name[i] <- "Sitting",
          "5" = Activity_Name[i] <- "Standing",
          "6" = Activity_Name[i] <- "Laying",
  )
}

finalOutDF <- cbind(sortedOutDF[,1:2], Activity_Name, sortedOutDF[, 4:68])

###############################################################################################
# write the final output to a disk file
###############################################################################################
write.table(finalOutDF, "finalOutput.txt",row.names = FALSE,append=FALSE, sep=" ")




