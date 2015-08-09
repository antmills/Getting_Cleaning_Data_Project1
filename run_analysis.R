# loadData(type) - load data and merge ydata and measuerment values for test
# or train - 'type': indicating test or train data

loadData <- function(type) {
        # load data
        folder <- "UCI HAR Dataset/"
        filetype <- ".txt"
        subjects <- read.table(paste(folder, type, "/subject_", type, filetype, 
        sep = ""))
        y <- read.table(paste(folder, type, "/y_", type, filetype, sep = ""))
        featuresName <- read.table(paste(folder, "features", filetype, sep = ""))
        x <- read.table(paste(folder, type, "/X_", type, filetype, sep = ""))
        # Put header of the feature value with features name
        names(x) <- featuresName[, 2]
        # Put header of the activity value and subject with proper name
        colnames(subjects) <- "subject"
        colnames(y) <- "activity"
        # Combine the y ,subject and x together
        df <- cbind(cbind(subjects, y), x)
        return(df)
}
setwd("C:/Users/antmills/Desktop/Coursera")
test <- loadData("test")
train <- loadData("train")
fulData <- rbind(train, test)
# Replace the activity with corrsponding activity name
activityNames <- read.table("UCI HAR Dataset/activity_labels.txt")
fulData$activity <- sapply(fulData$activity, function(x) activityNames[x, 2])
# Keep features that represents for mean and standard deviation (std)
featuresNames <- read.table("UCI HAR Dataset/features.txt")
colnames(featuresNames) <- c("fId", "featurename")
keepFeatures = subset(featuresNames, grepl("mean|std", featurename))[, 2]
preData <- subset(fulData, select = c(cbind("subject", "activity"), as.character(keepFeatures)))
library(reshape)
tidyMelted <- melt(preData, id = c("subject", "activity"))
tidy<-cast(tidyMelted, subject + activity ~ variable, mean) 
dim(tidy)
# See the row and column number
summary(tidy)
write.table(tidy, "tidy.txt", row.name = FALSE)
write.csv(tidy, "tidy.csv")



