library(reshape2)


#
# Step 1. Merge the training and the test sets to create one data set.
#

# Check if data directory exists, create one if needed
if (!file.exists("data")) {
  dir.create("data")
}

# Download the dataset and unzip the files
fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileURL, destfile="./data/dataset.zip")
unzip("./data/dataset.zip", exdir="./data")

# List the directories 'test' and 'train'
dirs <- list.dirs(path="./data/UCI HAR Dataset", full.names=TRUE, recursive=FALSE)

# Merge the data files in the test and train directories
data <- do.call("rbind", lapply(dirs, function(d) {
  # List data files in directories
  files <- sapply(d, list.files, pattern=".txt", full.names=TRUE)
  # Create a data set by binding file contents as columns
  do.call("cbind", lapply(files, function(f) { read.table(f, header=FALSE) }))
}))


#
# Step 2. Label the columns in the data set 
#

# The first and last column are 'subject' and 'activity' respectively, due to
# the ordering of the files in the data directories
names(data)[c(1,563)] <- c("subject","activity")

# The columns in between are the observations that can be labeled using the
# names in the file 'features.txt'
names(data)[2:562] <- as.character(read.table("./data/UCI HAR Dataset/features.txt", sep=" ")[,2])


# 
# Step 3. Extract only the measurements on the mean and standard deviation for 
# each measurement.
# 

# Get the indexes for 'std()' and 'mean()' measurements
idx <- grep("(std|mean)\\(", names(data))

# Filter the columns of the dataset to include 'subject', 'activity' and the
# 'std()' and 'mean()' measurements only
tidy <- data[c(1,563,idx)]


# 
# Step 4. Set descriptive activity names to name the activities in the data set
# 

# Read the activities from 'activity_labels.txt'
lbl <- read.table("./data/UCI HAR Dataset/activity_labels.txt", sep=" ")

# Replace the numerical values in 'activity' with descriptive labels
tidy$activity <- factor(tidy$activity, levels=lbl[,1], labels=lbl[,2])


# 
# Step 5. Create an independent tidy data set with the average of each variable 
# for each activity and each subject.
# 

# Melt tidy data in long format
tdl <- melt(tidy, id.vars=c("activity","subject"), measure.vars=3:66)

# Summarize
out <- dcast(tdl, activity + subject ~ variable, mean)

# Write output to file
write.table(out, "tidydata.txt", sep=",", row.names=FALSE)

