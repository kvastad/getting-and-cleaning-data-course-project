# Instructions for the script:
# 
#	1.	Merges the training and the test sets to create one data set.
#	2.	Extracts only the measurements on the mean and standard deviation for each measurement. 
#	3.	Uses descriptive activity names to name the activities in the data set
#	4.	Appropriately labels the data set with descriptive variable names. 
#	5.	From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

# OBS!!! It will be indicated in the notes below when each POINT for the script is performed/accomplished.
# -----------------------------------------------------------------------

# Do the following before you run the run_analysis.R script.
# Set working directory to UCI HAR Dataset and put the run_analysis script within this directory.

# -----------------------------------------------------------------------

# Loading libraries:
library(dplyr)


# Loading the traning and test data with corresponding lables, store them as data.frames.
# set = 561 varibles/features, we are only interested in mean() and std() variables/features.
# lab = 6 different activites in the data set.
# features = names for all the 561 different varibles/features.
# activity_labels = names for all the 6 different activities.
tra_set <- read.table("train/X_train.txt")
tra_lab <- read.table("train/y_train.txt")
tes_set <- read.table("test/X_test.txt")
tes_lab <- read.table("test/y_test.txt")
features <- read.table("features.txt")
activity_labels <- read.table("activity_labels.txt")

# Re-naming the lab data.frames.[POINT 4]
colnames(tra_lab) <-c("activity_name")
colnames(tes_lab) <-c("activity_name")

# Merging lab and set data.frames, for tra and tes data.frames respetivly.
tra <- bind_cols(tra_lab, tra_set)
tes <- bind_cols(tes_lab, tes_set)

# Merging the tra and tes data.frames to create one data set. [POINT 1]
dat <- bind_rows(tra, tes)

# Renaming the data.frame with unique column names. [POINT 4]
f_names <- as.character(features[,2])
colnames(dat) <- make.names(c("activity_name", f_names), unique = TRUE)

# Extracting only columns with activity_name or mean or std for each measurement. [POINT 2].
dat2 <- select(dat, matches("activity_name|mean|std"))

# Renaming the activities, from numbers to names. [POINT 3]
a_names <- as.character(activity_labels[,2])
for (i in 1:length(a_names)) {
	dat2$activity_name[dat2$activity_name == i] <- a_names[i]
}

# Loading the traning and test data where each row correspondes to the individual whom performed the activity, store as data.frames.
tra_id <- read.table("train/subject_train.txt")
tes_id <- read.table("test/subject_test.txt")

# Merging the traning and test data sets.
trates_id <- bind_rows(tra_id, tes_id)

# Renaming the trates_id colum.
colnames(trates_id) <- make.names(c("ind_id"), unique=TRUE)

# Adding the trates_id data.frame to the dat2 data.frame.
dat3 <- bind_cols(trates_id,dat2)

# New data set dat4 containing the average of each variable for each activity and each subject from dat3. [POINT 5]
dat4 <- data.frame()

# A nested for loop, first loop filters on each individual, second loop filters on each activity for each individual.
for (n in 1:30){
ind_n <- filter(dat3, ind_id == n) # filter on each individual
	for (a in 1:length(a_names)){
		ind_a <- filter(ind_n, activity_name == a_names[a]) # filter on each activity within each individual.
		dat_ia <- ind_a[1,1:2] # save the values for ind_id and activity_name.
		dat_sm <- as.matrix(ind_a[,3:88]) # saves the rest of ind_a as a matrix.
		dat_s <- apply(dat_sm,2,mean) # calculate the mean for each of the column in the matrix, returns a numeric vector.
		dat_sdf <- as.data.frame(dat_s) # convert numeric vector to data.frame
		ts <- as.data.frame(t(dat_sdf)) # transpose, t(), the data.frame so that rows becomes columns and columns becomes rows.
		dat_iats <- bind_cols(dat_ia, ts) 
		dat4 <- bind_rows(dat4, dat_iats) # Store the final data in the data.frame dat4.
		
	}

}






