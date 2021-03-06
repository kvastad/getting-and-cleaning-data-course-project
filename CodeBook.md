# CodeBook

## Variables

### Individuals and Activities variables
**"ind_id"** number corresponding to a unique individual.

**"activity_name"** character corresponding to performed activity.
Including the following activities: 

"WALKING"
"WALKING_UPSTAIRS"
"WALKING_DOWNSTAIRS"
"SITTING"
"STANDING"
"LAYING"


### Measurement variables
The features selected for this database come from the accelerometer and gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ.
These signals were used to estimate variables of the feature vector for each pattern:  
'-XYZ' is used to denote 3-axial signals in the X, Y and Z directions.
The variables used here come from the following source: [1]. 
From this [1] source only a subset was used, where the characters mean or std was included in the variable name, see below.

**Units of the summarized second data set**: all units are numeric and names with prefix 't' to denote time, prefix 'f' to indicate frequency domain signals and prefix 'angle' to indicate degrees between to vectors.

"tBodyAcc.mean...X"
"tBodyAcc.mean...Y"
"tBodyAcc.mean...Z"
"tBodyAcc.std...X"
"tBodyAcc.std...Y"
"tBodyAcc.std...Z"
"tGravityAcc.mean...X"
"tGravityAcc.mean...Y"
"tGravityAcc.mean...Z"
"tGravityAcc.std...X"
"tGravityAcc.std...Y"
"tGravityAcc.std...Z"
"tBodyAccJerk.mean...X"
"tBodyAccJerk.mean...Y"
"tBodyAccJerk.mean...Z"
"tBodyAccJerk.std...X"
"tBodyAccJerk.std...Y"
"tBodyAccJerk.std...Z"
"tBodyGyro.mean...X"
"tBodyGyro.mean...Y"
"tBodyGyro.mean...Z"
"tBodyGyro.std...X"
"tBodyGyro.std...Y"
"tBodyGyro.std...Z"
"tBodyGyroJerk.mean...X"
"tBodyGyroJerk.mean...Y"
"tBodyGyroJerk.mean...Z"
"tBodyGyroJerk.std...X"
"tBodyGyroJerk.std...Y"
"tBodyGyroJerk.std...Z"
"tBodyAccMag.mean.."
"tBodyAccMag.std.."
"tGravityAccMag.mean.."
"tGravityAccMag.std.."
"tBodyAccJerkMag.mean.."
"tBodyAccJerkMag.std.."
"tBodyGyroMag.mean.."
"tBodyGyroMag.std.."
"tBodyGyroJerkMag.mean.."
"tBodyGyroJerkMag.std.."
"fBodyAcc.mean...X"
"fBodyAcc.mean...Y"
"fBodyAcc.mean...Z"
"fBodyAcc.std...X"
"fBodyAcc.std...Y"
"fBodyAcc.std...Z"
"fBodyAcc.meanFreq...X"
"fBodyAcc.meanFreq...Y"
"fBodyAcc.meanFreq...Z"
"fBodyAccJerk.mean...X"
"fBodyAccJerk.mean...Y"
"fBodyAccJerk.mean...Z"
"fBodyAccJerk.std...X"
"fBodyAccJerk.std...Y"
"fBodyAccJerk.std...Z"
"fBodyAccJerk.meanFreq...X"
"fBodyAccJerk.meanFreq...Y"
"fBodyAccJerk.meanFreq...Z"
"fBodyGyro.mean...X"
"fBodyGyro.mean...Y"
"fBodyGyro.mean...Z"
"fBodyGyro.std...X"
"fBodyGyro.std...Y"
"fBodyGyro.std...Z"
"fBodyGyro.meanFreq...X"
"fBodyGyro.meanFreq...Y"
"fBodyGyro.meanFreq...Z"
"fBodyAccMag.mean.."
"fBodyAccMag.std.."
"fBodyAccMag.meanFreq.."
"fBodyBodyAccJerkMag.mean.."
"fBodyBodyAccJerkMag.std.."
"fBodyBodyAccJerkMag.meanFreq.."
"fBodyBodyGyroMag.mean.."
"fBodyBodyGyroMag.std.."
"fBodyBodyGyroMag.meanFreq.."
"fBodyBodyGyroJerkMag.mean.."
"fBodyBodyGyroJerkMag.std.."
"fBodyBodyGyroJerkMag.meanFreq.."
"angle.tBodyAccMean.gravity."
"angle.tBodyAccJerkMean..gravityMean."
"angle.tBodyGyroMean.gravityMean."
"angle.tBodyGyroJerkMean.gravityMean."
"angle.X.gravityMean."
"angle.Y.gravityMean."
"angle.Z.gravityMean."


## Transformations
In table dat4.txt produced by the script all variables from [1] were filtered based on *unique individual* and *performed activity*.

**Variables and summaries calculated** The average from each *measurement variable*, where the characters *mean* or *std* was included in the variable name is summarized in table dat4.txt.

**Relevant information before running script** Before you run the run_analysis.R script, first set working directory to UCI HAR Dataset[1] and then put the run_analysis.R script within the UCI HAR Dataset directory.
To run the run_analysis.R script the dplyr R package needs to be installed.

## Source
[1] Davide Anguita, Alessandro Ghio, Luca Oneto, Xavier Parra and Jorge L. Reyes-Ortiz. Human Activity Recognition on Smartphones using a Multiclass Hardware-Friendly Support Vector Machine. International Workshop of Ambient Assisted Living (IWAAL 2012). Vitoria-Gasteiz, Spain. Dec 2012
