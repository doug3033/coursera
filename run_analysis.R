# use plyr
library(plyr)

# set the working directory
setwd("C:\\Users\\Doug2\\Documents\\coursera\\dataCleaning\\assignment\\UCI HAR Dataset\\")
# set directories
train.init.dir <- "train\\Inertial Signals\\"
test.init.dir <- "test\\Inertial Signals\\"
train.dir <- "train\\"
test.dir <- "test\\"

# name the files
activity.labels.file <- paste(main.dir, "activity_labels.txt", sep="")
features.file <- paste(main.dir, "features.txt", sep="")
subject.train.file <- paste(train.dir, "subject_train.txt", sep="")
X.train.file <- paste(train.dir, "X_train.txt", sep="")
y.train.file <- paste(train.dir, "y_train.txt", sep="")
subject.test.file <- paste(test.dir, "subject_test.txt", sep="")
X.test.file <- paste(test.dir, "X_test.txt", sep="")
y.test.file <- paste(test.dir, "y_test.txt", sep="")
body.acc.x.train.file <- paste(train.init.dir, "body_acc_x_train.txt", sep="")
body.acc.y.train.file <- paste(train.init.dir, "body_acc_y_train.txt", sep="")
body.acc.z.train.file <- paste(train.init.dir, "body_acc_z_train.txt", sep="")
body.gyro.x.train.file <- paste(train.init.dir, "body_gyro_x_train.txt", sep="")
body.gyro.y.train.file <- paste(train.init.dir, "body_gyro_y_train.txt", sep="")
body.gyro.z.train.file <- paste(train.init.dir, "body_gyro_z_train.txt", sep="")
total.acc.x.train.file <- paste(train.init.dir, "total_acc_x_train.txt", sep="")
total.acc.y.train.file <- paste(train.init.dir, "total_acc_y_train.txt", sep="")
total.acc.z.train.file <- paste(train.init.dir, "total_acc_z_train.txt", sep="")
body.acc.x.test.file <- paste(test.init.dir, "body_acc_x_test.txt", sep="")
body.acc.y.test.file <- paste(test.init.dir, "body_acc_y_test.txt", sep="")
body.acc.z.test.file <- paste(test.init.dir, "body_acc_z_test.txt", sep="")
body.gyro.x.test.file <- paste(test.init.dir, "body_gyro_x_test.txt", sep="")
body.gyro.y.test.file <- paste(test.init.dir, "body_gyro_y_test.txt", sep="")
body.gyro.z.test.file <- paste(test.init.dir, "body_gyro_z_test.txt", sep="")
total.acc.x.test.file <- paste(test.init.dir, "total_acc_x_test.txt", sep="")
total.acc.y.test.file <- paste(test.init.dir, "total_acc_y_test.txt", sep="")
total.acc.z.test.file <- paste(test.init.dir, "total_acc_z_test.txt", sep="")

# read the data in
activity.labels <- read.table(activity.labels.file, header=FALSE)
features <- read.table(features.file, header=FALSE)
subject.train <- read.table(subject.train.file, header=FALSE)
X.train <- read.table(X.train.file, header=FALSE)
y.train <- read.table(y.train.file, header=FALSE)
subject.test <- read.table(subject.test.file, header=FALSE)
X.test <- read.table(X.test.file, header=FALSE)
y.test <- read.table(y.test.file, header=FALSE)
body.acc.x.train <- read.table(body.acc.x.train.file, header=FALSE)
body.acc.y.train <- read.table(body.acc.y.train.file, header=FALSE)
body.acc.z.train <- read.table(body.acc.z.train.file, header=FALSE)
body.gyro.x.train <- read.table(body.gyro.x.train.file, header=FALSE)
body.gyro.y.train <- read.table(body.gyro.y.train.file, header=FALSE)
body.gyro.z.train <- read.table(body.gyro.z.train.file, header=FALSE)
total.acc.x.train <- read.table(total.acc.x.train.file, header=FALSE)
total.acc.y.train <- read.table(total.acc.y.train.file, header=FALSE)
total.acc.z.train <- read.table(total.acc.z.train.file, header=FALSE)
body.acc.x.test <- read.table(body.acc.x.test.file, header=FALSE)
body.acc.y.test <- read.table(body.acc.y.test.file, header=FALSE)
body.acc.z.test <- read.table(body.acc.z.test.file, header=FALSE)
body.gyro.x.test <- read.table(body.gyro.x.test.file, header=FALSE)
body.gyro.y.test <- read.table(body.gyro.y.test.file, header=FALSE)
body.gyro.z.test <- read.table(body.gyro.z.test.file, header=FALSE)
total.acc.x.test <- read.table(total.acc.x.test.file, header=FALSE)
total.acc.y.test <- read.table(total.acc.y.test.file, header=FALSE)
total.acc.z.test <- read.table(total.acc.z.test.file, header=FALSE)

# add a "type" column to X.train and X.test to identify each row as train or test
X.train$type <- "train"
X.test$type <- "test"

# Part 1 Merges the training and the test sets to create one data set.
X.common <- rbind(X.train, X.test)
subject.common <- rbind(subject.train, subject.test)
y.common <- rbind(y.train, y.test)

# Part 2 Extracts only the measurements on the mean and standard deviation for each measurement. 
# modify the feature names to replace the - and ()
features.proc <- gsub("-", ".", features$V2)
features.proc <- gsub("\\(", "", features.proc)
features.proc <- gsub("\\)", "", features.proc)
names(X.common) <- features.proc
# select the mean and std columns
X.common.means <- subset(X.common, select=(names(X.common)[grep('*[Mm]ean*',names(X.common))]))
X.common.std <- subset(X.common, select=(names(X.common)[grep('*[Ss]td*',names(X.common))]))
X.common.select <- cbind(X.common.means, X.common.std)


# Part 3 Uses descriptive activity names to name the activities in the data set
y.common.labeled<-merge(y.common,activity.labels,by.x="V1", by.y="V1",all=FALSE)
names(y.common.labeled) <- c("activity", "activity.label")

# Part 4 Appropriately labels the data set with descriptive activity names. 
X.common.select <- cbind(y.common.labeled, X.common.select)
names(subject.common) <- c("subject")
X.common.select <- cbind(subject.common, X.common.select)

# Part 5 Creates a second, independent tidy data set with the average of each variable for each activity and each subject.
tidy.df <- ddply(X.common.select, .(subject,activity.label), summarise, tBodyAcc.mean.X = mean(tBodyAcc.mean.X, na.rm=TRUE),
                 time.Body.Acceleration.mean.Y = mean(tBodyAcc.mean.Y, na.rm=TRUE),
                 time.Body.Acceleration.mean.Z = mean(tBodyAcc.mean.Z, na.rm=TRUE),
                 time.Gravity.Acceleration.mean.X = mean(tGravityAcc.mean.X, na.rm=TRUE),
                 time.Gravity.Acceleration.mean.Y = mean(tGravityAcc.mean.Y, na.rm=TRUE),
                 time.Gravity.Acceleration.mean.Z = mean(tGravityAcc.mean.Z, na.rm=TRUE),
                 time.Body.Acceleration.Jerk.mean.X = mean(tBodyAccJerk.mean.X, na.rm=TRUE),
                 time.Body.Acceleration.Jerk.mean.Y = mean(tBodyAccJerk.mean.Y, na.rm=TRUE),
                 time.Body.Acceleration.Jerk.mean.Z = mean(tBodyAccJerk.mean.Z, na.rm=TRUE),
                 time.Body.Gyro.mean.X = mean(tBodyGyro.mean.X, na.rm=TRUE),
                 time.Body.Gyro.mean.Y = mean(tBodyGyro.mean.Y, na.rm=TRUE),
                 time.Body.Gyro.mean.Z = mean(tBodyGyro.mean.Z, na.rm=TRUE),
                 time.Body.Gyro.Jerk.mean.X = mean(tBodyGyroJerk.mean.X, na.rm=TRUE),
                 time.Body.Gyro.Jerk.mean.Y = mean(tBodyGyroJerk.mean.Y, na.rm=TRUE),
                 time.Body.Gyro.Jerk.mean.Z = mean(tBodyGyroJerk.mean.Z, na.rm=TRUE),
                 time.Body.Acceleration.Mag.mean = mean(tBodyAccMag.mean, na.rm=TRUE),
                 time.Gravity.Acceleration.Mag.mean = mean(tGravityAccMag.mean, na.rm=TRUE),
                 time.Body.Acceleration.Jerk.Mag.mean = mean(tBodyAccJerkMag.mean, na.rm=TRUE),
                 time.Body.Gyro.Mag.mean = mean(tBodyGyroMag.mean, na.rm=TRUE),
                 time.Body.Gyro.Jerk.Mag.mean = mean(tBodyGyroJerkMag.mean, na.rm=TRUE),
                 frequency.Body.Acceleration.mean.X = mean(fBodyAcc.mean.X, na.rm=TRUE),
                 frequency.Body.Acceleration.mean.Y = mean(fBodyAcc.mean.Y, na.rm=TRUE),
                 frequency.Body.Acceleration.mean.Z = mean(fBodyAcc.mean.Z, na.rm=TRUE),
                 frequency.Body.Acceleration.mean.Freq.X = mean(fBodyAcc.meanFreq.X, na.rm=TRUE),
                 frequency.Body.Acceleration.mean.Freq.Y = mean(fBodyAcc.meanFreq.Y, na.rm=TRUE),
                 frequency.Body.Acceleration.mean.Freq.Z = mean(fBodyAcc.meanFreq.Z, na.rm=TRUE),
                 frequency.Body.Acceleration.Jerk.mean.X = mean(fBodyAccJerk.mean.X, na.rm=TRUE),
                 frequency.Body.Acceleration.Jerk.mean.Y = mean(fBodyAccJerk.mean.Y, na.rm=TRUE),
                 frequency.Body.Acceleration.Jerk.mean.Z = mean(fBodyAccJerk.mean.Z, na.rm=TRUE),
                 frequency.Body.Acceleration.Jerk.mean.Freq.X = mean(fBodyAccJerk.meanFreq.X, na.rm=TRUE),
                 frequency.Body.Acceleration.Jerk.mean.Freq.Y = mean(fBodyAccJerk.meanFreq.Y, na.rm=TRUE),
                 frequency.Body.Acceleration.Jerk.mean.Freq.Z = mean(fBodyAccJerk.meanFreq.Z, na.rm=TRUE),
                 frequency.Body.Gyro.mean.X = mean(fBodyGyro.mean.X, na.rm=TRUE),
                 frequency.Body.Gyro.mean.Y = mean(fBodyGyro.mean.Y, na.rm=TRUE),
                 frequency.Body.Gyro.mean.Z = mean(fBodyGyro.mean.Z, na.rm=TRUE),
                 frequency.Body.Gyro.meanFreq.X = mean(fBodyGyro.meanFreq.X, na.rm=TRUE),
                 frequency.Body.Gyro.meanFreq.Y = mean(fBodyGyro.meanFreq.Y, na.rm=TRUE),
                 frequency.Body.Gyro.meanFreq.Z = mean(fBodyGyro.meanFreq.Z, na.rm=TRUE),
                 frequency.Body.Acceleration.Mag.mean = mean(fBodyAccMag.mean, na.rm=TRUE),
                 frequency.Body.Acceleration.Mag.mean.Freq = mean(fBodyAccMag.meanFreq, na.rm=TRUE),
                 frequency.Body.Body.Acceleration.Jerk.Mag.mean = mean(fBodyBodyAccJerkMag.mean, na.rm=TRUE),
                 frequency.Body.Body.Acceleration.Jerk.Mag.mean.Freq = mean(fBodyBodyAccJerkMag.meanFreq, na.rm=TRUE),
                 frequency.Body.Body.Gyro.Mag.mean = mean(fBodyBodyGyroMag.mean, na.rm=TRUE),
                 frequency.Body.Body.Gyro.Mag.mean.Freq = mean(fBodyBodyGyroMag.meanFreq, na.rm=TRUE),
                 frequency.Body.Body.Gyro.Jerk.Mag.mean = mean(fBodyBodyGyroJerkMag.mean, na.rm=TRUE),
                 frequency.Body.Body.Gyro.Jerk.Mag.mean.Freq = mean(fBodyBodyGyroJerkMag.meanFreq, na.rm=TRUE),
                 time.Body.Acceleration.std.X = mean(tBodyAcc.std.X, na.rm=TRUE),
                 time.Body.Acceleration.std.Y = mean(tBodyAcc.std.Y, na.rm=TRUE),
                 time.Body.Acceleration.std.Z = mean(tBodyAcc.std.Z, na.rm=TRUE),
                 time.Gravity.Acceleration.std.X = mean(tGravityAcc.std.X, na.rm=TRUE),
                 time.Gravity.Acceleration.std.Y = mean(tGravityAcc.std.Y, na.rm=TRUE),
                 time.Gravity.Acceleration.std.Z = mean(tGravityAcc.std.Z, na.rm=TRUE),
                 time.Body.Acceleration.Jerk.std.X = mean(tBodyAccJerk.std.X, na.rm=TRUE),
                 time.Body.Acceleration.Jerk.std.Y = mean(tBodyAccJerk.std.Y, na.rm=TRUE),
                 time.Body.Acceleration.Jerk.std.Z = mean(tBodyAccJerk.std.Z, na.rm=TRUE),
                 time.Body.Gyro.std.X = mean(tBodyGyro.std.X, na.rm=TRUE),
                 time.Body.Gyro.std.Y = mean(tBodyGyro.std.Y, na.rm=TRUE),
                 time.Body.Gyro.std.Z = mean(tBodyGyro.std.Z, na.rm=TRUE),
                 time.Body.Gyro.Jerk.std.X = mean(tBodyGyroJerk.std.X, na.rm=TRUE),
                 time.Body.Gyro.Jerk.std.Y = mean(tBodyGyroJerk.std.Y, na.rm=TRUE),
                 time.Body.Gyro.Jerk.std.Z = mean(tBodyGyroJerk.std.Z, na.rm=TRUE),
                 time.Body.Acceleration.Mag.std = mean(tBodyAccMag.std, na.rm=TRUE),
                 time.Gravity.Acceleration.Mag.std = mean(tGravityAccMag.std, na.rm=TRUE),
                 time.Body.Acceleration.Jerk.Mag.std = mean(tBodyAccJerkMag.std, na.rm=TRUE),
                 time.Body.Gyro.Mag.std = mean(tBodyGyroMag.std, na.rm=TRUE),
                 time.Body.Gyro.Jerk.Mag.std = mean(tBodyGyroJerkMag.std, na.rm=TRUE),
                 frequency.Body.Acceleration.std.X = mean(fBodyAcc.std.X, na.rm=TRUE),
                 frequency.Body.Acceleration.std.Y = mean(fBodyAcc.std.Y, na.rm=TRUE),
                 frequency.Body.Acceleration.std.Z = mean(fBodyAcc.std.Z, na.rm=TRUE),
                 frequency.Body.Acceleration.Jerk.std.X = mean(fBodyAccJerk.std.X, na.rm=TRUE),
                 frequency.Body.Acceleration.Jerk.std.Y = mean(fBodyAccJerk.std.Y, na.rm=TRUE),
                 frequency.Body.Acceleration.Jerk.std.Z = mean(fBodyAccJerk.std.Z, na.rm=TRUE),
                 frequency.Body.Gyro.std.X = mean(fBodyGyro.std.X, na.rm=TRUE),
                 frequency.Body.Gyro.std.Y = mean(fBodyGyro.std.Y, na.rm=TRUE),
                 frequency.Body.Gyro.std.Z = mean(fBodyGyro.std.Z, na.rm=TRUE),
                 frequency.Body.Acceleration.Mag.std = mean(fBodyAccMag.std, na.rm=TRUE),
                 frequency.Body.Body.Acceleration.Jerk.Mag.std = mean(fBodyBodyAccJerkMag.std, na.rm=TRUE),
                 frequency.Body.Body.Gyro.Mag.std = mean(fBodyBodyGyroMag.std, na.rm=TRUE),
                 frequency.Body.Body.Gyro.Jerk.Mag.std = mean(fBodyBodyGyroJerkMag.std, na.rm=TRUE)        
                 )
write.table(tidy.df, "wearable_fitness_test_tidy.txt", sep=' ', quote=FALSE)


