install.packages("plyr")
install.packages("dplyr")
library("plyr")
library("dplyr")

# features: list of all features (row: 561)
features <- read.csv('./UCI HAR Dataset/features.txt', header = FALSE, sep = ' ')
features <- as.character(features[,2])
features<-tbl_df(features)

#train_subject:each row identifies the subject (1-30,ttl21) who performed the activity (7352 obs)
#trainx:training set (col: v1-v561) (7352 obs)
#trainy:training labels (val: 1-6) (7352 obs)
trainx <- read.table('./UCI HAR Dataset/train/X_train.txt')
trainy <- read.csv('./UCI HAR Dataset/train/y_train.txt', header = FALSE, sep = ' ')
train_subject <- read.csv('./UCI HAR Dataset/train/subject_train.txt',header = FALSE, sep = ' ')
trainx<-tbl_df(trainx);trainy<-tbl_df(trainy);train_subject<-tbl_df(train_subject)

#join training data and rename the columns
train_data <- data.frame(train_subject,trainy,trainx); train_data<-tbl_df(train_data)
names(train_data) <- c("subject", "label",features)

#repeat same process to join test data
#test_subject: (2,4,9,10,12,13,18,20,24) - 9 subjects
#testy:test labels (val: 1-6) (2947 obs)  ; testx:test set (col: v1-v561) (2947 obs)
testx <- read.table('./UCI HAR Dataset/test/X_test.txt')
testy <- read.csv('./UCI HAR Dataset/test/y_test.txt', header = FALSE, sep = ' ')
test_subject <- read.csv('./UCI HAR Dataset/test/subject_test.txt', header = FALSE, sep = ' ')
testx <- tbl_df(testx);testy <- tbl_df(testy);test_subject <- tbl_df(test_subject)
test_data <-  data.frame(test_subject, testy, testx); test_data<-tbl_df(test_data)
names(test_data) <- c("subject", "label", features)

#Combine train and test data
all_data <- rbind(train_data,test_data);all_data<-tbl_df(all_data)

#Extract sub-data set with only mean and std measurements
colselect <- grep('mean|std', features)
subdata <- all_data[,c(1,2,colselect + 2)]

#replace label (1-6) with description
labelinfo <- read.table('./UCI HAR Dataset/activity_labels.txt', header = FALSE)
labelinfo <- as.character(labelinfo[,2])
subdata$label <- labelinfo[subdata$label]

#replace column names with description
#t: time domain signals ; f: frequency domain signals
 #BodyAcc: Body Acceleration Signal
 #GravityAcc: Gravity Acceleration Signal
 #BodyAccJerk: Body Acceleration jerk Signal
 #BodyGyro: Body Angular Velocity Signal
 #BodyGyroJerk: Body Angular Velocity Jerk Signal
 #BodyAccMag: Body Acceleration Signal Magnitude
 #GravityAccMag: Gravity Acceleration Signal Magnitude
 #BodyAccJerkMag: Body Acceleration Jerk Signal Magnitude
 #BodyGyroMag: Body Angular Velocity Signal Magnitude
 #BodyGyroJerkMag:Body Angular Velocity Jerk Signal Magnitude
newcolnames <- names(subdata) ; newcolnames<-gsub("^t","TimeDomain_",newcolnames) ; newcolnames<-gsub("^f","FrequencyDomain_",newcolnames)
newcolnames <- gsub("Acc","_Acceleration_", newcolnames);newcolnames<-gsub("Gyro","_Angular Velocity_",newcolnames)
newcolnames<-gsub("Mag","Magnitude",newcolnames) ; newcolnames<-gsub("-","_",newcolnames) ; newcolnames<-gsub("mean","Mean_",newcolnames)
newcolnames<-gsub("std","Standard Deviation",newcolnames) ; newcolnames<-gsub("[(][)]","",newcolnames)
names(subdata)<-newcolnames

#Create another tidy data with mean for each subject / each activity label
subdata1<-group_by(subdata, subject,label) 
mean_data <- summarise_all(subdata1,funs(mean))
mean_data <- as.data.fram(mean_data)
write.table(mean_data,file="./clean_mean_data.txt",row.name=FALSE)
