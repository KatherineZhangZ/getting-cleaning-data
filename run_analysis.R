setwd("/Users/katherinezhang/Desktop/Coursera R/UCI HAR Dataset")

#1.Merges the trainning and test sets to create one data set

# readin feature names and activity type 
	feature_nm<- read.table('./features.txt',header=FALSE); 
	activityType<- read.table('./activity_labels.txt',header=FALSE);

# read in train files 
	subjectTrain<- read.table('./train/subject_train.txt',header=FALSE); 
	xTrain<-read.table('./train/x_train.txt',header=FALSE); 
	yTrain<-read.table('./train/y_train.txt',header=FALSE);
	 
# read in test files 
	subjectTest<- read.table('./test/subject_test.txt',header=FALSE); 
	xTest<- read.table('./test/x_test.txt',header=FALSE); 
	yTest<- read.table('./test/y_test.txt',header=FALSE); 

# assignm coloum names to train and test files 
	colnames(activityType)  = c('activityId','activityType');
	colnames(subjectTrain)  = "subjectId";
	colnames(xTrain)        = feature_nm$V2; 
	colnames(yTrain)        = "activityId";

	colnames(subjectTest) = "subjectId";
	colnames(xTest)       = feature_nm$V2; 
	colnames(yTest)       = "activityId";

# column bind subject,y,and x files to create training and test sets 
	trainSet <- cbind(subjectTrain,yTrain,xTrain) 
	testSet <- cbind(subjectTest,yTest,xTest)

# row bind training and test sets to create one full set
	fullSet<-rbind(trainSet,testSet)
	
#2.Extracts only the measurements on the mean and standard deviation of each measurement
	pattern<-"subjectId|activityId|mean\\(\\)|std\\(\\)"  
	meanStdSet<-fullSet[,grep(pattern,names(fullSet),value=TRUE)] 

#3.Uses descriptive activity names to name the activities in the data set
  # merge with acvivityType dataframe to get descriptive activity names
	tidyData1<-merge(activityType,meanStdSet,by='activityId',all=TRUE);	
	tidyData1<-tidyData1[,names(tidyData1)!="activityId"];

#4. Appropriately labels the data set with descriptive variable names
       CleanNm <- gsub("^t","time",names(tidyData1))
       CleanNm <- gsub("Acc","Accelerometer",CleanNm)
       CleanNm <- gsub("^f","frequency",CleanNm)
       CleanNm <- gsub("Gyro","Gyroscope",CleanNm)
       CleanNm <- gsub("Mag","Magnitude",CleanNm)
       CleanNm <- gsub("BodyBody","Body",CleanNm)
       names(tidyData1) <- CleanNm
 
#5. Creates a independent tidy data set with the average of each variable/activity/subject	
	tidyData2<-aggregate(. ~subjectId+activityType, tidyData1, mean) 
	tidyData2<-tidyData2[order(tidyData2$subjectId,tidyData2$activityType),]
	write.table(tidyData2, file="./tidydata.txt", row.name=FALSE)
