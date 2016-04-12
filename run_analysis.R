##This R script does following in sequence: merges two datasets, obtain mean and sd for individual measurement,
##makes activity & label names descriptive and finally creates new clean dataset.

#1) Merges the training and the test sets to create one data set.
#2) Extracts only the measurements on the mean and standard deviation for each measurement. 
#3) Uses descriptive activity names to name the activities in the data set
#4) Appropriately labels the data set with descriptive variable names. 
#5) Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

##Warming up...

#Download data set
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
file <- "UCIHARDataset.zip"
if(!file.exists(file)){
	print("Downloading dataset...")
	download.file(url, file)
}

#Unzip and create folders if don't exist
datafolder <- "UCI HAR Dataset"
resultsfolder <- "results"
if(!file.exists(datafolder)){
	print("unzip file")
	unzip(file, list = FALSE, overwrite = TRUE)
} 
if(!file.exists(resultsfolder)){
	print("create results folder")
	dir.create(resultsfolder)
} 

##Helper funtions
#Converts file to datatable
gettables <- function (filename, cols = NULL){
	print(paste("Getting table:", filename))
	f <- paste(datafolder,filename,sep="/")
	data <- data.frame()
	if(is.null(cols)){
		data <- read.table(f,sep="",stringsAsFactors=F)
	} else {
		data <- read.table(f,sep="",stringsAsFactors=F, col.names= cols)
	}
	data
}

#Save resulting data in the indicated folder/name
saveresults <- function (data, name){
	print(paste("saving results", name))
	file <- paste(resultsfolder, "/", name,".csv" ,sep="")
	write.csv(data,file)
}

#Getting features datatable
features <- gettables("features.txt")

#read data and build database
getdata <- function(type, features){
	print(paste("Getting data", type))
	subject_data <- gettables(paste(type,"/","subject_",type,".txt",sep=""),"id")
	y_data <- gettables(paste(type,"/","y_",type,".txt",sep=""),"activity")
	x_data <- gettables(paste(type,"/","X_",type,".txt",sep=""),features$V2)
	return (cbind(subject_data,y_data,x_data))
}

##Getting datatables for test and train sets
test <- getdata("test", features)
train <- getdata("train", features)


##Excercises

#1.Merges the training and the test sets to create one data set.
library(plyr)
data <- rbind(train, test)
data <- arrange(data, id)

#2.Extracts only the measurements on the mean and standard deviation for each measurement. 
meanAndStd <- data[,c(1,2,grep("std", colnames(data)), grep("mean", colnames(data)))]
saveresults(meanAndStd ,"mean and standard deviation")

#3.Uses descriptive activity names to name the activities in the data set
activityLabels <- gettables("activity_labels.txt")

#4.Appropriately labels the data set with descriptive variable names. 
data$activity <- factor(data$activity, levels=activityLabels$V1, labels=activityLabels$V2)

#5.Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
tidyDataset <- ddply(meanAndStd, .(id, activity), .fun=function(x){ colMeans(x[,-c(1:2)]) })
colnames(tidyDataset)[-c(1:2)] <- paste(colnames(tidyDataset)[-c(1:2)], "_mean", sep="")
saveresults(tidyDataset,"clean dataset")