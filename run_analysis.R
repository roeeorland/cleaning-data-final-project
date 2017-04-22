rm(list = ls())

library(dplyr)
#reading the files
x_test=read.table("./Project Files Dataset/UCI HAR Dataset/test/x_test.txt")
y_test=read.table("./Project Files Dataset/UCI HAR Dataset/test/y_test.txt")
x_train=read.table("./Project Files Dataset/UCI HAR Dataset/train/x_train.txt")
y_train=read.table("./Project Files Dataset/UCI HAR Dataset/train/y_train.txt")

#getting the labels
activity_labels=read.table("./Project Files Dataset/UCI HAR Dataset/activity_labels.txt")
convert_to_labels=function(x){as.character(activity_labels[x,2])}
test_activities<-as.character(apply(y_test,1:length(y_test),convert_to_labels))
train_activities<-as.character(apply(y_train,1:length(y_train),convert_to_labels))


#changing variable names 

features=read.table("./Project Files Dataset/UCI HAR Dataset/features.txt")
names(x_test)=features$V2
names(x_train)=features$V2

#adding the activities
x_test=cbind(test_activities,x_test)
names(x_test)[1]="activity"
x_train=cbind(train_activities,x_train)
names(x_train)[1]="activity"


#when we merge the test and train sets, we want to know whether an observation
#is from test or train, so we add a "type"  
type=1:length(x_test$`tBodyAcc-mean()-X`)
type[1:length(type)]="test"
x_test=cbind(x_test,type)

type=1:length(x_train$`tBodyAcc-mean()-X`)
type[1:length(type)]="train"
x_train=cbind(x_train,type)

#adding subject IDs

subject_test=read.table("./Project Files Dataset/UCI HAR Dataset/test/subject_test.txt")
x_test=cbind(subject_test,x_test)
names(x_test)[1]="subject"
subject_train=read.table("./Project Files Dataset/UCI HAR Dataset/train/subject_train.txt")
x_train=cbind(subject_train,x_train)
names(x_train)[1]="subject"

#adding the 2 data sets

complete_data_set=rbind(x_test,x_train)

#subsetting the mean and std values
mean_variables=grep("mean",names(complete_data_set))
std_variables=grep("std",names(complete_data_set))
mean_and_std_variables=c(mean_variables,std_variables)
data1=cbind(complete_data_set[,1:2],complete_data_set[,sort(mean_and_std_variables)])

# time to tidy this up

library(reshape2)
measure_variables=names(data1)[!names(data1) %in% c("subject","activity")]
data_melt=melt(data1,id=c("subject","activity"),measure.vars = measure_variables)
data_melt=arrange(data_melt,subject)
grouped_data_melt=group_by(data_melt,subject,activity,variable)
tidy_data=summarise(grouped_data_melt,mean(value))

write.table(tidy_data,file = "tidy_data.txt",row.names = FALSE)



