#=========================Data Cleaning Project==========================================

# Step 1

feature=read.table("./UCI_HAR_Dataset/features.txt")   # fetching features parameters

train_x=read.table("./UCI_HAR_Dataset/train/X_train.txt") # fetching records corresponding to featrures
activity_train = read.table("./UCI_HAR_Dataset/train/y_train.txt"); # Activity Associate With Parameters Values
subject_train = read.table("./UCI_HAR_Dataset/train/subject_train.txt") # Subject Associated with Activity
activity_labels = read.table("./UCI_HAR_Dataset/activity_labels.txt") # fetching activity lebels 


# Setting proper column names to activity label dataframe

colnames(activity_labels) <- c("id","activity_name")

# Setting proper column names to subject data frame

colnames(subject_train) <- c("subject_id")

# Setting column names of train_x dataset by the values of features
colnames(train_x) <- feature$V2

# Appending column activity to the train_x dataset by the values fo activity table
train_x$activity_id = activity_train$V1
less_train_x = train_x #[,c(1,2)]  #just temporarry
less_train_x$activity_id = activity_train$V1
less_train_x=merge(less_train_x,activity_labels,by.x="activity_id",by.y="id",all = FALSE,)
less_train_x$subject_id = subject_train$subject_id

# Fetching dataset from test category observation 
test_x=read.table("./UCI_HAR_Dataset/test/X_test.txt")

# Activity Associate With Parameters Values
activity_test = read.table("./UCI_HAR_Dataset/test/y_test.txt");

# fetchin subject dataset for test 
subject_test = read.table("./UCI_HAR_Dataset/test/subject_test.txt")

# Setting proper column names to subject test dataframe
colnames(subject_test) <- c("subject_id")

colnames(test_x) <- feature$V2
test_x$activity_id = activity_test$V1
less_test_x = test_x #[,c(1,2)]  #just temporarry
less_test_x$activity_id = activity_test$V1
less_test_x=merge(less_test_x,activity_labels,by.x="activity_id",by.y="id",all = FALSE,)
less_test_x$subject_id = subject_test$subject_id

# Merges the training and the test sets to create one data set.

final_data_set = rbind(less_train_x,less_test_x) #final Result Set..



#============Extracting Mean And Std Deviation From Data Set============================
#Step 2

meancolname=grep("mean",feature$V2,value = TRUE)
stdcolname=grep("std",feature$V2,value= TRUE)
conam=union(meancolname,stdcolname)
col = c("subject_id","activity_name",conam);
extract_data = final_data_set[,col];

#========Descriptive Activity Name =================================================
#Step 3

extract_data$activity_name = gsub("_"," ",tolower(extract_data$activity_name))



#===========Descriptive Variable Label Name ===========================================
#Step 4

lower_colnames = tolower(col)
lower_col = gsub("\\()","",lower_colnames)
col_name_underscore = gsub("-","_",lower_col)
colnames(extract_data) <- col_name_underscore


#=============Mean ===================================================================
#Step 5

answer = aggregate(extract_data, list(extract_data$subject_id,extract_data$activity_name), mean)
answer = answer[,c(1:2,5:83)]
colnames(answer) <- col_name_underscore
answer = answer[order(answer$subject_id,answer$activity_name),]

write.table(answer,file="./result.txt",row.names = FALSE)

#====================Final Output Printing============================================
# printing final answer
print(answer)






