#=========================Data Cleaning Project==========================================

# Step 1

feature=read.table("./features.txt")   # Testing Parameters

train_x=read.table("./train/X_train.txt") # Testing Parameters Values
activity_train = read.table("./train/y_train.txt"); # Activity Associate With Parameters Values
subject_train = read.table("./train/subject_train.txt") # Subject Associated with Activity
activity_labels = read.table("./activity_labels.txt")


colnames(activity_labels) <- c("id","activity_name")
colnames(subject_train) <- c("subject_id")

length(grep("mean",feature$V2))
length(grep("std",feature$V2))

colnames(train_x) <- feature$V2
train_x$activity_id = activity_train$V1
less_train_x = train_x #[,c(1,2)]  #just temporarry
less_train_x$activity_id = activity_train$V1
less_train_x=merge(less_train_x,activity_labels,by.x="activity_id",by.y="id",all = FALSE,)
less_train_x$subject_id = subject_train$subject_id

test_x=read.table("./test/X_test.txt") # Testing Parameters Values
activity_test = read.table("./test/y_test.txt"); # Activity Associate With Parameters Values
subject_test = read.table("./test/subject_test.txt")

colnames(subject_test) <- c("subject_id")

colnames(test_x) <- feature$V2
test_x$activity_id = activity_test$V1
less_test_x = test_x #[,c(1,2)]  #just temporarry
less_test_x$activity_id = activity_test$V1
less_test_x=merge(less_test_x,activity_labels,by.x="activity_id",by.y="id",all = FALSE,)
less_test_x$subject_id = subject_test$subject_id

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

extract_data$activity_name = tolower(extract_data$activity_name)



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

print(answer)






