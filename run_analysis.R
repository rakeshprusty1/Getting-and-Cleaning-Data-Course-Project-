
##########################################################################################################


# 1. Merge the training and the test sets to create one data set.

# Read in the data from files and assign column names
features        <- read.table('features.txt'); #imports features.txt
activity_labels <- read.table('activity_labels.txt'); #imports activity_labels.txt

names(activity_labels)  <- c('activity_id','activity_type');

# Read in the training data sets
subject_train   <- read.table('subject_train.txt'); #imports subject_train.txt
x_train         <- read.table('x_train.txt'); #imports x_train.txt
y_train         <- read.table('y_train.txt'); #imports y_train.txt

# Assign column names to training data sets
names(subject_train)  <- "subject_id";
names(x_train)        <- features[,2]; 
names(y_train)        <- "activity_id";

# Create the final training set by merging y_train, subject_train, and x_train
train_data <- cbind(y_train,subject_train,x_train);

# Read in the test data sets
subject_test <- read.table('subject_test.txt',header=FALSE); #imports subject_test.txt
x_test       <- read.table('x_test.txt',header=FALSE); #imports x_test.txt
y_test       <- read.table('y_test.txt',header=FALSE); #imports y_test.txt

# Assign column names to test data sets
names(subject_test) <- "subject_id";
names(x_test)       <- features[,2]; 
names(y_test)       <- "activity_id";


# Create the final test set by merging the x_test, y_test and subject_test data
test_data <- cbind(y_test,subject_test,x_test);


# Combine training and test data to create a final data set
final_data <- rbind(train_data,test_data);


# 2. Extract only the measurements on the mean and standard deviation for each measurement.

# Create a vector for the column names from the final_data, which will be used
# to select the desired mean() & stddev() columns
col_names  <- names(final_data); 

# Create a logical_vector that contains TRUE values for the ID, mean() & stddev() columns and FALSE for others
logical_vector <- (grepl("activity..",col_names) | grepl("subject..",col_names) | grepl("-mean..",col_names) & !grepl("-meanFreq..",col_names) & !grepl("mean..-",col_names) | grepl("-std..",col_names) & !grepl("-std()..-",col_names));

# Subset final_data table based on the logical_vector to keep only desired columns
final_data <- final_data[logical_vector];

# 3. Use descriptive activity names to name the activities in the data set

# Merge the final_data set with the acitivityType table to include descriptive activity names
final_data <- merge(final_data,activity_labels,by='activity_id',all.x=TRUE);

# Updating the col_names vector to include the new column names after merge
col_names  <- names(final_data); 

# 4. Appropriately label the data set with descriptive activity names. 

# Cleaning up the variable names

col_names <- gsub("\\()","",col_names);
col_names <- gsub("^t","time",col_names);
col_names <- gsub("^f","freq",col_names);
col_names <- gsub("-","_",col_names)

# Reassigning the new descriptive column names to the final_data set
names(final_data) <- col_names;

# 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject. 

# Create a new table, final_dataNoactivity_type without the activity_type column
final_dataNoactivity_type  <- final_data[,names(final_data) != 'activity_type'];

# Summarizing the final_dataNoactivity_type table to include just the mean of each variable for each activity and each subject
tidy_data    <- aggregate(final_dataNoactivity_type[,names(final_dataNoactivity_type) != c('activity_id','subject_id')],by=list(activity_id=final_dataNoactivity_type$activity_id,subject_id = final_dataNoactivity_type$subject_id),mean);

# Merging the tidy_data with activity_type to include descriptive acitvity names
tidy_data    <- merge(tidy_data,activity_type,by='activity_id',all.x=TRUE);

# Export the tidy_data set 
write.table(tidy_data, './tidy_data.txt',row.names=FALSE,sep='\t')
