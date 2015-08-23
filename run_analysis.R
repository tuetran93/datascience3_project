## Set working directory to desired folder
path <- "D:\Coursera\R Programming\#3\Course Project\UCI HAR Dataset"
setwd(path)

## Read text files into R
act_label <- read.table("activity_labels.txt")
test_person_id <- read.table("test/subject_test.txt")
test_set <- read.table("test/X_test.txt")
test_label <- read.table("test/y_test.txt")
train_person_id <- read.table("train/subject_train.txt")
train_set <- read.table("train/X_train.txt")
train_label <- read.table("train/y_train.txt")
features <- read.table("features.txt")

## This function creates dataframes for test and train data. The first two columns
## are "person_id" and "activity", while the rest are named by the contents of 
## "features.txt".

merge_set <- function(id, label, set) {
        result <- cbind(id, label, set)
        colnames(result) <- c("person_id", "activity", as.character(features$V2))
        return(result)
}

## Create a comprehensive dataframe for test and train, respectively. 
test <- merge_set(test_person_id, test_label, test_set)
train <- merge_set(train_person_id, train_label, train_set)

## Comebine the test and train dataset ==> STEP 1
## The variables all have descriptive names due to merge_set function. ==> STEP 4
test_train <- rbind(test, train)


## This function subsets the dataset for mean and sd variables, while keeping
## keeping the first two identifiers.
mean_std_subset <- function(df) {
        indices <- grep("mean\\(\\)|std\\(\\)", names(df))
        df[, c(1:2, indices)]
}

## Extract mean and sd measurements from test_train dataset. ==> STEP 2 
mean_std <- mean_std_subset(test_train)

## This function labels the activities and re-orders the columns, as merging 
## moves the "activity" column before the "person_id" column.
label <- function(df) {
        df <- merge(df, act_label, by.x = "activity", by.y = "V1")
        ## Remove the old "activity" column, and move the label column "V2" to 
        ## 2nd place.
        df <- df[, c(2, length(df), 3:(length(df) - 1))]
        ## Rename the label column from "V2" to "activity"
        df <- rename(df, activity = V2)
        df <- arrange(df, person_id, activity)
        return(df)
}

## Call the label function to name the activities in the mean_std dataset 
## ==> STEP 3
mean_std <- label(mean_std)

## This function cleans up the column names for a dataset.
clean_name <- function(df) {
        sub1 <- gsub("\\(\\)", "", names(df))
        sub2 <- gsub("\\-", ".", sub1)
        return(sub2)
}

## Call clean_name on mean_std.
names(mean_std) <- clean_name(mean_std)

## Combine the person_id and activity in mean_std into one combined_id column. 
## Then, create a new data set, mean_std_1, that removes person_id and activity.
mean_std <- mutate(mean_std, combined_id = paste(person_id, activity, sep = ""))
mean_std_1 <- mean_std[, c(length(mean_std), 3:length(mean_std))]

## Group mean_std_1 by the combined_id column, then summarise_each to calculate
## the mean of each column ==> STEP 5
mean_std_1 <- group_by(mean_std_1, combined_id)
mean_std_1 <- summarise_each(mean_std_1, funs(mean))

mean_std_1
