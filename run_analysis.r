

#
# download the zipped data files from UCI if not previously downloaded
#
if (!file.exists("getdata_projectfiles_UCI HAR Dataset.zip")) {
  download.file(url = "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip", destfile = "UCI HAR Dataset.zip")  
}


#
# unzip the dataset into the current working directory
#
if (!file.exists(".\\getdata_projectfiles_UCI HAR Dataset\\UCI HAR Dataset\\features.txt")) {
  unzip(zipfile  = "UCI HAR Dataset.zip", exdir = ".")
}

#
# save the working current directory 
#
oldWD <- getwd()

#
# change working directory to the folder with the unzipped dataset
#
setwd(".\\getdata_projectfiles_UCI HAR Dataset\\UCI HAR Dataset\\")


#
# read list of features and subset the list to only those that we are interested in -- thos containing the words std and mena
# for those that represent standard deviations and mean values
#
features <- read.table(file="features.txt", header=FALSE, col.names = c("feature.number", "feature.name"))
feature.subset.std.and.mean <- features[with(data=features, expr = grepl("mean|std", feature.name)),]

#
# read the list of activities represented in the dataset
#
activities <- read.table("activity_labels.txt", header=FALSE, col.names=c("activity.number", "activity.name"))




#
# from the folders containing the train and test data
#   read the x, y and subject files
#   combine the x, y and subject files 
#       use feature names for thecolumn names of the  x-file
#       the y-file contains the activity performed in one column - use the column name activity.number
#       the subject-file contains the numbered subject. read this into a column named subject number
# 
#   append the data from the test files to that of the train files in to a variable named total.set     
#
#
total.set <- 
    rbind(
          cbind(
                read.table(file = "train\\x_train.txt", header = FALSE, col.names = features$feature.name)
                , read.table(file = "train\\y_train.txt", header = FALSE, col.names = "activity.number")
                , read.table(file = "train\\subject_train.txt", header = FALSE, col.names = "subject.number")
                )
        
          , cbind(
                read.table(file = "test\\x_test.txt", header = FALSE, col.names = features$feature.name)
                , read.table(file = "test\\y_test.txt", header = FALSE, col.names = "activity.number")
                , read.table(file = "test\\subject_test.txt", header = FALSE, col.names = "subject.number")    
            )
          )


#
# we are done reading ... set the working directory back to the original location
#
setwd(oldWD)

#
# created a subsetted dataset that contains only the columns that we are interested in - those with std or mean in the column name 
# also include activity and subject number
#
total.set.features.subsetted <- 
  total.set[ ,
              c(feature.subset.std.and.mean$feature.number
                , grep("activity.number",dimnames(total.set)[[2]])
                , grep("subject.number",dimnames(total.set)[[2]])
                )
          ]



#
# group the subsetted data by subject and activity
# get the mean value of each of the features 
#
summary.data <- t(
                  sapply(
                          with(total.set.features.subsetted, 
                               by(total.set.features.subsetted
                                  , list(subject.number, activity.number)
                                  , function(x) x )
                               )
                          , function(x) colMeans(x)
                          )
                  )


#
# replace the activity number in each dataset with a more descriptice activity name
# first join in the list of activity names and then remove the activity number column
#
total.set.features.subsetted <- merge(total.set.features.subsetted, activities, "activity.number")
summary.data <- merge(summary.data, activities, "activity.number")

total.set$activity.number = NULL
summary.data$activity.number = NULL



write.table(x = summary.data, file = "activity.summary.data.txt", sep = "," ,row.names = FALSE)
