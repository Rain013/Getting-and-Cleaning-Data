#Data Science Scpecialization: Course 3, Week4
run_analysis <- function(){

#Step1: Create a directory and download the dataset in that directory  
  if (!file.exists("./mydat")){dir.create(("./mydat"))}
  url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(url, "./mydat/Dataset.zip")

#Step2: Unzip the dataset into the same directory
  unzip("./mydat/Dataset.zip", exdir = "./mydat")

#Step3: Read the training tables
  subject_train <- read.table("./mydat/UCI HAR Dataset/train/subject_train.txt")
  x_train <- read.table("./mydat/UCI HAR Dataset/train/X_train.txt")
  y_train <- read.table("./mydat/UCI HAR Dataset/train/y_train.txt")    
  
#Step4: Read the test tables 
  subject_test <- read.table("./mydat/UCI HAR Dataset/test/subject_test.txt")
  x_test <- read.table("./mydat/UCI HAR Dataset/test/X_test.txt")
  y_test <- read.table("./mydat/UCI HAR Dataset/test/y_test.txt")   
  
#Step5: Read feature vector
  features <- read.table("./mydat/UCI HAR Dataset/features.txt")
 
#Step6: Read activity labels
  activityLabels <- read.table("./mydat/UCI HAR Dataset/activity_labels.txt")
   
#Step7: Assign column names
  colnames(subject_train) <- "subjectID"
  colnames(x_train) <- features[,2]
  colnames(y_train) <- "activityID"
  
  colnames(subject_test) <- "subjectID"
  colnames(x_test) <- features[,2]
  colnames(y_test) <- "activityID"
  
  colnames(activityLabels) <- c("activityID", "activityType")

#Step8: Create test and train dataset
  testset <- cbind(y_test, subject_test,x_test)
  trainset <- cbind(y_train, subject_train, x_train)
  onedat <- rbind(trainset, testset)

#Step9: Seperate out the mean and standard deviation data    
  MeanStd <- grepl("activityID|subjectID|mean\\(\\)|std\\(\\)", colnames(onedat))
  msdata <- onedat[, MeanStd == TRUE]

#Step10: Merge the data set with activityLabels 
  tidydata <- merge(activityLabels, msdata, by = 'activityID')

#Step11: Create a new tidy dataset
   secdata <- aggregate(. ~subjectID + activityID +activityType, tidydata, mean)
   secdata <- secdata[order(secdata$subjectID, secdata$activityID, secdata$activityType),]
   write.table(secdata, file = "secdata.csv", row.names = FALSE)    
  
  }
