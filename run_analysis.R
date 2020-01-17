## import features
file4 <- "/Users/Zaid/Desktop/Cleaning Data Project/UCI HAR Dataset/features.txt"
features <- read.table(file = file4,col.names = c("n","functions"))
file8 <- "/Users/Zaid/Desktop/Cleaning Data Project/UCI HAR Dataset/activity_labels.txt"
activity <- read.table(file = file8,col.names = c("y","activity"))

## import test
file1 <- "/Users/Zaid/Desktop/Cleaning Data Project/UCI HAR Dataset/test/X_test.txt"
x_test <- read.table(file = file1,header = FALSE, col.names = features$functions)
head(x_test)
file2 <- "/Users/Zaid/Desktop/Cleaning Data Project/UCI HAR Dataset/test/y_test.txt"
y_test <- read.table(file = file2,header = FALSE, col.names = c("y"))
file3 <- "/Users/Zaid/Desktop/Cleaning Data Project/UCI HAR Dataset/test/subject_test.txt"
subject_test <- read.table(file = file3, header = FALSE, col.names = c("subject"))

## import train
file5 <- "/Users/Zaid/Desktop/Cleaning Data Project/UCI HAR Dataset/train/X_train.txt"
x_train <- read.table(file = file5,header = FALSE, col.names = features$functions)
View(x_train)
file6 <- "/Users/Zaid/Desktop/Cleaning Data Project/UCI HAR Dataset/train/y_train.txt"
y_train <- read.table(file = file6,header = FALSE, col.names = c("y"))
file7 <- "/Users/Zaid/Desktop/Cleaning Data Project/UCI HAR Dataset/train/subject_train.txt"
subject_train <- read.table(file = file7, header = FALSE, col.names = c("subject"))

##joining test and train
subject <- rbind(subject_train,subject_test)
x <- rbind(x_train,x_test)
y <- rbind(y_train,y_test)

## combing subject, X and Y
df <- cbind(subject,x,y)
View(df)

# mean and std extract
library(dplyr)
df_extract <- df %>% select(subject,c(grep("[Mm][Ee][Aa][Nn]",names(df)),grep("[Ss][Tt][Dd]",names(df))),y)
# df_name <- data.frame(names(df),count(names(df)))
#tibble::enframe(names(df)) %>% count(value) %>% filter(n > 1)
names_freq <- as.data.frame(table(names(df))) 
names_freq[names_freq$Freq > 1, ]
names(df_extract)

#descriptive activity names
df_descriptive1 <- merge.data.frame(df_extract,activity) # merge with activity table to have descriptive names for activities
df_descriptive3 <- df_descriptive1[-1] #remove y because it is redundantg

# descriptive variable names
clean_names <- function(.data, unique = FALSE) {
  n <- if (is.data.frame(.data)) colnames(.data) else .data
  
  n <- gsub("%+", "_pct_", n)
  n <- gsub("\\$+", "_dollars_", n)
  n <- gsub("\\++", "_plus_", n)
  n <- gsub("-+", "_minus_", n)
  n <- gsub("\\*+", "_star_", n)
  n <- gsub("#+", "_cnt_", n)
  n <- gsub("&+", "_and_", n)
  n <- gsub("@+", "_at_", n)
  
  n <- gsub("[^a-zA-Z0-9_]+", "_", n)
  n <- gsub("([A-Z][a-z])", "_\\1", n)
  n <- tolower(trimws(n))
  
  n <- gsub("(^_+|_+$)", "", n)
  
  n <- gsub("_+", "_", n)
  
  if (unique) n <- make.unique(n, sep = "_")
  
  if (is.data.frame(.data)) {
    colnames(.data) <- n
    .data
  } else {
    n
  }
}
names(df_descriptive3) <- clean_names(names(df_descriptive3))
names(df_descriptive3) <- gsub("^t","time",names(df_descriptive3))
names(df_descriptive3) <- gsub("acc","accelerometer",names(df_descriptive3))
names(df_descriptive3) <- gsub(" ","",names(df_descriptive3))
names(df_descriptive3) <- gsub("^f","frequency",names(df_descriptive3))
names(df_descriptive3) <- gsub("_freq","_frequency",names(df_descriptive3))
names(df_descriptive3) <- gsub("mag","magnitude",names(df_descriptive3))
names(df_descriptive3) <- gsub("gyro","gyroscope",names(df_descriptive3))

# variable average for each activity
Final_data <- df_descriptive3 %>% group_by(subject, activity) %>% summarise_all(.funs = mean)






