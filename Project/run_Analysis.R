##install/upload packages
install.packages("reshape2") ##install reshape package
library(reshape2) ##upload reshape package

##read relevant files into R
x_train <- read.table("x_train.txt", header = FALSE) ##uploads x_train data
x_test <- read.table("x_test.txt", header = FALSE) ##uploads x_test data
y_train <- read.table("y_train.txt", header = FALSE) ##uploads class names
y_test <- read.table("y_test.txt", header = FALSE) ##uploads class names
features <- read.table("features.txt", header = FALSE) ##uploads features names
subject_train <- read.table("subject_train.txt", header = FALSE) ##uploads subject identifiers for train data
subject_test <- read.table("subject_test.txt", header = FALSE) ##uploads subject identifiers for text data

##create merged file
data <- rbind(x_test, x_train) ##combines data sets

##label features
features_names <- features[, 2] ##creates vector of feature names
colnames(data) <- features_names ##adds column names to text data

##extract mean and std columns
extract_columns <- c(grep("mean", features_names), grep("std", features_names)) ##creates a vector of mean and std columns
extract_data <- data[,extract_columns]

##clean feature names
colnames(extract_data) <- sub("\\()", "", colnames(extract_data)) ##removes R forbidden ()
colnames(extract_data) <- sub("-std", "StandardDeviation", colnames(extract_data)) ##expands standard deviation, removes '-' 
colnames(extract_data) <- sub("-", "", colnames(extract_data)) ##removes '-' 
colnames(extract_data) <- tolower(colnames(extract_data)) ##transforms all column names to lowercase

##label activity
activity_labels_number <- rbind(y_test, y_train) ##combines activity labels (MATCHES ORDER OF merge()!)
activity_labels_number <- activity_labels_number[,1] ##transforms activity_labels_number in vector
activity_labels_list <- list() ##creates empty list for activity labels
for (n in activity_labels_number) { ##converts numeric activity list to activity label
	if (n == 1) {
		x <- "walking"
		}
	if (n == 2) {
		x <- "walkingupstairs"
		}
	if (n == 3) {
		x <- "walkingdownstairs"
		}
	if (n == 4) {
		x <- "sitting"
		}
	if (n == 5) {
		x <- "standing"
		}
	if (n == 6) {
		x <- "laying"
		}
	activity_labels_list <- rbind(activity_labels_list, x)
	}

##add activity labels to extracted data
extract_data$activity <- activity_labels_list
	
##add subject labels
subject_list <- rbind(subject_test, subject_train) ##combines test and train subject list (MATCHES ORDER of merged data)
extract_data$subjectnumber <- subject_list

##average by activity and subject
data_melt <- melt(extract_data, id = c("activity", "subject"), measure.vars=c(1:79)) ##melts data by activity and subject
tidy_data <- dcast(data_melt, subject + activity ~ variable, mean) ##creates new data set with averages per subject per activity

##write to .txt file
write.table(tidy_data, "tidy_data.txt", sep = "\t")
