Located on the same directory where I've unziped the files:
First, I load file with feature names ~ column names

data_features = read.table('./features.txt', header = FALSE,stringsAsFactors = F)

then I create a list with the names of the features, including Users and Activities

column_names <- c('Users','Activities',as.list(data_features[,2]))

Now
data_train = read.table('./train/X_train.txt', header = FALSE,stringsAsFactors = F)
data_train_activity = read.table('./train/y_train.txt', header = FALSE,stringsAsFactors = F)
subject_train = read.table('./train/subject_train.txt', header = FALSE,stringsAsFactors = F)
data_train <- cbind(subject_train,data_train_activity,data_train)

data_test = read.table('./test/X_test.txt', header = FALSE,stringsAsFactors = F)
data_test_activity = read.table('./test/y_test.txt', header = FALSE,stringsAsFactors = F)
subject_test = read.table('./test/subject_test.txt', header = FALSE,stringsAsFactors = F)
data_test <- cbind(subject_test,data_test_activity,data_test)

##############
# here data is the full set (section 1)
data <- rbind(data_train,data_test)
colnames(data) <- column_names
##############


##############
# here (section 2) subset mean and sdt only
new_data <- subset(data[,c(grep("Users",names(data)),
                           grep("Activities",names(data)),
                           grep("mean",names(data)),
                           grep("sdt",names(data)) )])

name_activities <- function(x)
{
    if (x == "1")
    {    
        y <- c("WALKING")
    }
    if (x == "2")
    {    
        y <- c("WALKING_UPSTAIRS")
    }
    if (x == "3")
    {    
        y <- c("WALKING_DOWNSTAIRS")
    }
    if (x == "4")
    {    
        y <- c("SITTING")
    }
    if (x == "5")
    {    
        y <- c("STANDING")
    }
    if (x == "6")
    {    
        y <- c("LAYING")
    }
    return(y)
}


# lable all activities
new_data$Activities <- sapply(new_data$Activities,name_activities)


library(reshape2)
dataMelt <- melt(new_data,id=c("Users","Activities"))
tidyData <- dcast(dataMelt, Users ~ Activities,mean)

write.table(tidyData,"tidy.txt",row.name=FALSE)
