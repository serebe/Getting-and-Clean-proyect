# Open the libreries which we need
library(dplyr)
# read from archives
link <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
descarga <- "archivos.zip"
# creation from the archive in the carpet 
if (!file.exists(descarga)) {
  download.file(link, descarga, mode = "wb")
}
# unzip the archivo.zip folder 
packdatos <- "UCI HAR Dataset"
if (!file.exists(packdatos)) {
  unzip(descarga)
}
# open of the train carpet
train1 <- read.table(file.path(packdatos, "train", "subject_train.txt"))
train2 <- read.table(file.path(packdatos, "train", "X_train.txt"))
train3 <- read.table(file.path(packdatos, "train", "y_train.txt"))
# open of the test carpet
test1 <- read.table(file.path(packdatos, "test", "subject_test.txt"))
test2 <- read.table(file.path(packdatos, "test", "X_test.txt"))
test3 <- read.table(file.path(packdatos, "test", "y_test.txt"))
# read features
carateristicas <- read.table(file.path(packdatos, "features.txt"), as.is = TRUE)
# read activities
actividades <- read.table(file.path(packdatos, "activity_labels.txt"))
colnames(actividades) <- c("Identificacion", "actividad")
# cration from the table(merge) primero:
desarrollo <- rbind(
  cbind(train1, train2, train3),
  cbind(test1, test2, test3)
)
# assign column names
colnames(desarrollo) <- c("subject", carateristicas[, 2], "actividades")
sostenido <- grepl("subject|actividades|mean|std", colnames(desarrollo))
desarrollo <- desarrollo[, sostenido]
# convert someone factor variables
desarrollo$actividades <- factor(desarrollo$actividades, levels = actividades[, 1], labels = actividades[, 2])
# get column names
columnasdesarrollo <- colnames(desarrollo)
# we correct some terms
columnasdesarrollo <- gsub("[\\(\\)-]", "", columnasdesarrollo)
columnasdesarrollo <- gsub("^f", "frequencyDomain", columnasdesarrollo)
columnasdesarrollo <- gsub("^t", "timeDomain", columnasdesarrollo)
columnasdesarrollo <- gsub("Acc", "Accelerometer", columnasdesarrollo)
columnasdesarrollo <- gsub("Gyro", "Gyroscope", columnasdesarrollo)
columnasdesarrollo <- gsub("Mag", "Magnitude", columnasdesarrollo)
columnasdesarrollo <- gsub("Freq", "Frequency", columnasdesarrollo)
columnasdesarrollo <- gsub("mean", "Mean", columnasdesarrollo)
columnasdesarrollo <- gsub("std", "StandardDeviation", columnasdesarrollo)
columnasdesarrollo <- gsub("BodyBody", "Body", columnasdesarrollo)
colnames(desarrollo) <- columnasdesarrollo
# use group by
finalizado <- desarrollo %>% 
  group_by(subject, actividades) %>%
  summarise_each(list(mean))
# write the table
write.table(finalizado, "tidy_data.txt", row.names = FALSE, quote = FALSE)
