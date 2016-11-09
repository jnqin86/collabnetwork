US_data = read.csv("US_data.csv")

#Delete a Column
US_data <- subset(US_data, select = -c(X1,X2))


out <- strsplit(sub(" ","A!&%DHBsdbA",US_data$X2.2),"A!&%DHBsdbA")
US_data = data.frame(US_data,do.call(rbind,out))


US_data <- subset(US_data, select = -c(X2.1))
US_data <- subset(US_data, select = -c(X2.3))


out <- strsplit(sub(";","A!&%DHBsdbA",US_data$X2.3),"A!&%DHBsdbA")
US_data = data.frame(US_data,do.call(rbind,out))

US_data <- subset(US_data, select = -c(X1))

#Convert to Numeric:
US_data$X1.2 = as.numeric(as.character(US_data$X1.2))


#Remove some rows with NAs
US_data = US_data[!is.na(US_data$X1.2),]

write.csv(US_data,"US_data.csv")


US_data<-cSplit(US_data, 'X1.3', sep="-", type.convert=FALSE)

# Renaming all the columns:

colnames(US_data)[colnames(US_data) == 'X2'] <- 'journal'
colnames(US_data)[colnames(US_data) == 'X1.1'] <- 'country'
colnames(US_data)[colnames(US_data) == 'X1.2_2'] <- 'suffix_1'
colnames(US_data)[colnames(US_data) == 'X1.2'] <- 'suffix_2'
colnames(US_data)[colnames(US_data) == 'X2.1'] <- 'institute'
colnames(US_data)[colnames(US_data) == 'X1.3_1'] <- 'day'
colnames(US_data)[colnames(US_data) == 'X1.3_2'] <- 'month'
colnames(US_data)[colnames(US_data) == 'X1.3_2'] <- 'year'

