#sample_data = sample_data

out <- strsplit(sub(":","A!&%DHBsdbA",sample_data$journal),"A!&%DHBsdbA") 
sample_data = data.frame(sample_data, do.call(rbind, out))
head(sample_data$X1)
head(sample_data$X2)

# strsplit. This approach uses only functions in the core of R and no complex regular expressions.
# Replace the first space with a semicolon (using sub and not gsub), 
# strsplit on the semicolon and then rbind it into a 2 column matrix:

out2 <- strsplit(sub(" ","A!&%DHBsdbA",sample_data$X2),"A!&%DHBsdbA")
sample_data = data.frame(sample_data,do.call(rbind,out2))

# Commenting so that I do not delete this column again.
sample_data = sample_data[,-11]

out3 <- strsplit(sub(" ","A!&%DHBsdbA",sample_data$X2.1),"A!&%DHBsdbA")
sample_data = data.frame(sample_data,do.call(rbind,out3))
head(sample_data$X2.2)

out_sample_data <- as.list(sub(" ","!sample_data!",sample_data$X2.2))
str(out_sample_data)
sample_data = data.frame(sample_data,do.call(rbind,out_sample_data))

out4 <- strsplit(sub(" ","A!&%DHBsdbA",sample_data$do.call.rbind..out_sample_data.),"A!&%DHBsdbA")
sample_data = data.frame(sample_data,do.call(rbind,out4))

out_sample_data1 <- as.list(sub("!sample_data!"," ",sample_data$X1.2))
str(out_sample_data)
sample_data = data.frame(sample_data,do.call(rbind,out_sample_data1))

# Seperate the Date

out5 <- strsplit(sub(";","A!&%DHBsdbA",sample_data$X2.3),"A!&%DHBsdbA")
head(out5)
sample_data = data.frame(sample_data,do.call(rbind,out5))

out6 <- strsplit(sub("\t","A!&%DHBsdbA",sample_data$X2.4),"A!&%DHBsdbA")
head(out6)
sample_data = data.frame(sample_data,do.call(rbind,out6))

out7 <- strsplit(sub("-","A!&%DHBsdbA",sample_data$do.call.rbind..out_sample_data1.),"A!&%DHBsdbA")
head(out6)
sample_data = data.frame(sample_data,do.call(rbind,out7))

#sample_data1 = sample_data

#Cleaning the Dataset

# Deleting unwanted columns:
sample_data = sample_data[, !(colnames(sample_data) %in% c("X2","X2.1","X2.2","do.call.rbind..out_sample_data.","X1.2","X2.3","X2.4","X1.4"))]

# Renaming Cols:
sample_data = rename(sample_data, c("X1"="Patent", "X1.1"="Country","do.call.rbind..out_sample_data1."="Patent_number","X1.3"="Date","X2.5"="Institution"
                      ,"X1.5"="Patent_number","X2.6"="Patent_code"))




