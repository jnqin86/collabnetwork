# Disambiguation - Cleaning the journal field 
# Sarah Bratt | 0312018
# Parse out the genbank.ReferencePatent table journal field. 
# Subset for non US patents and no "." in authors field.
# Take ref_id, patent_id, title, pattentype, authors. In chunks?
# Transpose and Melt, so there is one author name per line. 
# Please run stats at this point (how many unique patent numbers? How many rows? etc.)
# Write bridge table (dbWrite.table). 


getwd()
setwd("/data/rstudio4/disambiguation")

library(dbConnect)
library(splitstackshape)
library(RMySQL)
# get data to parse journal field. where not US and authors not '.'

con<-dbConnect(MySQL(),user="GenBankUser", password="HH16collabnet",host="metadatalab.syr.edu", dbname="genbank")



start.time <- Sys.time()
# Check for the patents with no authors.
rp <- dbGetQuery(con,"SELECT * FROM genbank.ReferencePatent AS rp 
                  WHERE rp.authors not like '.' 
                  AND rp.journal not like 'Patent: US%';")

# "No limit" took: 6 minutes - not bad at all.
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

# Number of rows in original ReferencePatent table = 23,791,099 - I checked table inspector in MySQL workbench.
# Number of rows in rp table according to "actual" query: 26,003,035 <- WE can trust this one. 
whole <- dbGetQuery(con, "SELECT COUNT(*) FROM genbank.ReferencePatent;")
whole

# Resulting rows in no '.' author and no like 'US' patent:  17,989,559  
dim(rp)
26003035 - nrow(rp) # = 8013476 less authors after the filter.  

# Of these, how many are '.'? 16492 
dot.count <- dbGetQuery(con, "SELECT COUNT(*) FROM genbank.ReferencePatent AS rp 
                  WHERE rp.authors like '.';")
dot.count

# How many are 'US'? 7996978
US <- dbGetQuery(con, "SELECT COUNT(*) FROM genbank.ReferencePatent AS rp 
                  WHERE rp.journal like 'Patent: US%';")
US
US + dot.count # = 8,013470, combined. Weird - there is a discrepancy of 6 between the counts of rp length and regex match for US and dot.count. 


colnames(rp)
rp.df <- rp[c(1,3,5,6)] 
# I excluded bases (reference), consortium, pubmed, remark because these are text fields. 
# They will take extra computational muscle, and pubmed will not (theoretically) contain any data because only US
# patents have affiliated pubmedids
rm(rp)
# Let's check just in case...
pmids.pats.count <- dbGetQuery(con, "SELECT COUNT(*) FROM genbank.ReferencePatent AS rp 
                  WHERE rp.pubmed IS NULL;")
pmids.pats.count # yeah, they are all NULL 26003035

colnames(rp.df)
sample_data <- rp.df

# Cleaned out workspace with ls(), rm(i), and gc() at this point. 
# A call of gc causes a garbage collection to take place. 
# This will also take place automatically without user intervention, and 
# the primary purpose of calling gc is for the report on memory usage.
# However, it can be useful to call gc after a large object has been removed, 
# as this may prompt R to return memory to the operating system.

start.time <- Sys.time()
out <- strsplit(sub(":","A!&%DHBsdbA",sample_data$journal),"A!&%DHBsdbA") 
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

start.time <- Sys.time()
sample_data = data.frame(sample_data, do.call(rbind, out))
head(sample_data$X1)
head(sample_data$X2)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
# strsplit. This approach uses only functions in the core of R and no complex regular expressions.
# Replace the first space with a semicolon (using sub and not gsub), 
# strsplit on the semicolon and then rbind it into a 2 column matrix:

start.time <- Sys.time()
out2 <- strsplit(sub(" ","A!&%DHBsdbA",sample_data$X2),"A!&%DHBsdbA")
sample_data = data.frame(sample_data,do.call(rbind,out2))
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

# Commenting so that I do not delete this column again.
# sample_data = sample_data[,-11]
start.time <- Sys.time()
out3 <- strsplit(sub(" ","A!&%DHBsdbA",sample_data$X2.1),"A!&%DHBsdbA")
sample_data = data.frame(sample_data,do.call(rbind,out3))
# head(sample_data$X2.2)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

start.time <- Sys.time()
out_sample_data <- as.list(sub(" ","!sample_data!",sample_data$X2.2))
str(out_sample_data)
sample_data = data.frame(sample_data,do.call(rbind,out_sample_data))
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

start.time <- Sys.time()
out4 <- strsplit(sub(" ","A!&%DHBsdbA",sample_data$do.call.rbind..out_sample_data.),"A!&%DHBsdbA")
sample_data = data.frame(sample_data,do.call(rbind,out4))
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

start.time <- Sys.time()
out_sample_data1 <- as.list(sub("!sample_data!"," ",sample_data$X1.2))
# str(out_sample_data)
sample_data = data.frame(sample_data,do.call(rbind,out_sample_data1))
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
# Separate the date out

start.time <- Sys.time()
out5 <- strsplit(sub(";","A!&%DHBsdbA",sample_data$X2.3),"A!&%DHBsdbA")
# head(out5)
sample_data = data.frame(sample_data,do.call(rbind,out5))
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

start.time <- Sys.time()
out6 <- strsplit(sub("\t","A!&%DHBsdbA",sample_data$X2.4),"A!&%DHBsdbA")
head(out6)
sample_data = data.frame(sample_data,do.call(rbind,out6))
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken


start.time <- Sys.time()
out7 <- strsplit(sub("-","A!&%DHBsdbA",sample_data$do.call.rbind..out_sample_data1.),"A!&%DHBsdbA")
# head(out6)
sample_data = data.frame(sample_data,do.call(rbind,out7))
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

sample_data1 = sample_data

#Cleaning the Dataset

# Deleting unwanted columns:

keeps <- sample_data[c(1:4,12,15,18,19)]
                     
sample_data2 <- cSplit(keeps, "X1.3", "!sample_data!", 'wide')
sample_data2 <- cSplit(sample_data2, "X1.3_1", "-", 'wide')
# Renaming Cols:

colnames(sample_data2)[5] = "Date" 
colnames(sample_data2)[6] = "Affiliation" 
colnames(sample_data2)[7] ="patenttype"
colnames(sample_data2)[8] = "seq_index"
colnames(sample_data2)[9] = "patentnumber"
colnames(sample_data2)[10] = "seq_version"

# remove white spaces.
start.time <- Sys.time()
sample_data2$authors<- gsub(" and",",", sample_data2$authors)
sample_data2$authors <- gsub(" and ",",", sample_data2$authors)
sample_data2$authors <- gsub("and ",",", sample_data2$authors)
sample_data2$authors <-  gsub(" ", "", sample_data2$authors)
#substitute for the three variants in formatting for the author names. 

df.auths<- cSplit(sample_data2, "authors", ".,",'long')
# df.auths1 <-cSplit(sample_data2, "authors", ",", 'wide')
dbWriteTable(con, "InternationalPatentAuthors", df.auths)


# Check for new db InternationalPatentAuthors
system.time(IPA <- dbGetQuery(con,"SELECT COUNT(*), ipa.* FROM test.InternationalPatentAuthors AS ipa 
                  GROUP BY patentnumber LIMIT 1000;"))
# Time difference of 1.11725 hours

#################################################################################################

# Parse out the initials after the author's last name
system.time(intl <- dbGetQuery(con,"SELECT ipa.* FROM test.InternationalPatentAuthors AS ipa;"))
# Time taken: 1870.123 seconds is 31 minutes. not bad. 

system.time(df.auths <-cSplit(intl, "authors", ",", 'wide'))
# Time taken: 545.107 9 mins. 

not.blanks <- df.auths[which(!df.auths$authors_1 == ""),]

blanks <- df.auths[which(df.auths$authors_1 == ""),]
blanks$authors_1 <- NULL
colnames(blanks)[15] <- "authors_3"
colnames(blanks)[14] <- "authors_2"
colnames(blanks)[13] <- "authors_1"
blanks$authors_4 <- NA
blanks$row_names <- NULL
not.blanks$row_names <- NULL

colnames(blanks)
colnames(not.blanks)

bind <- rbind(blanks, not.blanks)
nrow(not.blanks)+nrow(blanks) 

colnames(bind)[10] <- "extra_title_parse1"
colnames(bind)[11] <- "extra_title_parse2"

dbWriteTable(con, "Par_InternationalPatentAuthors", bind)
dbRemoveTable(con, "InternationalPatentAuthors")

####################################################################

system.time(blanks.splt <- cSplit(blanks, "authors_3", ".", 'wide'))
system.time(blanks.splt <- cSplit(blanks.splt, "authors_4", ".", 'wide'))

system.time(not.blanks.splt <- cSplit(not.blanks, "authors_3", ".", 'wide'))
system.time(not.blanks.splt <- cSplit(not.blanks.splt, "authors_4", ".", 'wide'))
system.time(not.blanks.splt <- cSplit(not.blanks.splt, "authors_2", ".", 'wide'))
system.time(df3.1 <- cSplit(df3, "V12", ".", 'wide'))

View(head(not.blanks.splt))

# Shift NAs to the left. Try it first on a smaller subset:

# lil_guy <- blanks.splt[1:10000,]
# df1 = as.data.frame(t(apply(lil_guy,1, function(x) { return(c(x[!is.na(x)],x[is.na(x)]) )} )))

# Shift to the left to bypass NAs: the "real deal" on blanks subset, then on not.blanks subset.
df2 = as.data.frame(t(apply(blanks.splt,1, function(x) { return(c(x[!is.na(x)],x[is.na(x)]) )} )))
df3 = as.data.frame(t(apply(not.blanks.splt,1, function(x) { return(c(x[!is.na(x)],x[is.na(x)]) )} )))
df3.1 = as.data.frame(t(apply(df3.1,1, function(x) { return(c(x[!is.na(x)],x[is.na(x)]) )} )))

write.csv(df2, "df2.csv")
write.csv(df3, "df3.csv")

df3 <- read.csv("df3.csv")

View(head(df2))
View(head(df3))

#row-bind the two cleaned up, parsed dfs. 
bind.split <- rbind(df2, df3)


