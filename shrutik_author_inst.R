library(reshape2)
library(plyr)
library(dbConnect)

con <- dbConnect(MySQL(),
                 user="GenBankUser", 
                 password="123GenBank456",
                 dbname="genbank", 
                 host="metadatalab.syr.edu")

journal_field <- dbGetQuery(con,"select journal from ReferencePatent;")
author_list <- read.csv("unique_patent_authors.csv")
head(author_list)

# SELECT * FROM ReferencePatent LIMIT 0,1000;
# Bring examples
colnames(author_list) <- c("id","Name")

author_list$Name = as.character(author_list$Name)
out1 <- strsplit(author_list$Name," - ") 
author_list = data.frame(author_list, do.call(rbind, out1))


author_list$X2 = as.character(author_list$X2)
out2 <- strsplit(author_list$X2,",") 
author_list = data.frame(author_list, do.call(rbind, out2))


unique_lastNames = author_list$X5
unique_lastNames = data.frame(unique_lastNames)


###################################################################
# REGEX
##################

journal_field[grepl('smith',journal_field)]

