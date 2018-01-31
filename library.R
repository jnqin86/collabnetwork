install.packages("ggmap")
############################################################################
library(reshape2)
library(plyr)
library(dbConnect)
library(ggplot2)
library(ggmap)

#############################################################################
con <- dbConnect(MySQL(),
                 user="GenBankUser", 
                 password="123GenBank456",
                 dbname="genbank", 
                 host="metadatalab.syr.edu")

journal_field <- dbGetQuery(con,"select * from ReferencePatent LIMIT 20000;")
newdf = data.frame(journal_field$journal,journal_field$authors)


con_write <- dbConnect(MySQL(),
                 user="GenBankUser", 
                 password="123GenBank456",
                 host="metadatalab.syr.edu",
                 dbname="test")

#To write data in a MySql table:
dbWriteTable(con_write, "Assignee_M", newdf)

dbWriteTable(con_write, "US_data", US_data)


