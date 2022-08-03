#Investigating publication citations and their relationship to GenBank submissions

# ABSTRACT: Biomedical scientists increasingly use large nucleotide acid sequence data repositories such as the DNA Databank of 
# Japan (DDBJ) and NCBI's GenBank. In this way, scientists build upon other's work and enhance reproducibility but there is no formal 
# mechanism for directly giving credit to the creators of the data. In this study, we exploit the citation network between publications 
# to estimate the relative importance of data submissions. In particular, we use the GenBank metadata to build submission-to-publication 
# links. We analyze the submissions with most citations and examine features of the submission that are related with differences in 
# citations. This work therefore assigns credit to submissions, which is something that is not available now.
# PAPER: https://arxiv.org/abs/2001.05917
#updated 8-3-2022 to connect datasets to publications 

install.packages("dbConnect")
library(dbConnect)
con<-dbConnect(MySQL(), user="GenBankUser", password="HH16collabnet", host="metadatalab.syr.edu", dbname="genbank2018")

test<-dbGetQuery(con, "SELECT * FROM Reference LIMIT 10;")
# See how there is a dataset associated with the publication according to the id? 
# So reference "bundle" is id= 400 and ref = 1,2.

# Get the pubmed id records. Or let's do just a handful first.   
pubmed <- dbGetQuery(con, "SELECT r.* FROM Reference as r where pubmed IS NOT NULL 
                     AND r.year >2002;")

for (my.row in 1:dim(pubmed)[1]) {
  # my.row <- 1 + my.row
  # print(my.row)
  df <- nchar
  
  
  colnames(tmp.f) <- c("Auth1", "Auth2")
  
  tmp.f$ref.years<- ref.years[my.row]
  tmp.f$ref.type <- ref.type[my.row]    
  
  tmp.f <- rbind(tmp.f, tmp.f)
}
pubmed$length < - nchar(pubmed$pubmed[1])



# Clean: unnessary columns and formatting issues 
# pubmed$remark <-NULL
# pubmed$consortium <- NULL
# pubmeds <- pubmed[5:nrow(pubmed), ]
View(head(pubmed))

# Csplit the last three digits out into a new col. 
# install.packages("splitstackshape")
library(splitstackshape)

# pubmed2 <- transform(pubmed, refid = substr(id, 1, 8), index = substr(id, 9, 10))
# pubmed2[1:3,]

#install.packages("tidyr")
library(tidyr)
pm <- pubmed %>% separate(id, into = c('refid', 'index'), sep = -2, convert = FALSE)
View(head(pm))

p.med.all <- dbGetQuery(con, "SELECT * From genbank2018.Reference WHERE year >2002 ORDER BY id ASC, pubmed DESC LIMIT 10000;")
dim(p.med.all)
View(head(p.med.all))

p.med.all <- p.med.all %>% separate(id, into = c('refid', 'index'), sep = -2, convert = FALSE)
View(head(p.med.all))
p.med.all <- p.med.all[grep("Direct Submission", p.med.all$title), ] 

nrow(sub.p.med.sep)/nrow(p.med.sep)
# How many of the records are Direct Submissions?

p.index <- dbGetQuery(con,"SELECT COUNT(*), r.id, r.pubmed FROM Reference as r 
WHERE pubmed IS NOT NULL GROUP BY pubmed ORDER BY id ASC, pubmed DESC LIMIT 50000;")
dim(p.index)
View(head(p.index))

count <- as.data.frame(p.index[,1])
count.t <-table(count)
plot(count.t)

p.index.sep <- p.index %>% separate(id, into = c('refid', 'index'), sep = -2, convert = FALSE)
p.index.sep [1:4,]
dim(p.index.sep)

mini.indx <- p.index.sep[1:10,]
mini.pmed <- sub.p.med.sep[1:20,]

mini.merge <- merge(x = mini.indx, y = mini.pmed, by= 'refid', )

dim(mini.merge)
mini.merge

mega.merge <- merge(x = p.index.sep, y =sub.p.med.sep, by= 'refid')

dim(p.index.sep)
dim(sub.p.med.sep)
dim(mega.merge)

colnames(mega.merge)[2] <- "count"
colnames(mega.merge)

head(mega.merge[order(-mega.merge[2]), ])
max(mega.merge$count)

#myvars <- c("refid", "index.x", "pubmed.x", "pubmed.y","year", "title")
pmids <- mega.merge[myvars]
dim(pmids)

head(pmids)

# Convert Pubmed ids to DOIs
PMIDS <- read.csv("PMC-ids.csv")

myvars <-c("refid", "count", "index.x", "PMID", "reference", "authors", "consortium", "title","journal", "remark", "year")


colnames(PMIDS)
colnames(mega.merge)[4] <- "PMID"
colnames(mega.merge)
# sum(is.na(mega.merge$pubmed.x))

mega.merge[1:2,]
mega.merge <- mega.merge[myvars]

DOI.merge <- merge(x = mega.merge, y = PMIDS, by = 'PMID') # this is an "inner join" so all and ONLY matches are returned. 
dim(DOI.merge)
































