###########################################################
#
#
# Author network overlaps: Data, Publication, Patent
# May 5, 2020
# Using approx. fuzzy matching to determine which authors are on the publications (1997-2018)
# This script is for Amit's data authors. then we'll expand it to others.
#
############################################################

path <- "C:/Users/sarah/Dropbox/GenBank2016/Statistical properties/D2K1997-2017/"

i = 1997

datknow <- read.csv(paste0(path,"D2k_",i,".csv"))
# clean
datknow$X <- NULL
d <- as.data.frame(datknow[1:171,c(1,2,7,13)])
d$authors<- gsub(" and",",", d$authors) 
d$authors <- gsub(" and ",",", d$authors)
d$authors <- gsub("and ",",", d$authors)

# team size sub: count number of  authors (for each pub/sub)
library(splitstackshape)
d <- cSplit(d, "authors", ".,",'wide')
d$teamsize <- (rowSums(!is.na(d))-3)

# Presence on data submission for ea. author (in 6500 sample) 
library(dplyr)
e <- d %>% filter_at(vars(authors_01, authors_02, authors_04,authors_05,authors_06, authors_07,
                          authors_08, authors_09, authors_10, authors_11, authors_12, authors_13,
                          authors_14, authors_15), any_vars(. %in% d$Author)) 

f <- e[which(e$type == "submission"),]
data_auths_IDs <- f$ID_1
d$has.match <- ifelse(d$ID_1 %in% data_auths_IDs, yes = "yes", no="no")

subs_auths <- d[which(d$type == "submission"),]
subs_auths <- d[which(d$has.match =="yes"),]

# First Author (add column with binary Y = 1, 0 = 2)
d$Author  <- as.character(d$Author)
d$first_author <- ifelse(d$Author == d$authors_01,yes = 1, no = 0)

number.of.authors <- ncol(d) - 3
d <- as.data.frame(d)
g <- data.frame(author = character())
g$author <- "test" 


i = 4
colname <- paste0("author_",i-3) 
out <- ifelse(d$Author == d[,i], yes = i-3, no = NA)
out  <- as.data.frame(out)
colnames(out)[1] <- colname

for (i in 5:number.of.authors)
{  
    #i = 5
    
    colname <- paste0("author_",i-3) 
    
    g <- ifelse(d$Author == d[,i], yes = i-3, no = NA)
    g <- as.data.frame(g)
    colnames(g)[1] <- colname
    
    out <- cbind(out, g)
    out <- as.data.frame(out)
}

try <- cbind(d[2],out)
yes <- cbind.data.frame(try$Author, author.order = rowSums(try[, -1], na.rm = TRUE))

yup <- cbind(d, yes$author.order)
colnames(yup)[22] <-"author.order"

yup$last_author <- ifelse(yup$teamsize == yup$author.order, yes = 1, no = 0)

yup$teamrank <- round(yup$author.order/yup$teamsize, digits = 3)  # higher scores mean that much more authors are in front of you, so to speak. Normalized by team size.
colnames(yup)

path <- "C:/Users/sarah/Dropbox/GenBank2016/Statistical properties/"
setwd(path)
write.csv(yup, "author_orders_ElbowSample.csv")


# Crap
# Author order (Add a column for which column of "authors_n" the "Author" is in) #HALP HELP
subs_auths$hasAUTH <- apply(subs_auths == "Imai,T.", 1, FUN= function(x) toString(names(x)[x]))




