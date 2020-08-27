# # # # # # # # # # # #
# Taxonomy "Task 1" data prep.
# Comes from Kaggle notebook:
# August 14, 2020
# 
# # # # # # # # # # # # #
library(dplyr)
library(splitstackshape)
library(tidyverse) # metapackage of all tidyverse packages

setwd("C:/Users/sarah/Downloads/")

virus_sub <- read.csv("Virus_sub.csv")
#virus_pub <- read.csv("Virus_pub.csv")
#virus_pub$tax_id_1 <- NULL
virus_sub$tax_id_1 <- NULL
#virus_pub$X1 <- NULL
virus_sub$X <- NULL

d <- as.data.frame(virus_sub)
d$authors<- gsub(" and",",", d$authors) 
d$authors <- gsub(" and ",",", d$authors)
d$authors <- gsub("and ",",", d$authors)

# team size sub: count number of  authors (for each pub/sub)
#library(splitstackshape)
d <- cSplit(d, "authors", ".,",'wide')
N <- d

N$iz.na<-is.na(N[,9]) 
table(N$iz.na)

N.single_auths <- N[N$iz.na=="TRUE",]
N<-N[N$iz.na=="FALSE",]
N$iz.na <- NULL


# started 8-23-2020 at 3:40 pm. 
# restarted 8-27-2020 5:11p.

# Two loops. Firstslice by year and write.csv. Then prepare for dyad processing. Second loop does dyads and writes csv per year

setwd("C://Users/Sarah/Dropbox/GenBank2016/Statistical properties/Taxonomy Analysis/")
for (i in 1992:2018)
{
  
  #i = 1992
  N.year <-  subset(N, year == i)
  
  virus.sub.year <- N.year$year
  virus.sub.journal<-N.year$journal
  virus.sub.tax_name <- N.year$tax_name
  virus.sub.top_1_taxname <- N.year$top1_tax_name
  virus.sub.tax_id <- N.year$tax_id
  
  N.year <-  (N.year)[,-1:-7] #We want JUST the authors awesome.
  
  
  tmpN <- t(combn(N.year[1,(N.year[1,])], 2)) #all possible combinations of pairs in first row then transposed.
  tmpN[1,] #the first pair
  df.N <- data.frame(Auth1 = unlist(tmpN[, 1]), Auth2 = unlist(tmpN[, 2]), stringsAsFactors = FALSE)  
  df.N <- df.N[complete.cases(df.N), ]
  
  
  df.N$ref.year <- virus.sub.year[1]
  df.N$journal <- virus.sub.journal[1]
  df.N$tax_name <- virus.sub.tax_name[1] 
  df.N$top1_tax_name <- virus.sub.top_1_taxname[1]
  df.N$tax_id <- virus.sub.tax_id[1]
  

  
  for (my.row in 2:5) {
      #my.row = 2
      # my.row <- 1 + my.row
      print(my.row)
      # print((my.row/nrow(N.year)*100)
      
      tmpN <- t(combn(N.year[my.row,(N.year[my.row,])], 2)) #all possible combinations of pairs in first row then transposed.
      tmp.df <- data.frame(Auth1 = unlist(tmpN[, 1]), Auth2 = unlist(tmpN[, 2]), stringsAsFactors = FALSE)  
      tmp.df <-tmp.df[complete.cases(tmp.df), ]    
      
      
      tmp.df$ref.year <- virus.sub.year[my.row]
      tmp.df$journal <- virus.sub.journal[my.row]
      tmp.df$tax_name <- virus.sub.tax_name[my.row] 
      tmp.df$top1_tax_name <- virus.sub.top_1_taxname[my.row]
      tmp.df$tax_id <- virus.sub.tax_id[my.row]
      
      df.N <- rbind(df.N, tmp.df)
      filename <- paste0("sub_virus_el_",i,".csv")
      write.csv(df.N,filename)
  }
} 

#N <- read.csv("C://Users/sarah/Downloads/N.csv")

