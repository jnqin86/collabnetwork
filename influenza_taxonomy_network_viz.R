#
#The purpose of this code file is to visualize author collaboration networks in influenza taxonomy dataset and drilling through it by levels of taxonomy   
#

install.packages("dbConnect")
library(dbConnect)

install.packages("splitstackshape")


install.packages("stringr")

install.packages("igraph")

install.packages("igraph")

library("splitstackshape")
library("stringr")
library("igraph")

#library("rgl")

#creating a connection to mySQL server to read the data
con <- dbConnect(MySQL(),
                 user="GenBankUser",
                 password="123GenBank456",
                 dbname="genbank",
                 host="metadatalab.syr.edu")

#query that acesses influenza taxonomy direct submissions
influenza_taxonomy =  dbGetQuery(con,"select o_n.top2_tax_name as 'phylum', o_n.top3_tax_name as 'class',o_n.bottom3_tax_name as 'order',
                                 o_n.bottom2_tax_name as 'family', o_n.bottom1_tax_name as 'genus',o_n.tax_name as 'species',r.*,
                                 tn.name, tn.parent_name ,o.content from genbank.Reference as r 
                                 inner join 
                                 genbank.AnnotationReference as ar on r.id = ar.referenceId 
                                 inner join genbank.Annotation as a on  ar.gi = a.gi  
                                 inner join genbank.Organism as o on o.id = a.organismId 
                                 inner join genbank.taxNode as tn on tn.id = a.tax_id
                                 inner join genbank.Organism_new as o_n on o_n.tax_id = tn.id
                                 where o.content like  '%influenza%' and tn.name like '%influenza%' and  r.title like '%Direct%Submission%';")

#Reading influenza taxonomy dataset for the year 1997 as a prototype
influenza_taxonomy_1997 = read.csv("influenza_taxonomy_1997.csv",header = T, stringsAsFactors = FALSE)
str(influenza_taxonomy_1997)
nrow(influenza_taxonomy_1997)

#level 1: Complete dataset
table(influenza_taxonomy_1997$phylum)

authors = influenza_taxonomy_1997$authors
phylum = influenza_taxonomy_1997$phylum

influenza_taxonomy_1997_authors = data.frame(authors,stringsAsFactors = FALSE) 
influenza_taxonomy_1997_phylum = data.frame(phylum,stringsAsFactors = FALSE)
str(influenza_taxonomy_1997_authors)
str(influenza_taxonomy_1997_phylum)
View(influenza_taxonomy_1997_authors)

#Separating author names into one name per column for further computational ease
influenza_taxonomy_1997_authors$authors = gsub("\\s+", " ", str_trim(influenza_taxonomy_1997_authors$authors))
influenza_taxonomy_1997_authors$authors = gsub(" and ", ", ", influenza_taxonomy_1997_authors$authors)
influenza_taxonomy_1997_authors = cSplit(influenza_taxonomy_1997_authors, "authors","., ", direction = "wide",type.convert = FALSE)
View(influenza_taxonomy_1997_authors)
str(influenza_taxonomy_1997_authors)


#setting intermediate variables to null
df = NULL
tmp = NULL
tmp.df = NULL

#Creating a dataframe with 2 authors per row from the above dataset until all the authors (of ech row from thee previos dataset) are grouped 
#make an empty dataframe
df <- data.frame(Auth1 = character(), Auth2 = character(), phylum =character(), stringsAsFactors = FALSE)
influenza_taxonomy_1997_authors = data.frame(influenza_taxonomy_1997_authors, stringsAsFactors = FALSE)

#for each row, make a full list of pairs
for (i in 1:nrow(influenza_taxonomy_1997_authors)) {
  # make combinations of 2 from the whole first line (after ignoring NAs)
  if(!is.na(influenza_taxonomy_1997_authors[i,2]) == TRUE)
  {
    tmp <- t(combn(influenza_taxonomy_1997_authors[i,!is.na(influenza_taxonomy_1997_authors[i,])], 2)) 
  }
  else {
    tmp = influenza_taxonomy_1997_authors[i,]
  }
  tmp.df <- data.frame(Auth1 = unlist(tmp[, 1]), Auth2 = unlist(tmp[, 2]), stringsAsFactors = FALSE)
  tmp.df$phylum = influenza_taxonomy_1997_phylum[i,]
  df <- rbind(df, tmp.df, stringsAsFactors = FALSE)
}
str(df)
View(df)

#creating a final dataframe to use for network analysis
df2 <- data.frame(Auth1 = as.character(df$Auth1), Auth2 = as.character(df$Auth2), phylum = df$phylum, stringsAsFactors = FALSE)

#create a igraoh object to perform further network analysis
g <- graph.data.frame(df2, directed=TRUE)  

#Computing network parameters
#vertex count
vcount(g)

#edge count
ecount(g)

#density
edge_density(g, loops=TRUE) 

#centralization
#number of submissions
n_sub = nrow(influenza_taxonomy_1997)

#number of authors
n_auth = length(unique(c(df2$Auth1, df2$Auth2)))

#degree centralization
centr_degree(g,loops = TRUE)

#betweenness centralization
centr_betw(g)

#executing 'g' shows us that phylum is on the edges
g

table(E(g)$phylum)

E(g)$color <- ifelse(E(g)$phylum == "Proteobacteria", "red","blue")

coordsFR <- layout.fruchterman.reingold(g, dim=3)

#setting seed to control randomness in plots while drilling down the stages
set.seed(42)

#plotting the network of collaboration
plot.igraph(g, edge.arrow.size=0.1, edge.arrow.width=.5,vertex.size = 2,vertex.color = "black",main="Collaboration network: GenBank submissions", vertex.label=NA, margin = -.2)
mtext( paste0("Number of Submissions " ,n_sub ,"| Number of authors " ,n_auth, "| Vertex Count " ,vcount(g),"| Edge Count " ,ecount(g)  ) , side = 1, line = 3)
rglplot(g,vertex.size = 2,vertex.label = NA,layout = coordsFR, margin = 3, asp= -1)


#level 2:Phylum level 
table(influenza_taxonomy_1997$phylum)
influenza_taxonomy_1997_phylum = subset(influenza_taxonomy_1997, phylum == "ssRNA negative-strand viruses")

authors = influenza_taxonomy_1997_phylum$authors
class = influenza_taxonomy_1997_phylum$class
influenza_taxonomy_1997_phylum_authors = data.frame(authors, stringsAsFactors = FALSE) 
str(influenza_taxonomy_1997_phylum_authors)
influenza_taxonomy_1997_phylum_class = data.frame(class, stringsAsFactors = FALSE)
str(influenza_taxonomy_1997_phylum_class)
View(influenza_taxonomy_1997_phylum_authors)

#Separating author names into one name per column for further computational ease
influenza_taxonomy_1997_phylum_authors$authors = gsub("\\s+", " ", str_trim(influenza_taxonomy_1997_phylum_authors$authors))
influenza_taxonomy_1997_phylum_authors$authors = gsub(" and ", ", ", influenza_taxonomy_1997_phylum_authors$authors)
influenza_taxonomy_1997_phylum_authors = cSplit(influenza_taxonomy_1997_phylum_authors, "authors","., ", direction = "wide", type.convert = FALSE)
str(influenza_taxonomy_1997_phylum_authors)


#setting intermediate variables to null
df = NULL
tmp = NULL
tmp.df = NULL

#Creating a dataframe with 2 authors per row from the above dataset until all the authors (of ech row from thee previos dataset) are grouped 
#make an empty dataframe
df <- data.frame(Auth1 = character(), Auth2 = character(), class =character(), stringsAsFactors = FALSE)
str(influenza_taxonomy_1997_phylum_authors)

influenza_taxonomy_1997_phylum_authors = data.frame(influenza_taxonomy_1997_phylum_authors, stringsAsFactors = FALSE)

#for each row, make the full list of pairs
for (i in 1:nrow(influenza_taxonomy_1997_phylum_authors)) {
  
  # make combinations of 2 from the whole first line (after ignoring NAs)
  if(!is.na(influenza_taxonomy_1997_phylum_authors[i,2]) == TRUE)
  {
    tmp <- t(combn(influenza_taxonomy_1997_phylum_authors[i,!is.na(influenza_taxonomy_1997_phylum_authors[i,])], 2)) 
  }
  else {
    tmp = influenza_taxonomy_1997_phylum_authors[i,]
  }
  tmp.df <- data.frame(Auth1 = unlist(tmp[, 1]), Auth2 = unlist(tmp[, 2]), stringsAsFactors = FALSE)
  tmp.df$class = influenza_taxonomy_1997_phylum_class[i,]
  df <- rbind(df, tmp.df, stringsAsFactors = FALSE)
}
str(df)
View(df)

#creating a final dataframe to use for network analysis
df2 <- data.frame(Auth1 = as.character(df$Auth1), Auth2 = as.character(df$Auth2), class = df$class, stringsAsFactors = FALSE)

rm(g)
#create a igraoh object to perform further network analysis
g <- graph.data.frame(df2, directed=TRUE)  

#Computing network parameters
#vertex count
vcount(g)

#edge count
ecount(g)

#density
edge_density(g, loops=TRUE) 

#centralization
#number of submissions
n_sub = nrow(influenza_taxonomy_1997_phylum)

#number of authors
n_auth = length(unique(c(df2$Auth1, df2$Auth2)))

#degree centralization
centr_degree(g,  loops = TRUE)

#betweenness centralization
centr_betw(g)

#executing 'g' shows us that class is on the edges
g

table(E(g)$class)
E(g)$color <- ifelse(E(g)$class == "Mononegavirales", "red","blue")

coordsFR <- layout.fruchterman.reingold(g, dim=3)

#setting seed to control randomness in plots while drilling down the stages
set.seed(42)

#plotting the network of collboration
plot.igraph(g, edge.arrow.size=0.1, vertex.color = "black", edge.arrow.width=.5,vertex.size = 2,main="Collaboration network: GenBank submissions", vertex.label=NA, margin = -0.2)
mtext( paste0("Number of Submissions " ,n_sub ,"| Number of authors " ,n_auth, "| Vertex Count " ,vcount(g),"| Edge Count " ,ecount(g)  ) , side = 1, line = 3)
rglplot(g,vertex.size = 4,vertex.label = NA,layout = coordsFR, margin =3)


# level 3: class level 
table(influenza_taxonomy_1997_phylum$class)
influenza_taxonomy_1997_phylum_class = subset(influenza_taxonomy_1997_phylum, class = "Orthomyxoviridae")

authors = influenza_taxonomy_1997_phylum_class$authors
order = influenza_taxonomy_1997_phylum_class$order

influenza_taxonomy_1997_phylum_class_authors = data.frame(authors, stringsAsFactors = FALSE) 
str(influenza_taxonomy_1997_phylum_class_authors)
influenza_taxonomy_1997_phylum_class_order = data.frame(order, stringsAsFactors = FALSE)
str(influenza_taxonomy_1997_phylum_class_order)

#Separating author names into one name per column for further computational ease
influenza_taxonomy_1997_phylum_class_authors$authors = gsub("\\s+", " ", str_trim(influenza_taxonomy_1997_phylum_class_authors$authors))
influenza_taxonomy_1997_phylum_class_authors$authors = gsub(" and ", ", ", influenza_taxonomy_1997_phylum_class_authors$authors)
influenza_taxonomy_1997_phylum_class_authors = cSplit(influenza_taxonomy_1997_phylum_class_authors, "authors","., ", direction = "wide", type.convert = FALSE)
str(influenza_taxonomy_1997_phylum_authors)
View(influenza_taxonomy_1997_phylum_class_authors)

#setting intermediate variables to null
df = NULL
tmp = NULL
tmp.df = NULL

#Creating a dataframe with 2 authors per row from the above dataset until all the authors (of ech row from thee previos dataset) are grouped 
#make an empty dataframe
df <- data.frame(Auth1 = character(), Auth2 = character(), order =character(), stringsAsFactors = FALSE)

influenza_taxonomy_1997_phylum_class_authors = data.frame(influenza_taxonomy_1997_phylum_class_authors, stringsAsFactors = FALSE)

#for each row, make the full list of pairs
for (i in 1:nrow(influenza_taxonomy_1997_phylum_class_authors)) {
  # make combinations of 2 from the whole first line (after ignoring NAs)
  if(!is.na(influenza_taxonomy_1997_phylum_class_authors[i,2]) == TRUE)
  {
    tmp <- t(combn(influenza_taxonomy_1997_phylum_class_authors[i,!is.na(influenza_taxonomy_1997_phylum_class_authors[i,])], 2)) 
  }
  else {
    tmp = influenza_taxonomy_1997_phylum_class_authors[i,]
  }
  tmp.df <- data.frame(Auth1 = unlist(tmp[, 1]), Auth2 = unlist(tmp[, 2]), stringsAsFactors = FALSE)
  tmp.df$order = influenza_taxonomy_1997_phylum_class_order[i,]
  df <- rbind(df, tmp.df, stringsAsFactors = FALSE)
}
str(df)
View(df)

#creating a final dataframe to use for network analysis
df2 <- data.frame(Auth1 = as.character(df$Auth1), Auth2 = as.character(df$Auth2), order = df$order, stringsAsFactors = FALSE)

#create a igraoh object to perform further network analysis
g <- graph.data.frame(df2, directed=TRUE)  

#Computing network parameters
#vertex count
vcount(g)

#edge count
ecount(g)

#density
edge_density(g, loops=TRUE) 

#centralization
#number of submissions
n_sub = nrow(influenza_taxonomy_1997_phylum_class)

#number of authors
n_auth = length(unique(c(df2$Auth1, df2$Auth2)))

#degree centralization
centr_degree(g,  loops = TRUE)

#betweenness centralization
centr_betw(g)

#executing 'g' shows us that order is on the edges
g

table(E(g)$order)
E(g)$color <- ifelse(E(g)$order == "Mononegavirales", "red",ifelse(E(g)$order == "Paramyxoviridae", "blue", "green"))
coordsFR <- layout.fruchterman.reingold(g, dim=3)

#setting seed to control randomness in plots while drilling down the stages
set.seed(42)

#plotting the network of collboration
plot.igraph(g, edge.arrow.size=0.1, edge.arrow.width=.5,vertex.size = 1,vertex.color = "black",main="Collaboration network: GenBank submissions", vertex.label=NA, margin = -0.19)
mtext( paste0("Number of Submissions " ,n_sub ,"| Number of authors " ,n_auth, "| Vertex Count " ,vcount(g),"| Edge Count " ,ecount(g)  ) , side = 1, line = 3)
rglplot(g,vertex.size = 4,vertex.label = NA,layout = coordsFR,margin = 3, asp= -1)


# level 4: order level 
table(influenza_taxonomy_1997_phylum_class$order)
influenza_taxonomy_1997_phylum_class_order = subset(influenza_taxonomy_1997_phylum_class, order = "ssRNA negative-strand viruses")
  
authors = influenza_taxonomy_1997_phylum_class_order$authors
family = influenza_taxonomy_1997_phylum_class_order$family
  
influenza_taxonomy_1997_phylum_class_order_authors = data.frame(authors, stringsAsFactors = FALSE) 
str(influenza_taxonomy_1997_phylum_class_order_authors)
influenza_taxonomy_1997_phylum_class_order_family = data.frame(family, stringsAsFactors = FALSE)
str(influenza_taxonomy_1997_phylum_class_order_family)
  
#Separating author names into one name per column for further computational ease
influenza_taxonomy_1997_phylum_class_order_authors$authors = gsub("\\s+", " ", str_trim(influenza_taxonomy_1997_phylum_class_order_authors$authors))
influenza_taxonomy_1997_phylum_class_order_authors$authors = gsub(" and ", ", ", influenza_taxonomy_1997_phylum_class_order_authors$authors)
influenza_taxonomy_1997_phylum_class_order_authors = cSplit(influenza_taxonomy_1997_phylum_class_order_authors, "authors","., ", direction = "wide", type.convert = FALSE)
str(influenza_taxonomy_1997_phylum_class_order_authors)
View(influenza_taxonomy_1997_phylum_class_order_authors)
  
  
#setting intermediate variables to null
df = NULL
tmp = NULL
tmp.df = NULL

#Creating a dataframe with 2 authors per row from the above dataset until all the authors (of ech row from thee previos dataset) are grouped 
#make an empty dataframe
df <- data.frame(Auth1 = character(), Auth2 = character(), family =character(), stringsAsFactors = FALSE)
influenza_taxonomy_1997_phylum_class_order_authors = data.frame(influenza_taxonomy_1997_phylum_class_order_authors, stringsAsFactors = FALSE)

#for each row, make the full list of pairs
for (i in 1:nrow(influenza_taxonomy_1997_phylum_class_order_authors)) {
  # make combinations of 2 from the whole first line (after ignoring NAs)
  if(!is.na(influenza_taxonomy_1997_phylum_class_order_authors[i,2]) == TRUE)
  {
    tmp <- t(combn(influenza_taxonomy_1997_phylum_class_order_authors[i,!is.na(influenza_taxonomy_1997_phylum_class_order_authors[i,])], 2)) 
  }
  else {
    tmp = influenza_taxonomy_1997_phylum_class_order_authors[i,]
  }
  tmp.df <- data.frame(Auth1 = unlist(tmp[, 1]), Auth2 = unlist(tmp[, 2]), stringsAsFactors = FALSE)
  tmp.df$family = influenza_taxonomy_1997_phylum_class_order_family[i,]
  df <- rbind(df, tmp.df, stringsAsFactors = FALSE)
}

str(df)
View(df)

#creating a final dataframe to use for network analysis

df2 <- data.frame(Auth1 = as.character(df$Auth1), Auth2 = as.character(df$Auth2), family = df$family, stringsAsFactors = FALSE)

#create a igraoh object to perform further network analysis
g <- graph.data.frame(df2, directed=TRUE) 

#Computing network parameters

#vertex count
vcount(g)

#edge count
ecount(g)

#density
edge_density(g, loops=TRUE) 

#centralization

#number of submissions
n_sub = nrow(influenza_taxonomy_1997_phylum_class_order)

#number of authors
n_auth = length(unique(c(df2$Auth1, df2$Auth2)))

#degree centralization
centr_degree(g,  loops = TRUE)

#betweenness centralization
centr_betw(g)

#executing 'g' shows us that famimly is on the edges
g


table(E(g)$family)

E(g)$color <- ifelse(E(g)$family == "Orthomyxoviridae", "red",ifelse(E(g)$family == "Paramyxoviridae", "blue", "green"))
coordsFR <- layout.fruchterman.reingold(g, dim=3)

#setting seed to control randomness in plots while drilling down the stages
set.seed(42)

plot.igraph(g, edge.arrow.size=0.1, edge.arrow.width=.5,vertex.size = 2,vertex.color = "black",main="Collaboration network: GenBank submissions", vertex.label=NA, margin = -0.2)
mtext( paste0("Number of Submissions " ,n_sub ,"| Number of authors " ,n_auth, "| Vertex Count " ,vcount(g),"| Edge Count " ,ecount(g)  ) , side = 1, line = 3)
rglplot(g,vertex.size = 4,vertex.label = NA,layout = coordsFR, margin = 3, asp= -1)


# level 5: family level 
table(influenza_taxonomy_1997_phylum_class_order$family)
influenza_taxonomy_1997_phylum_class_order_family = subset(influenza_taxonomy_1997_phylum_class_order, family = "Orthomyxoviridae")

authors = influenza_taxonomy_1997_phylum_class_order_family$authors
genus = influenza_taxonomy_1997_phylum_class_order_family$genus

influenza_taxonomy_1997_phylum_class_order_family_authors = data.frame(authors, stringsAsFactors = FALSE) 
str(influenza_taxonomy_1997_phylum_class_order_family_authors)
influenza_taxonomy_1997_phylum_class_order_family_genus = data.frame(genus, stringsAsFactors = FALSE)
str(influenza_taxonomy_1997_phylum_class_order_family_genus)

#Separating author names into one name per column for further computational ease
influenza_taxonomy_1997_phylum_class_order_family_authors$authors = gsub("\\s+", " ", str_trim(influenza_taxonomy_1997_phylum_class_order_family_authors$authors))
influenza_taxonomy_1997_phylum_class_order_family_authors$authors = gsub(" and ", ", ", influenza_taxonomy_1997_phylum_class_order_family_authors$authors)
influenza_taxonomy_1997_phylum_class_order_family_authors = cSplit(influenza_taxonomy_1997_phylum_class_order_family_authors, "authors","., ", direction = "wide", type.convert = FALSE)
str(influenza_taxonomy_1997_phylum_class_order_family_authors)
View(influenza_taxonomy_1997_phylum_class_order_family_authors)


#setting intermediate variables to null
df = NULL
tmp = NULL
tmp.df = NULL

#Creating a dataframe with 2 authors per row from the above dataset until all the authors (of ech row from thee previos dataset) are grouped 
#make an empty dataframe
df <- data.frame(Auth1 = character(), Auth2 = character(), genus =character(), stringsAsFactors = FALSE)
influenza_taxonomy_1997_phylum_class_order_family_authors = data.frame(influenza_taxonomy_1997_phylum_class_order_family_authors, stringsAsFactors = FALSE)

#for each row, make the full list of pairs
for (i in 1:nrow(influenza_taxonomy_1997_phylum_class_order_family_authors)) {
  # make combinations of 2 from the whole first line (after ignoring NAs)
  if(!is.na(influenza_taxonomy_1997_phylum_class_order_family_authors[i,2]) == TRUE)
  {
    tmp <- t(combn(influenza_taxonomy_1997_phylum_class_order_family_authors[i,!is.na(influenza_taxonomy_1997_phylum_class_order_family_authors[i,])], 2)) 
  }
  else {
    tmp = influenza_taxonomy_1997_phylum_class_order_family_authors[i,]
  }
  tmp.df <- data.frame(Auth1 = unlist(tmp[, 1]), Auth2 = unlist(tmp[, 2]), stringsAsFactors = FALSE)
  tmp.df$genus = influenza_taxonomy_1997_phylum_class_order_family_genus[i,]
  df <- rbind(df, tmp.df, stringsAsFactors = FALSE)
}
str(df)
View(df)

#creating a final dataframe to use for network analysis
df2 <- data.frame(Auth1 = as.character(df$Auth1), Auth2 = as.character(df$Auth2), genus = df$genus, stringsAsFactors = FALSE)

#create a igraph object to perform further network analysis
g <- graph.data.frame(df2, directed=TRUE)  

#Computing network parameters

#vertex count
vcount(g)

#edge count
ecount(g)

#density
edge_density(g, loops=TRUE) 

#centralization

#number of submissions
n_sub = nrow(influenza_taxonomy_1997_phylum_class_order_family)

#number of authors
n_auth = length(unique(c(df2$Auth1, df2$Auth2)))

#degree centralization
centr_degree(g,  loops = TRUE)

#betweenness centralization
centr_betw(g)

#executing 'g' shows us that genus is on the edges
g


table(E(g)$genus)
  E(g)$color <- ifelse(E(g)$genus == "Influenzavirus A", "red",ifelse(E(g)$genus == "Influenzavirus B", "blue", ifelse(E(g)$genus == "Influenzavirus C","green",ifelse(E(g)$genus == "Paramyxovirinae","brown", ifelse(E(g)$genus == "Respirovirus", "purple", "yellow")))))

coordsFR <- layout.fruchterman.reingold(g, dim=3)

#setting seed to control randomness in plots while drilling down the stages
set.seed(42)

plot.igraph(g, edge.arrow.size=0.1, edge.arrow.width=.5,vertex.size = 2,vertex.color = "black",main="Collaboration network: GenBank submissions", vertex.label=NA, margin = -.001)
mtext( paste0("Number of Submissions " ,n_sub ,"| Number of authors " ,n_auth, "| Vertex Count " ,vcount(g),"| Edge Count " ,ecount(g)  ) , side = 1, line = 3)
rglplot(g,vertex.size = 4,vertex.label = NA,layout = coordsFR,margin = 3, asp= -1)  


