V(sw)$collabcapacity <- 0   
V(sw)$degree_outside <- 0
V(sw)$degree_within <- 0

community_no <- length(unique(V(sw)$community)) # Number of steps to take in loop is the number of communities.
#collaboration capacity add to vertices in whole graph

for (i in 1:community_no){
  #i = 1
  subg <- induced_subgraph(sw, which(V(sw)$community == i))
  V(subg)$degree_within <- degree(subg) # degree inside function.
  total_degree <- V(subg)$degree
  within <- V(subg)$degree_within
  V(subg)$degree_outside <- total_degree - within
  
  # V(subg)$collab_capacity<- round((V(subg)$degree_outside - V(subg)$degree_within)/total_degree, 2) 
  
  V(sw)$collabcapacity[V(sw)$community == i] <- round((V(subg)$degree_outside - V(subg)$degree_within)/total_degree, 2) 
  #V(sw)$cc_2 <- collaboration + degree + 2(eigenalue). 
  
  V(sw)$degree_within[V(sw)$community == i] <-  V(subg)$degree_within
  V(sw)$degree_outside[V(sw)$community == i] <- V(subg)$degree_outside
  
  # nam <- paste("subg", i, sep = "")
  # assign(nam, subg)
}

plot(V(sw)$degree_within, V(sw)$degree_outside, cex = 1 + 4 * abs(V(sw)$collabcapacity), ylim = c(0,4), type = "n")
text(jitter(V(sw)$degree_within), jitter(V(sw)$degree_outside), labels = V(sw)$collabcapacity)

##########################################################################################################################

# GenBank Network Data: Elbow subset for years 1993 - 1997 (from Amit, 2/13/2020)

setwd("C:/Users/sarah/Downloads")
LL_93 <- read.csv("AuthorSubsetLinkedlist1993.csv")
g <- graph.data.frame(LL_93, directed=FALSE)  
dim(g)

plot(g, vertex.label= NA, edge.arrow.size=0.02,vertex.size = 0.5, xlab = "Publication Linked List, Elbow subset")














