library(igraph)


### Small world model (Watts-Strogatz). Similar to GenBank Network (we think)
# The small world phenomenon also known as six degrees of separation. 
#Two individual people on Earth are just six people "distance" from each other. 
#(Was known earlier Investigated facebook data it is just 4.74)
#Small world model play with dimensionality of lattice (like a 2D, 3D lattice). 
#There is an initial lattice structure of nodes with links to its k closest neighbors. 
#The next proporties of rewiring probability that means each edge has probability p that 
#it will be rewired to the graph as a random edge.

library(igraph)
sw <- watts.strogatz.game(3, 3, 1, .2, loops = FALSE, multiple = FALSE) # 100 nodes, with n = probability of linking, no loops.
plot(sw, vertex.label= NA, edge.arrow.size=0.02,vertex.size = 0.5, xlab = "Small world model") 
sw
hist(degree(sw))
?watts.strogatz.game
#install.packages("linkcomm")
library(linkcomm)
#demo(topic = "linkcomm", package = "linkcomm")

#g <- sample_pa(200)
#g <- as_edgelist(g, names = TRUE)
g <- as_edgelist(sw, names = TRUE)
# ?igraph
lc <- getLinkCommunities(g)
plot(lc, type ="members")
#?getLinkCommunities()
head(g)

# We can defined the communities through the cutat point.  
lc2 <- newLinkCommsAt(lc, cutat = 0.97) # cut higher up the dendrogram, for larger, fewer communities. 
#lc2 <- newLinkCommsAt(lc, cutat = 0.6) # cut lower for more communities. 
print(lc2)
plot(lc2, type = "members")

#getNodesIn(lc2, clusterids = c(4,5))
#get.shared.nodes(lc2, comms = c(4,5))
#lc2$nodeclusters

par(mar = c(0,0,0,0))
plot(lc2, type = "graph")
#getNestedHierarchies(lc, clusid = 1, plot = TRUE)
plot(lc2, type = "graph", layout="spencer.circle")
plot(lc2, type = "commsumm", summary = "mod")

cr <- getClusterRelatedness(lc, hcmethod = "ward")
# Source: https://cran.r-project.org/web/packages/linkcomm/vignettes/linkcomm.pdf


# add node characteristics, or "attributes"
cfg <- cluster_fast_greedy(as.undirected(sw))
V(sw)$id <-V(sw)
# V(sw)$bridge_node <- crossing(cfg, sw)  # nodes that connects two or more communities. Bridge nodes, sort of.
V(sw)$community <- cfg$membership
V(sw)$clusters <- lc$nodeclusters # give the vertices attributes of community 
V(sw)$degree <- degree(sw) # give the vertices attributes of degree (count). HELP
V(sw)$egv_cent <- round(as.numeric(unlist(eigen_centrality(sw)$vector)),2) # centrality scores
# ?eigen_centrality
plot(unlist(V(sw)$egv_cent) ~ V(sw)$degree, type = "n") # HELP EH.
text(jitter(as.numeric((sw)$egv_cent)), jitter(V(sw)$degree), labels = V(sw)$egv_cent)
vertex_attr_names(sw)

#Compute eigenvector centrality scores
eigen_centrality(sw)$vector #	A vector containing the centrality scores.
eigen_centrality(sw)$value # The eigenvalue corresponding to the calculated eigenvector, i.e. the centrality scores.

# THE CLASSIC CC FORMULA BASED ON EO and EW 
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


plot(V(sw)$degree_within, V(sw)$degree_outside, cex = 1 + 4 * abs(V(sw)$collabcapacity), ylim = c(-1,3), xlim = c(-1, 7), type = "n")
text(jitter(V(sw)$degree_within), jitter(V(sw)$degree_outside), labels = V(sw)$egv_cent)

ew <- V(sw)$degree_within   
eo <- V(sw)$degree_outside
cc <- V(sw)$collabcapacity

size <- 1 + 2 * abs(cc)
n <- length(cc)
col <- rep("red", n)
col[cc > 0] <- "blue"
plot(eo,ew, col = col, cex = size, pch = 16)

vertex_attr_names(sw)

size <- V(sw)$egv_cent
plot(sw, vertex.label= V(sw)$egv_cent, edge.arrow.size=0.02,vertex.size = size, xlab = "Small world model") 

##########################################################################################################################

# GenBank Network Data: Elbow subset for years 1993 - 1997 (from Amit, 2/13/2020)
# HELP
setwd("C:/Users/sarah/Downloads")
LL_93 <- read.csv("AuthorSubsetLinkedlist1993.csv")
dim(LL_93)
g <- graph.data.frame(LL_93[1:5000, ], directed=FALSE)  
g <- simplify(g, remove.loops = TRUE)
class(g)
plot(g, vertex.label= NA, edge.arrow.size=0.02,vertex.size = 0.5, xlab = "Publication Linked List, Elbow subset")
g


class(LL_93[1:1000, ])
colnames(LL_93)
#g <- LL_93
#g <- as.matrix(g)
#?edge.duplicates
#dim(g)

# g <- g[3:102,]
#g <- g[0:100,]
#g <- g[which(!is.na(g)),]
#g <- graph_from_edgelist(g, directed = TRUE)
#g <- as.igraph(g, names = TRUE)
#head(g)

el <- as_edgelist(g)
class(el)
colnames(el)
el
lc <- getLinkCommunities(el)
dim(el)
plot(lc)


plot(lc, type = "members")

pdf(file = "lc_plot_graph.pdf", width = 24, height = 24)
plot(lc, type = "graph")
dev.off()


getNestedHierarchies(lc, clusid = 1, plot = TRUE)
plot(lc, type = "graph", layout="spencer.circle")
plot(lc, type = "commsumm", summary = "mod")

# Source (for below): https://cran.r-project.org/web/packages/linkcomm/vignettes/linkcomm.pdf
cr <- getClusterRelatedness(lc, hcmethod = "ward")

# DEFINE the communities through the 'cut at' point.  
lc2 <- newLinkCommsAt(lc, cutat = 0.85) # cut higher up the dendrogram, for larger, fewer communities. 
lc2 <- newLinkCommsAt(lc, cutat = 0.4) # cut lower for more communities. 
print(lc2)
plot(lc2, type = "members")

getNodesIn(lc2, clusterids = c(4,5))
get.shared.nodes(lc2, comms = c(4,5))
lc2$nodeclusters

# REAL DATA ORIGINAL CC FOrmula and FUNCTIOn

#cfg <- cluster_fast_greedy(as.undirected(g))
#V(sw)$id <-V(sw)
# V(sw)$bridge_node <- crossing(cfg, sw)  # nodes that connects two or more communities. Bridge nodes, sort of.
V(sw)$community <- cfg$membership
V(g)$clusters <- lc$nodeclusters # give the vertices attributes of community 
V(sw)$degree <- degree(sw) # give the vertices attributes of degree (count). HELP
V(sw)$egv_cent <- round(as.numeric(unlist(eigen_centrality(sw)$vector)),2) # centrality scores
# ?eigen_centrality
plot(unlist(V(sw)$egv_cent) ~ V(sw)$degree, type = "n") # HELP EH.
text(jitter(as.numeric((sw)$egv_cent)), jitter(V(sw)$degree), labels = V(sw)$egv_cent)
vertex_attr_names(sw)

#Compute eigenvector centrality scores
eigen_centrality(sw)$vector #	A vector containing the centrality scores.
eigen_centrality(sw)$value # The eigenvalue corresponding to the calculated eigenvector, i.e. the centrality scores.

# THE CLASSIC CC FORMULA BASED ON EO and EW 
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


plot(V(sw)$degree_within, V(sw)$degree_outside, cex = 1 + 4 * abs(V(sw)$collabcapacity), ylim = c(-1,3), xlim = c(-1, 7), type = "n")
text(jitter(V(sw)$degree_within), jitter(V(sw)$degree_outside), labels = V(sw)$egv_cent)

ew <- V(sw)$degree_within   
eo <- V(sw)$degree_outside
cc <- V(sw)$collabcapacity

size <- 1 + 2 * abs(cc)
n <- length(cc)
col <- rep("red", n)
col[cc > 0] <- "blue"
plot(eo,ew, col = col, cex = size, pch = 16)

vertex_attr_names(sw)

size <- V(sw)$egv_cent
plot(sw, vertex.label= V(sw)$egv_cent, edge.arrow.size=0.02,vertex.size = size, xlab = "Small world model") 

plot(g, vertex.label= V(sw)$egv_cent, edge.arrow.size=0.02,vertex.size = size, xlab = "Small world model") 

