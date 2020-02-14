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
sw <- watts.strogatz.game(1, 100, 1, 0.35, loops = FALSE, multiple = FALSE) # 100 nodes, with n = probability of linking, no loops.
plot(sw, vertex.label= NA, edge.arrow.size=0.02,vertex.size = 0.5, xlab = "Small world model") 

#install.packages("linkcomm")
library(linkcomm)
#demo(topic = "linkcomm", package = "linkcomm")
g <- as_edgelist(sw, names = TRUE)
lc <- getLinkCommunities(g)
head(g)

# We can defined the communities through the cutat point.  
lc2 <- newLinkCommsAt(lc, cutat = 0.99) # cut higher up the dendrogram, for larger, fewer communities. 
#lc2 <- newLinkCommsAt(lc, cutat = 0.6) # cut lower for more communities. 
print(lc2)
plot(lc2, type = "members")

#getNodesIn(lc2, clusterids = c(4,5))
#get.shared.nodes(lc2, comms = c(4,5))
#lc2$nodeclusters

plot(lc2, type = "members")
plot(lc2, type = "graph")
getNestedHierarchies(lc, clusid = 1, plot = TRUE)
plot(lc2, type = "graph", layout="spencer.circle")
plot(lc2, type = "commsumm", summary = "mod")

cr <- getClusterRelatedness(lc, hcmethod = "ward")
# Source: https://cran.r-project.org/web/packages/linkcomm/vignettes/linkcomm.pdf


# add node characteristics, or "attributes"
cfg <- cluster_fast_greedy(as.undirected(sw))
V(sw)$id <-V(sw)
V(sw)$bridge_node <- crossing(cfg, sw)  # nodes that connects two or more communities. Bridge nodes, sort of.
V(sw)$community <- cfg$membership
V(sw)$clusters <- lc2$nodeclusters # give the vertices attributes of community 
V(sw)$degree <- degree(sw) # give the vertices attributes of degree (count)
V(sw)$egv_cent <- round(as.numeric(unlist(eigen_centrality(sw)$vector)),2) # centrality scores

plot(unlist(V(sw)$egv_cent) ~ V(sw)$degree, type = "n") # HELPP MEEEE!
text(jitter(as.numeric((sw)$egv_cent)), jitter(V(sw)$degree), labels = V(sw)$egv_cent)
vertex_attr_names(sw)

#Compute eigenvector centrality scores
eigen_centrality(sw)$vector #	A vector containing the centrality scores.
eigen_centrality(sw)$value # The eigenvalue corresponding to the calculated eigenvector, i.e. the centrality scores.

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

getwd()
