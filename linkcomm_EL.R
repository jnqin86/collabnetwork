karate <- make_graph("Zachary")
wc <- cluster_walktrap(karate, membership = TRUE)
modularity(wc)
membership(wc)
plot(wc, karate)
?membership
communities(wc)
?cluster_walktrap
crossing()

install.packages("linkcomm")
library(linkcomm)
demo(topic = "linkcomm", package = "linkcomm")

install.packages("dplyr")
library(dplyr)

g <- swiss[,3:4]
dim(g)

g <- swiss[,3:4]
g.dup <- edge.duplicates(g)


# Source: LinkedComm Cran. https://cran.r-project.org/web/packages/linkcomm/linkcomm.pdf

setwd("C:/Users/sarah/Downloads")
LL_93 <- read.csv("AuthorSubsetLinkedlist1993.csv")

#g <- graph.data.frame(LL_93, directed=FALSE)  
#plot(g, vertex.label= NA, edge.arrow.size=0.02,vertex.size = 0.5, xlab = "Publication Linked List, Elbow subset")
LL_93 <- read.csv("AuthorSubsetLinkedlist1993.csv")

g <- LL_93
?edge.duplicates
dim(g)
g <- distinct(g)
dim(g)

g <- g[1:100,]
lc <- getLinkCommunities(g)
head(g)
plot(lc, type = "members")
plot(lc, type = "graph")
getNestedHierarchies(lc, clusid = 1, plot = TRUE)
plot(lc, type = "graph", layout="spencer.circle")
plot(lc, type = "commsumm", summary = "mod")

# Source (for below): https://cran.r-project.org/web/packages/linkcomm/vignettes/linkcomm.pdf

cr <- getClusterRelatedness(lc, hcmethod = "ward")

# We can defined the communities through the cutat point.  
lc2 <- newLinkCommsAt(lc, cutat = 0.85) # cut higher up the dendrogram, for larger, fewer communities. 
lc2 <- newLinkCommsAt(lc, cutat = 0.4) # cut lower for more communities. 
print(lc2)
plot(lc2, type = "members")

getNodesIn(lc2, clusterids = c(4,5))
get.shared.nodes(lc2, comms = c(4,5))




