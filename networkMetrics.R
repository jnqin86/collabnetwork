n=20

network.metric <- data.frame(
  
  year = rep(0, n)
  ,submission = rep(0, n)
  ,publication = rep(0, n)
  ,nodeNum = rep(0, n)
  ,edgeNum = rep(0, n)
  ,no.cluster = rep(0, n)
  ,density = rep(0, n)
  ,node.giant = rep(0, n)
  ,edge.giant = rep(0, n)
  ,size.giant = rep(0, n)
  ,meandegree = rep(0, n)
  ,weighted = rep(0, n)
  ,unweighted = rep(0, n)
  ,cluster.coeffient = rep(0, n)
  ,assortativity = rep(0, n)

)

for (i in 1994:2012){
  #####################convert data frame to network###################
  sub <- subset(reduceSubNetwork,year==i)####file: ReducedSubNetwork
  submission <- length(unique(sub$referenceId))
  pub <- subset(reducePubNetwork,year==i)####file: ReducedPubNetwork
  publication <- length(unique(pub$id))
  #########################################
  nam <- subset(reducedNetwork,year==i)
  test <- graph.data.frame(nam,directed=FALSE)
  ######################calculate the giant components#####################
  comp <- clusters(test)
  subg <- induced.subgraph(test, which(comp$membership == which.max(comp$csize)))
  #########################record the parameters######################
  nodeNum <- vcount(test)
  edgeNum <- ecount(test)
  fit1 <- power.law.fit(edgeNum+1)
  meandegree <- round(edgeNum/nodeNum,digits=2)
  year <- i
  layout <- 'drl'
  no.cluster <- no.clusters(test)
  simplified <- simplify(test)
  density <-round( graph.density(test,loops=FALSE),digit=6)
  ############################parameters about giant component#####
  node.giant <- vcount(subg)
  edge.giant <- ecount(subg)
  size.giant <- percent(node.giant/nodeNum)
  #diameter<-diameter(subg)
  cluster.coeffient <- percent(transitivity(subg))
  assortativity <- percent(assortativity.degree(subg))
  unweighted <- max(degree(subg))
  namnet <- simplify(subg)
  weighted <- max(degree(namnet))
  unweighted <- max(degree(subg))
  
  network.metric[i-1993,] <- c(year,submission,publication,nodeNum,edgeNum,no.cluster,density,
             node.giant, edge.giant,size.giant,meandegree,weighted,unweighted,
             cluster.coeffient,assortativity)
}