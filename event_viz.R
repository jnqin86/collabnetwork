install.packages('igraph')
library(igraph)

install.packages('scales')
library(scales)

load("~/event/forViz/event.Rda")

load("~/reducedNetwork/ReducedNetwork.Rda")

net.df <- merge(reducedNetwork,event.df,by=c('id','year'),all.x = TRUE,all.y = FALSE)

net <- net.df[,c(3,4,1,2,5,6)]

net[is.na(net)] <- 0

auth <- subset(net,net$event!=0)
data1 <- auth[,c(1,3,4,5,6)]
data2 <- auth[,c(2,3,4,5,6)]
colnames(data1) <- c('autor','id','year','event','type')
colnames(data2) <- c('autor','id','year','event','type')

author <- unique(rbind(data1,data2))

authorId <- unique(author$autor)

save(author,file='event/forViz/author.Rda')


load("~/reducedNetwork/reduce_pub_sub_layout.Rda")
load("~/reducedNetwork/nodeSize.Rda")

 for (i in 1998:2005){
  ##############################################
  #####################convert data frame to network###################

  i=2000
  #########################################
  nam <- subset(net,year==i)
  test <- graph.data.frame(nam,directed=FALSE)
  
  
  
  
  ######################calculate the giant components#####################
  comp <- clusters(test)
  subg <- induced.subgraph(test, which(comp$membership == which.max(comp$csize)))
  #################set attributes for nodes and edges##################
  V(test)$label <- NA  #define default lable
  #all(is.character(V(test)$name))
  
  ########vertice size#####
  #nodeSize$size1 <- round(nodeSize$count*1.75/19,digit=2)
  #nodeSize$size2 <- round(nodeSize$count*0.75/19,digit=2)
  #save(nodeSize,file='reducedNetwork/nodeSize.Rda')
  size.position <- match(V(test)$name,nodeSize$auth)
  size.value <- nodeSize$size2[size.position]
  V(test)$size <- size.value
  
  #V(test)$size <- 0.25
  #set.vertex.attribute(test, "size",index=V(test), value=size.value)
  
  E(test)$width <- 0.8
  E(test)$color <- rgb(255,255,255,alpha = 0,maxColorValue = 255)
  set.edge.attribute(test, "type",index=E(test), value=nam$type)###publication&sub
  set.edge.attribute(test, "event",index=E(test), value=nam$event)###publication&sub
  #########set attribute(module) for vertice
  V(test)$frame <- rgb(0,0,0,alpha = 10,maxColorValue = 255)
  V(test)$color <- rgb(173,216,230,alpha = 20,maxColorValue = 255)
  author.sub <- subset(author,year==i)
  authorid <- unique(author.sub$autor)
  pos.node.color <- match(authorid,V(test)$name)
  V(test)[pos.node.color]$color <- rgb(255,105,180,alpha = 200,maxColorValue = 255)
  #V(test)[pos.node.color]$frame <- rgb(0,0,0,alpha = 200,maxColorValue = 255)

  
  #####################################assign edges colors###################################
  #E(test)[type=='publication']$color='brown1'
  E(test)[type=='publication' & event == 'West Nile']$color=rgb(139,0,0,alpha = 200,maxColorValue = 255)
  E(test)[type=='submission' & event == 'West Nile']$color=rgb(255,0,0,alpha = 200,maxColorValue = 255)
  
  E(test)[type=='publication' & event == 'H1N1']$color=rgb(84,139,84,alpha = 200,maxColorValue = 255)
  E(test)[type=='submission' & event == 'H1N1']$color=rgb(152,251,152,alpha = 200,maxColorValue = 255)
  
  E(test)[type=='publication' & event == 'SARS']$color=rgb(93,71,139,alpha = 200,maxColorValue = 255)
  E(test)[type=='submission' & event == 'SARS']$color=rgb(171,130,255,alpha = 200,maxColorValue = 255)
  
  ########################set the layout#############
  #seed <- matrix(c(V(test)$x, V(test)$y), ncol = 2, byrow=F)
  #time.layout<-system.time(l <- layout.drl(test, options=igraph.drl.final))
  #V(test)$x <- l[,1]
  #V(test)$y <- l[,2]
  location<-match(V(test)$name,subg_layout$authorID)
  interim2<-subg_layout[location,]
  V(test)$x<-interim2$x
  V(test)$y<-interim2$y
  #str(interim2)
  #all(is.na(V(test)$x))
  #all(is.na(V(test)$y))
  #all(is.na(V(test)$size))
  
  #V(test)[is.na(V(test)$size)]
  
  ##################################add west nile ###############################
  
  
  
  ########delete isolated nodes##################
  missingNode<-which(is.na(V(test)$x))
  
  #missingNode.check<-which(!(V(test)$name%in%subg_layout$authorID))
  
  #missingNode==missingNode.check
  
  missingId<-V(test)$name[missingNode]
  
  missingId.int<-as.numeric(missingId)
  
  new.graph<-delete.vertices(test,missingId)
  
  #new.graph1<-delete.edges(new.graph,E(test)[event=='0'])
  #########################record the parameters######################
  nodeNum <- vcount(test)
  edgeNum <- ecount(test)
  node.allGC <- vcount(new.graph)
  fit1 <- power.law.fit(edgeNum+1)
  
  year <- i
  layout <- 'drl'
  no.cluster <- no.clusters(test)
  density <- graph.density(test,loops=FALSE)
  #####################SARS
  pub.sars <- subset(nam,type=='publication' & event=='SARS')
  pub.sars.no <- length(unique(pub.sars$id))
  
  sub.sars <- subset(nam,type=='submission' & event=='SARS')
  sub.sars.no <- length(unique(sub.sars$id))
  
  #####################H1N1
  pub.hn <- subset(nam,type=='publication' & event=='H1N1')
  pub.hn.no <- length(unique(pub.hn$id))
  
  sub.hn <- subset(nam,type=='submission' & event=='H1N1')
  sub.hn.no <- length(unique(sub.hn$id))
  
  #####################SARS
  pub.west <- subset(nam,type=='publication' & event=='West Nile')
  pub.west.no <- length(unique(pub.west$id))
  
  sub.west <- subset(nam,type=='submission' & event=='West Nile')
  sub.west.no <- length(unique(sub.west$id))
  
  #####community analysis
  filename_read <- paste("community/all_chunk",i,".csv",sep="")#change filename
  module_1996 <- read.table(filename_read,head=TRUE,sep=',')
  #module_1996 <- read.table('visualization/all_chunk96.csv',head=TRUE,sep=',')
  module_viz_1996 <- as.data.frame(cbind(module_1996[,6],module_1996[,3]))
  colnames(module_viz_1996) <- c('authorId','module')
  module_viz_1996$authorId <- as.character(module_viz_1996$authorId)
  community <- as.data.frame(table(module_viz_1996$module))
  colnames(community) <- c('Module','Freq')
  
  #######################plot the network#######################
  filename<-paste("visualization/event/",i,"event_disambig_NetworkLegend.png",sep="")#change filename
  png(file=filename,width=13660, height=7680, pointsize = 180)
  #png(file=filename,width=27320, height=15360, pointsize = 240)
  nf <- layout(matrix(c(2,2,1,1,1,1,1,3,3,
                        2,2,1,1,1,1,1,3,3,
                        2,2,1,1,1,1,1,4,4,
                        2,2,1,1,1,1,1,4,4,
                        2,2,1,1,1,1,1,5,5), 5, 9, byrow = TRUE), respect = FALSE)
  #layout.show(nf)
  ######plot the network
  par(mar=c(1,1,3,1))
  time.plot<-system.time(plot(new.graph,vertex.frame.color=V(test)$frame))
  #plot(test)
  #mtext((paste("Collaboration Network in", year,"\n (Submissions from GenBank)")), side = 3,line=-1,cex=18)
  #mtext((paste("Collaboration Network in", year,"\n (Submissions from GenBank)")), side = 3,line=-10,cex=18)
  #mtext((paste("Collaboration Network in", year,"\n (Submissions from GenBank)")), side = 3,line=-20,cex=18)
  #mtext((paste("Collaboration Network in", year,"\n (Submissions from GenBank1)")), side = 3,line=10,cex=18,at=1)
  #mtext((paste("Collaboration Network in", year,"\n (Submissions from GenBank5)")), side = 3,line=10,cex=18)
  #mtext((paste("Collaboration Network in", year,"\n (Submissions from GenBank10)")), side = 3,line=50,cex=18)
  mtext((paste("Figure 1:Collaboration Network in", year,"\n (Submissions from GenBank)")), side = 3,line=-1)
  legend("bottomleft",c('module1','other modules'),lty=c(1,1),lwd=c(15,15),col=c('hotpink','lightblue'), title="Nodes:Module Distribution") 
  legend("bottomright",c('West Nile: Publication','West Nile: Submission','H1N1 Publication','H1N1 Submission','SARS Publication','SARS Submission'),
         lty=c(1,1,1,1,1,1),lwd=c(15,15,15,15,15,15),col=c(rgb(139,0,0,alpha = 200,maxColorValue = 255),rgb(255,0,0,alpha = 200,maxColorValue = 255),
                                                           rgb(84,139,84,alpha = 200,maxColorValue = 255),rgb(152,251,152,alpha = 200,maxColorValue = 255),
                                                           rgb(93,71,139,alpha = 200,maxColorValue = 255),rgb(171,130,255,alpha = 200,maxColorValue = 255)), title="Edges:West Nile, H1N1, SARS") 
  
  ####################### plot 1: add text about network information################ 
  par(mar=c(1,1,1,1))
  plot(x=1:35,y=1:35,type="n",xaxt='n',yaxt='n',xlab='',ylab='',axes=FALSE)
  ###add general information
  text(x=17,y=35,paste("Year:", year),col='grey0')
  text(x=17,y=34,paste("Output File Name:", year,"disambig_Network.png"),col='grey0')
  
  ####add entire network
  text(x=17,y=30,paste("Nodes Number in ",year,':', nodeNum),col='grey20')
  text(x=17,y=29,paste("Edges Number in ",year,':', edgeNum),col='grey20')
  #text(x=12,y=18,paste("Diameter:", diameter),col='grey20')
  text(x=17,y=28,paste("Nodes Number in all years' GC", node.allGC),col='grey20')
  text(x=17,y=27,paste("Number of Components:", no.cluster),col='grey20')
  #text(x=17,y=26,paste("Number of Nodes in Module1:", NumM1),col='grey20')
  #####################add giant component
  text(x=17,y=23,paste("Number of publications in SARS: ", pub.sars.no),col='grey30')
  text(x=17,y=22,paste("Number of submissions in SARS: ", sub.sars.no),col='grey30')
  
  text(x=17,y=21,paste("Number of publications in H1N1: ", pub.hn.no),col='grey30')
  text(x=17,y=20,paste("Number of submissions in H1N1: ", sub.hn.no),col='grey30')
  #text(x=12,y=12,paste("Assortativity in GC:", assortativity),col='grey30')
  
  
  text(x=17,y=19,paste("Number of publications in West Nile: ", pub.west.no),col='grey30')
  text(x=17,y=18,paste("Number of submissions in West Nile: ", sub.west.no),col='grey30')
  
  ###########plot the third picture
  ####   par(mar=c(30,20,30,20)+0.5)
  par(mar=c(4,4,4,4))
  plot(x=log_NumDegree,y=log_NumAuthor,main=paste('Figure 2: Degree Distribution(log) \n for Submission Network in',year),
       xlab='Number of Degree (log)',ylab='Number of Authors/Frequency (log)',col='grey35',pch=20)
  abline(lm(log_NumAuthor ~ log_NumDegree),col='red', lwd = 20, xpd = FALSE)
  
  #############plot the forth picture##############################
  #par(mar=c(30,20,30,20)+0.5)
  #par(mar=c(30,20,30,20)+0.5)
  par(mar=c(4,4,4,4))
  plot(x=community$Module,y=community$Freq,main='Figure 3: Number of Authors \nin per Module',xlab='Modules',ylab='Number of Authors in Module', xaxt = 'n')
  axis(1)
  lines(x=community$Module,y=community$Freq,lwd=3,col='firebrick4')
  #mtext('Number of Authors in Module',side=2,cex=10,line=20)
  #mtext('Modules1',side=1,cex=10,at=20,line=30)
  #mtext('Modules2',side=1,cex=10,at=40,line=30)
  #mtext('Modules',side=1,cex=10,at=1000,line=30)
  #mtext('Modules4',side=1,cex=10,at=-20,line=30)
  
  ############################################################plot picture 4
  #par(mar=c(10,10,10,10))
  par(mar=c(1,10,1,10))
  plot(x=1:10,y=1:10,type="n",xaxt='n',yaxt='n',xlab='',ylab='',axes=FALSE)
  
  text(x=5,y=10,paste("Figure 2: \nshows the Degree Distribution in log scale"))
  text(x=6,y=8,paste("X is calculated by log(Number of Degrees)"))
  text(x=6,y=7,paste("Y is calculated by log(Number of Authors)"))
  text(x=6,y=6,paste("Generally, it presents a linear relation."))
  text(x=5,y=4,paste("Figure 3: \nshows number of authors in every modules"))
  text(x=6,y=2,paste("X is Module Id"))
  text(x=6,y=1,paste("Y is the number of members in this module"))
  dev.off()
  
}
