install.packages("igraph")
library(igraph)
install.packages("plyr")
library(plyr)
#connect to mysql
con<-dbConnect(MySQL(),
               user="GenBankUser", password="",
               host="metadatalab.syr.edu", dbname="genbank")

#get data from mysql
sub<-dbSendQuery(con,"select dra1.author_id as auth1, dra2.author_id as auth2, dra1.id as id, par.year as year from disambig_Reference_Authors as dra1 inner join Reference as gr on gr.id = dra1.id inner join disambig_Reference_Authors as dra2 on gr.id = dra2.id inner join test.par_Reference_Journal as par on par.reference_id=gr.id
                 where dra1.author_id < dra2.author_id and gr.journal like 'Submitted%'")
subnet=fetch(sub,n=-1)
save(subnet,file='submittedNetwork.Rda')

organism<-dbSendQuery(con,"select r.id, o.species from Reference as r, Annotation as a, par_Organisms as o, AnnotationReference as ar where r.id=ar.referenceId and ar.gi=a.gi and a.organismId=o.id")
organism=fetch(organism,n=-1)
save(organism,file='organism.Rda')

tax<-dbSendQuery(con,"select r.id, o.tax_name from Reference as r, Annotation as a, Organism_new as o, AnnotationReference as ar where r.id=ar.referenceId and ar.gi=a.gi and a.tax_id=o.tax_id")
tax=fetch(tax,n=-1)
save(tax,file='tax.Rda')

ta<-dbSendQuery(con,"select dra1.author_id as auth1, dra2.author_id as auth2, dra1.id as id, tax_name from disambig_Reference_Authors as dra1 inner join Reference as gr on gr.id = dra1.id 

inner join disambig_Reference_Authors as dra2 on gr.id = dra2.id 

inner join AnnotationReference as ar on gr.id = ar.rEferenceId

inner join Annotation as a on a.gi=ar.gi

inner join Organism_new as o on o.tax_id = a.tax_id

where (dra1.author_id < dra2.author_id)  and tax_name = 'Homo sapiens' and gr.journal like 'Submitted%'")

ta=fetch(ta,n=-1)

#get the sample data
#data1<-sample(dataset,size=1000,replace=FALSE)
#it will have error:Error in sample.int(length(x), size, replace, prob) : 
#cannot take a sample larger than the population when 'replace = FALSE'
#You need to sample from the numbers, not from the data frame. Then use the results to get the sampled rows.

data1<-dataset[sample(nrow(dataset),size=1000,replace=FALSE),]
data2<-dataset[sample(nrow(dataset),size=2500,replace=FALSE),]
data3<-dataset[sample(nrow(dataset),size=5000,replace=FALSE),]
data4<-dataset[sample(nrow(dataset),size=7500,replace=FALSE),]
data5<-dataset[sample(nrow(dataset),size=10000,replace=FALSE),]

#get the tax in dataset
data11<-merge(data1,tax,by="id")
head(data11,10)
data11<-unique(data11) #in this case,we notice that data11 have more rows than data1. The reason is one reference can refer to more than one tax
dat11<-ddply(data11,c('tax_name'),function(x) count=nrow(x))
head(dat11)
names(dat11)[2]<-paste('freq')
dat11<-dat11[order(-dat11$freq),]



#visualize the data
head(tax)

data1000<-graph.data.frame(data1,directed=FALSE)
V(data1000)
E(data1000)
V(data1000)$deg<-degree(data1000)
E(data1000)$color<-'red'
#set the size
#in this point, we can use the size to reflect the high closeness or high betweenness 
V(data1000)$size=0.5
V(data1000)[deg>=10]$size=1
V(data1000)[deg>=15]$size=1.5
V(data1000)[deg>=20]$size=2

V(data1000)$size1<-sqrt(V(data1000)$deg)

max(degree(data1000))

# try add the colors
#in this point, we can use color to reflect info such as region, field...
head(data11)
Mus<-data11[which(data11$tax_name=='Mus musculus'),] #subset the data
Musid<-data11[,c('auth1','auth2')]
Musid<-unique(Musid)
Musid$auth1<-as.character(Musid$auth1)#change it to character
Musid$auth2<-as.character(Musid$auth2)
E(data1000)[V(data1000)[Musid$auth1]%--%V(data1000)[Musid$auth1]]$color<-'blue'

#E(g)[ V(g)[ color=="lightblue" ] %--% V(g)[ color=="green" ] ]$color <- "red"
for(i in 1:length(Musid[,1])){
  E(data1000,path=Musid[i,])$color="blue"
}
E(data1000)$color

png(file="submitted/data1000_size_type_color.png",width=2000,height=1125)
plot(data1000,vertex.size=V(data1000)$size,edge.width=1,vertex.label=NA)
#plot.igraph()
dev.off()

#get the largest cluster and visualize it

#community detection comparison
system.time(ec <- edge.betweenness.community(data1000))
print(modularity(ec))
png(file="submitted/data1000_betweenness.png",width=2000,height=1125)
plot(ec,data1000,vertex.size=V(data1000)$size,edge.width=1,vertex.label=NA)
#plot.igraph()
dev.off()

system.time(wc <- walktrap.community(data1000))
print(modularity(wc))
png(file="submitted/data1000_walktrap.png",width=2000,height=1125)
plot(wc,data1000,vertex.size=V(data1000)$size,edge.width=1,vertex.label=NA)
#plot.igraph()
dev.off()

system.time(lec <- leading.eigenvector.community(data1000))
print(modularity(lec))
png(file="submitted/data1000_eigenvector.png",width=2000,height=1125)
plot(lec,data1000,vertex.size=V(data1000)$size,edge.width=1,vertex.label=NA)
#plot.igraph()
dev.off()

#fast-greedy community finding works only on graphs without multiple edges
#here I just simplify() the data, in fact, i think it's better to weight the egdes
simdata<-simplify(simdata)
system.time(fc <- fastgreedy.community(simdata))
print(modularity(fc))
png(file="submitted/simdata_fastgreedy.png",width=2000,height=1125)
plot(fc,simdata,vertex.size=V(simdata)$size,edge.width=1,vertex.label=NA)
#plot.igraph()
dev.off()

system.time(mc <- multilevel.community(data1000))
print(modularity(mc))
png(file="submitted/data1000_multilevel.png",width=2000,height=1125)
plot(mc,data1000,vertex.size=V(data1000)$size,edge.width=1,vertex.label=NA)
#plot.igraph()
dev.off()

system.time(lpc <-label.propagation.community(data1000))
print(modularity(lpc))
png(file="submitted/data1000_multilevel.png",width=2000,height=1125)
plot(lpc,data1000,vertex.size=V(data1000)$size,edge.width=1,vertex.label=NA)
#plot.igraph()
dev.off()


#analyze this network
no.clusters(data1000)
clusters(data1000)


dbDisconnect(con)
