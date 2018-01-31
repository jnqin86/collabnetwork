con<-dbConnect(MySQL(),
               user="GenBankUser", password="123GenBank456",
               host="metadatalab.syr.edu", dbname="genbank")

##################################################end of the geographical data#######################

data<-dbSendQuery(con,"select  r.id, r.journal, r.year, o.tax_id,o.tax_name
                  from Reference as r, AnnotationReference as ar, Annotation as a, Organism_new as o
                  where r.id=ar.referenceId and ar.gi=a.gi and a.tax_id=o.tax_id and o.tax_name like 'SARS %';")
eventdata=fetch(data,n=-1)
eventdata<-unique(eventdata)
save(eventdata,file='event/mers/event.Rda')

#############process the reference-event data###################################################################################################
#####part 1: plot the trend by year
byYear<-table(eventdata$year)
barplot(byYear,main='Reference in H1N1',xlab='year',ylab='Frequency',ylim=c(0,15000),col='darkblue')


h1n1<-eventdata
h1n1$type<-'publication'
h1n1[grep('Submitted',h1n1$journal),]$type<-'submission'
h1n1[grep('Unpublished',h1n1$journal),]$type<-'Unpublished'
# Pie Chart from data frame with Appended Sample Sizes
mytable <- table(h1n1$type)
lbls <- paste(names(mytable), "\n", mytable, sep="")
pie(mytable, labels = lbls, 
    main="h1n1 (with sample sizes)")


#############################submission by region################################################
h1n1<-merge(h1n1,geo,by=c('id','year'))
b<-table(h1n1$country,h1n1$year)
name<-unique(h1n1$country)
color<-topo.colors(length(name))
par(mar=c(5,10,5,5)) 
barplot(b,main="West Nile Submission by Country", horiz=TRUE, col=color,las=2,xlim=c(0,100),
        legend.text = TRUE,  args.legend = list(x = "bottomright", bty = "n",ncol=2,cex=0.56))

####part 2: analysis the network ---submission####################################################################
westnile<-read.table(file='event/westnile/westnilesubmission.csv',sep=',',head=TRUE)
reference<-unique(westnile$id)
subnet<-data.frame()
for(i in 1:length(reference))
{
  sql<-paste("select dra1.author_id as auth1, dra1.author_name as author1_name, 
dra2.author_id as auth2, dra1.author_name as author2_name, dra1.id as id, gr.year as year 
from (
             select dra.id, dra.author_id, da.author_name from disambig_Authors as da, disambig_Reference_Authors as dra where da.id=dra.author_id
  ) as dra1 inner join Reference as gr on gr.id = dra1.id inner join (
             select dra.id, dra.author_id, da.author_name from disambig_Authors as da, disambig_Reference_Authors as dra where da.id=dra.author_id
  ) as dra2 on gr.id = dra2.id 
             where dra1.author_id<dra2.author_id and  gr.id = '",reference[i],"' and gr.journal like 'Submitted%' ;",sep="")
  data<-dbSendQuery(con,sql)
  net1=fetch(data,n=-1)
  subnet<-rbind(subnet,net1)
}
save(subnet,file='event/westnile/submittednetwork.Rda')

subnet<-merge(subnet,geo,by=c('id','year'))



######################network####################################

subnet1<-merge(net,author,by.x='auth1',by.y='id')
colnames(subnet1)<-c('auth1','auth2','id','year','name1')
subnet1<-merge(subnet1,author,by.x='auth2',by.y='id')
colnames(subnet1)<-c('auth2','auth1','id','year','name1','name2')
save(subnet1,file='event/westnile/namenetwork.Rda')
subnet1<-merge(subnet1,geo,by=c('id','year'))
head(subnet1)




##########################end of adding author name#######################
xx<-table(subnet$country)
namnet<-graph.data.frame(subnet1[,c(5,6)],directed=FALSE)  ###for publication
#################find the top 10 influential people
#author<-as.data.frame(V(namnet)$name)
#measurement<-data.frame(author,
#                        deg=degree(namnet,normalized = TRUE),#degree
#                        eig=evcent(namnet)#eig.cent
#)
#measurement<-measurement[order(-measurement[,3],-measurement[,2]),]
#top10<-measurement[1:10,1]
#namnet<-simplify(namnet,edge.attr.comb="sum")
#####set the edges' attributes
#namnet <- set.edge.attribute(namnet, "year",index=E(namnet), value=sarsnet[,4])###submission
continentValue<-as.character(subnet1[,8])
namnet <- set.edge.attribute(namnet, "continent",index=E(namnet), value=continentValue)###publication
namnet <- set.edge.attribute(namnet, "color", value='grey')
###set the vertices' attributes
V(namnet)$color='lightblue'
V(namnet)$size=1
V(namnet)$lable<-NA
###assign vertices attributes
n <- vcount(namnet)
x<-vector()
for (i in 1:n)
{
  # from and to are iGraph "iterators"
  continentsNum <- unique(E(namnet)[from(V(namnet)$name[i])]$continent)
  x<-c(x,length(continentsNum))
  if (length(continentsNum) > 1){
    V(namnet)[V(namnet)$name==V(namnet)$name[i]]$lable<-V(namnet)[V(namnet)$name==V(namnet)$name[i]]$name
  }
}
###assign edges the colors
continent1<-sort(unique(E(namnet)$continent))
colorList<-rainbow(length(continent1))
for(i in 1:length(continent1))
{
  E(namnet)[continent==continent1[i]]$color=colorList[i]
}
lay <- layout.fruchterman.reingold(namnet)
E(namnet)$curved <- FALSE
png(file='event/H1N1/network.png',width=1600,height=900)
plot(namnet,vertex.size=V(namnet)$size,vertex.label=V(namnet)$lable,vertex.color=V(namnet)$color,vertex.label.cex=2,main='submission Network from 2000 to 2013')
legend("bottomright",continent1,lty=c(1,1),lwd=c(5,5),col=colorList, title="Edges") 
dev.off()


##############################author numbers############################################
colnames(authorNum)<-c('id','year','authorNum')
dataauthor<-merge(eventdata,authorNum,by=c('id','year'))
data<-dataauthor[,c(1,2,6)]

breaks<-c(0,1,4,15,100,max(data$authorNum))

data$num<-cut(data$authorNum,breaks,include.lowest = TRUE)

data1<-table(data$year,data$num)
data1<-as.data.frame(data1)
colnames(data1)<-c('year','num','Frequency')
data1$year<-as.numeric(as.character(data1$year))
save(data1,file='event/westnile/authornumdata.Rda')
numlevel<-unique(data1$num)

colorList<-rainbow(length(numlevel))

plot(x=c(2000,2015),y=c(0,70),type='n',xlab="year",ylab='Frequency',main='Submission Frequency by Number of Authors')

for (i in 1:length(numlevel)){
  points(data1$year[data1$num==numlevel[i]],data1$Frequency[data1$num==numlevel[i]],col=colorList[i],pch=16)
  lines(data1$year[data1$num==numlevel[i]],data1$Frequency[data1$num==numlevel[i]],col=colorList[i])
}

legend('topleft',legend=numlevel,col=colorList,pch=16,lty=1)
