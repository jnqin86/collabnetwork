##########ppublication#############
#load ReducedPubNetwork.Rda
data1 <- reducePubNetwork[,c('auth1','id','year')]
data2 <- reducePubNetwork[,c('auth2','id','year')]

colnames(data1) <- c('auth','id','year')
colnames(data2) <- c('auth','id','year')


pub <- unique(rbind(data1,data2))
save(pub,file='publication_author.Rda')

#####################Submission##############
#load ReducedSubNetwork.Rda
data1 <- reduceSubNetwork[,c('auth1','id','year')]
data2 <- reduceSubNetwork[,c('auth2','id','year')]

#colnames(data1) <- c('auth','id','year')
#colnames(data2) <- c('auth','id','year')

#######author, submission,year###############
#sub <- unique(rbind(data1,data2))
#save(sub,file='submission_author.Rda')





##################REMOVE AUTHORS WHO ONLY HAVE 1 PUBLICATION WITHOUT SUBMISSION#############
####Read file wholeNetworkAnalysis/authorID_sub_pub.Rda
#SubPub[is.na(SubPub)] <- 0

removedID<-subset(SubPub, SubPub$subFreq<=1 & SubPub$pubFreq<=1,select=author)

SubPub$author<-as.numeric(as.character(SubPub$author))
removedID$author<-as.numeric(as.character(removedID$author))

#retainedID<-setdiff(SubPub$author,removedID$author)



position1 <- match(removedID$author,sub_pub[,"auth1"])

position1 <- position1[!is.na(position1)]

retainedNetwork<-sub_pub[-position1,]



position2 <- match(removedID$author,retainedNetwork[,"auth2"])

position2 <- position2[!is.na(position2)]

retainedNetwork1<-retainedNetwork[-position2,]

save(retainedNetwork1,file='retainedNetwork_less1.Rda')


