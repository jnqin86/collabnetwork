######################################reduce redundant in publication######################

get.data <- FALSE
if (get.data) {
  con<-dbConnect(MySQL(),
               user="GenBankUser", password="123GenBank456",
               host="metadatalab.syr.edu", dbname="genbank")

  #pub<-dbSendQuery(con,"select dra1.author_id as auth1, dra2.author_id as auth2, dra1.id as id, gr.year as year, gr.authors as authors, gr.journal as journal
  #              from disambig_Reference_Authors as dra1 inner join Reference as gr on gr.id = dra1.id inner join disambig_Reference_Authors as dra2 on gr.id = dra2.id
    #              where dra1.author_id < dra2.author_id and gr.journal not like 'Submitted%' and gr.journal not like 'Unpublished%';")
  #test=fetch(pub,n=-1)

  pub<-dbSendQuery(con,"select id,authors,title,journal,year from Reference as gr
                 where gr.journal not like 'Submitted%' and gr.journal not like 'Unpublished%';")
  
  subs <- dbSendQuery(con,"select id,authors,title,journal,year from Reference as gr
                 where gr.journal like 'Submitted%' and gr.journal not like 'Unpublished%';")
  
  subs <- dbSendQuery(con,"select id,authors,title,journal,year from Reference as gr
                 where gr.journal like 'Submitted%' and gr.journal not like '%Unpublished%';")
  submissions <- fetch(subs,n=-1)
  dim(submissions)
  colnames(submissions)
  submissions$journal[1:10]
  submissions[1,]
  table(submissions$title)
  
  tmp.index <- grep("unpublished", ignore.case = TRUE, x = submissions$journal)
  submissions$journal[tmp.index]
  
  publication=fetch(pub,n=-1)
  
  dim(publication)
}


save(publication,file='publication.Rda')
#########################################################################################################################
#colnames(publication)
df <- publication
#df <- submissions
#colnames(df) <- c('refid','auth','journal','title','year')
colnames(df) <- c('refid','auth','title','journal','year')

# refid, auth, journal, year, title

all(is.na(df))###there exists na value
any(is.na(df))

df[is.na(df)] <- 0

#n <- 20
#df <- data.frame(
 # refid = 1:n
#  , auth = sample(LETTERS[1:4], n, replace = T)
 # , journal = sample(letters[9:20], n, replace = T)
  #, year = sample(1996:2012, n, replace = T)
  #, title = sample(letters, n, replace = T)
#)

n <- dim(df)[1]
final <- data.frame(refid = rep(0, n)
                    , auth = rep("", n)
                    , journal = rep("", n)
                    , title = rep("", n)
                    , year = rep(0, n)
                    , count = rep(1, n)
                    , stringsAsFactors = FALSE
)

#final$auth <- as.character(final$auth)
#final$journal <- as.character(final$journal)
#final$title <- as.character(final$title)

current.final.row <- 1

#auths <- unique(df$auth)
#auths.len <- length(auths)
distinct <- unique(df[,2:5])
dis.len <- dim(distinct)[1]

length(unique(df$journal))
unknown.index <- grep(pattern = "unknown", ignore.case = TRUE, x = df$journal)
df$journal[unknown.index]


for (i in 1:dis.len) {
  a.index <- which(df$auth == distinct[i,1] & df$journal == distinct[i,2] & df$year ==  distinct[i,4] & df$title == distinct[i,3])
  a.len <- length(a.index)
  if (a.len == 1) {
    final[current.final.row, 1:5] <- df[a.index, ]
    current.final.row <- current.final.row + 1
  } else {
    df.sub <- df[a.index, ]
    final[current.final.row, 1:5] <- df[a.index[1], ]
    final$count[i] <- a.len
    current.final.row <- current.final.row + 1
    # temp.tab <- table(df.sub$journal, df.sub$year)
    
  #  jours <- unique(df.sub$journal)
  #  years <- unique(df.sub$year)
    
  }
  

}

finalpub <- final[1:289191,]
save(finalpub,file='finalpub.Rda')


colnames(finalSub)
grep("Unpublished", finalSub$journal, ignore.case = TRUE)



a.index <- which(df$auth == distinct[1,1] & df$journal == distinct[1,2] & df$year ==  distinct[1,4] & df$title == distinct[1,3])
a.len <- length(a.index)
if (a.len == 1) {
  final[current.final.row, 1:5] <- df[a.index, ]
  current.final.row <- current.final.row + 1
} else {
  df.sub <- df[a.index, ]
  final[current.final.row, 1:5] <- df[a.index[1], ]
  final$count <- a.len
  current.final.row <- current.final.row + 1
  # temp.tab <- table(df.sub$journal, df.sub$year)
  
  #  jours <- unique(df.sub$journal)
  #  years <- unique(df.sub$year)
  
}

colnames(reduceSubNetwork)
############################reduce in network#################
###load publishedNetwork.Rda:pubnet

pubnet$isElement <- is.element(pubnet$id,finalpub$refid)
tail(pubnet)
reducePubNetwork <- subset(pubnet,isElement == TRUE)

save(reducePubNetwork,file='reducedNetwork/ReducedPubNetwork.Rda')


########################combind submission and publication#############
reduceSubNetwork$type <- 'submission'
reduceSubNetwork$isElement <- NULL
reducePubNetwork$type <- 'publication'
reducePubNetwork$isElement <- NULL

colnames(reduceSubNetwork) <- c('auth1','auth2','id','year','type')

reducedNetwork <- rbind(reduceSubNetwork,reducePubNetwork)

save(reducedNetwork,file='ReducedNetwork.Rda')



