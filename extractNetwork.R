install.packages("igraph")
library(igraph)
install.packages("plyr")
library(plyr)
#connect to mysql
con<-dbConnect(MySQL(),
               user="GenBankUser", password="123GenBank456",
               host="metadatalab.syr.edu", dbname="genbank")

#get data from mysql  #####Submissions
sub<-dbSendQuery(con,"select dra1.author_id as auth1, dra2.author_id as auth2, dra1.id as id, par.year as year from disambig_Reference_Authors as dra1 inner join Reference as gr on gr.id = dra1.id inner join disambig_Reference_Authors as dra2 on gr.id = dra2.id inner join test.par_Reference_Journal as par on par.reference_id=gr.id
                 where dra1.author_id < dra2.author_id and gr.journal like 'Submitted%'")
subnet=fetch(sub,n=-1)
save(subnet,file='submittedNetwork.Rda')


#get data from mysql  #####Submissions
pub<-dbSendQuery(con,"select dra1.author_id as auth1, dra2.author_id as auth2, dra1.id as id, par.year as year from disambig_Reference_Authors as dra1 inner join Reference as gr on gr.id = dra1.id inner join disambig_Reference_Authors as dra2 on gr.id = dra2.id inner join test.par_Reference_Journal as par on par.reference_id=gr.id
                 where dra1.author_id < dra2.author_id and gr.journal not like 'Submitted%' and gr.journal not like 'Unpublished%'")
pubnet=fetch(sub,n=-1)
save(subnet,file='publishedNetwork.Rda')