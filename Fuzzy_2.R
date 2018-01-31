# This code works without data since we are defininf a dummy dataset

# Packages:
install.packages('stringdist')
library(stringdist)
library(reshape2)
library(data.table)

###Fuzzy Function:
fuzzy1 <- function(a,b)
{
  #a1 = as.character(a$authors[1])
  #b1 = as.character(b$authors[grep("Talmadge",b$authors)][2])
  a1 = as.character(a)
  b1 = as.character(b)
  dist.a1b1<-adist(a1,b1, partial = TRUE, ignore.case = TRUE)
  distance.methods<-c('osa','lv','dl','hamming','lcs','qgram','cosine','jaccard','jw')
  dist.methods<-list()
  
  #Fuzz 1###########
  for(m in 1:length(distance.methods))
  {
    dist.name.enh<-matrix(NA, ncol = length(b1),nrow = length(a1))
    for(i in 1:length(b1)) {
      for(j in 1:length(a1)) { 
        dist.name.enh[j,i]<-stringdist(tolower(b1[i]),tolower(a1[j]),method = distance.methods[m])      
        #adist.enhance(source2.devices[i,]$name,source1.devices[j,]$name)
      }  
    }
    dist.methods[[distance.methods[m]]]<-dist.name.enh
  }
  
  match.s1.s2.enh<-NULL
  for(m in 1:length(dist.methods))
  {
    
    dist.matrix<-as.matrix(dist.methods[[distance.methods[m]]])
    min.name.enh<-apply(dist.matrix, 1, base::min)
    for(i in 1:nrow(dist.matrix))
    {
      s2.i<-match(min.name.enh[i],dist.matrix[i,])
      s1.i<-i
      match.s1.s2.enh<-rbind(data.frame(s2.i=s2.i,s1.i=s1.i,s2name=b1[s2.i], s1name=a1[s1.i], adist=min.name.enh[i],method=distance.methods[m]),match.s1.s2.enh)
    }
  }
  
  # Let's have a look at the results
  matched.names.matrix<-dcast(match.s1.s2.enh,s2.i+s1.i+s2name+s1name~method, value.var = "adist")
  Fuzzy1 = matched.names.matrix
  
  return(Fuzzy1)
  
}


#Initial frames#############
a = data.frame( patent_no = c(1,1,1,2,3,4,5,6)
                ,Lname = c("roberts","evans","george","yu","bahl","stevens","Alyssia","Cecelia")
                ,Fname = c("A","L","A","F","G","M","A","A")
                ,seq = c(1,2,3,1,1,1,1,1))

b = data.frame( unique_id = c(1,1,2,3,4,5,6,7)
                ,Lname = c("Robert","Evans","micheal","watson","baxter","stevens","Elissa","Cecily")
                ,Fname = c("A","L","A","F","G","M","A","A")
                ,seq = c(1,2,1,1,1,1,1,1))

a = data.table(a)
b = data.table(b)
# Structure for final author list:
merged_author_list_temp = data.frame(   patentAuthorLname = as.character(a$Lname[1])
                                        ,patentAuthorFname = as.character(a$Fname[1])
                                        ,refAuthorLname = as.character(b$Lname[1])
                                        ,refentAuthorFname = as.character(b$Fname[1])
                                        ,patentNumber = a$patent_no[1]
                                        ,unique_id = b$unique_id[1]
)
final_list = merged_author_list_temp[0,]
final_list = data.table(final_list)

#tapply way######

function1 = function(a,b)
{
  argument1 = a$Lname[i]
  argument2 = b$Lname[i]
  result = fuzzy1(argument1,argument2)
  df = result[0,] # For copying the df structure
  return(df)
}


function2 = function(df,a,b)
{
  # Now, since we only want similar last names:
  current_df <<- df[df$cosine<0.2,]
  #print(current_df)
  # Now, lets check if the first initial matches:
  firstInitial_table1 <<- a$Fname[a$Lname == as.character(current_df$s1name[1])]
  firstInitial_table2 <<- b$Fname[b$Lname == as.character(current_df$s2name[1])]
  # Fuzzy match for Fnames having the same Lnames
  result_firstInitial_1 <<- fuzzy1(firstInitial_table1,firstInitial_table2) 
  
  #Now, if the match is good enough and there is just one entry in the table,
  #we can say that the two authors from diff table are the same.
  if(!is.na(result_firstInitial_1$cosine[1]) & result_firstInitial_1$cosine[1]<0.2 & length(result_firstInitial_1$cosine)==1)
  {
    #Let's create a final merged author list, all the final matched authors will go into this list
    merged_author_list <<- data.frame(  patentAuthorLname <<- as.character(current_df$s1name[1])
                                      ,patentAuthorFname <<- as.character(result_firstInitial_1$s1name[1])
                                      ,refAuthorLname <<- as.character(current_df$s2name[1])
                                      ,refentAuthorFname <<- as.character(result_firstInitial_1$s2name[1])
                                      ,patentNumber <<- a$patent_no[a$Fname==as.character(result_firstInitial_1$s1name[1]) &
                                                                    a$Lname==as.character(current_df$s1name[1])]
                                      ,unique_id <<- b$unique_id[b$Fname==as.character(result_firstInitial_1$s2name[1]) &
                                                                 b$Lname==as.character(current_df$s2name[1])]
    )
    l <<- list(final_list,merged_author_list)
    final_list <<- rbindlist(l)
    
  }
  else
  {
    # To get all the authors from table1 with same patent number
    currentPatentNumber <<- a$patent_no[a$Lname == as.character(argument1)]
    argument1_df <<- a[a$patent_no == currentPatentNumber,]
    argument1 <<- argument1_df$Lname[argument1_df$Lname != as.character(argument1)][1]
    # Same with table 2
    currentUnique_id <<- b$unique_id[b$Lname == as.character(argument2)]
    argument2_df <<- b[b$unique_id == currentUnique_id,]
    argument2 <<- argument2_df$Lname[argument2_df$Lname != as.character(argument2)][1]
    result1 <<- fuzzy1(argument1,argument2)
    df1 <<- result1[0,] # Again, For copying the df structure
    
    function3(df1,a,b,argument1,argument2,argument1_df,argument2_df,final_list)
  }
}

function3 = function(df1,a,b,argument1,argument2,argument1_df,argument2_df,final_list)
{
  for(k in 1:length(argument2_df$unique_id))
  {
    argument2 <<- argument2_df$Lname[k]
    result1 <<- fuzzy1(argument1,argument2)
    df1 <<- rbind(df1,result1)
  }
  
  current_df <<- df1[df1$cosine<0.1,]
  firstInitial_table1 <<- argument1_df$Fname[argument1_df$Lname == as.character(current_df$s1name[1])]
  firstInitial_table2 <<- argument2_df$Fname[argument2_df$Lname == as.character(current_df$s2name[1])]
  result_firstInitial_1 <<- fuzzy1(firstInitial_table1,firstInitial_table2) 
  
  if(!is.na(result_firstInitial_1$cosine[1]) & result_firstInitial_1$cosine[1]<0.1 & length(result_firstInitial_1$cosine) == 1)
  {
    #print ("SECOND IF!!!")
    merged_author_list1 <<- data.frame(  patentAuthorLname <<- as.character(current_df$s1name[1])
                                       ,patentAuthorFname <<- as.character(result_firstInitial_1$s1name[1])
                                       ,refAuthorLname <<- as.character(current_df$s2name[1])
                                       ,refentAuthorFname <<- as.character(result_firstInitial_1$s2name[1])
                                       ,patentNumber <<- a$patent_no[a$Fname==as.character(result_firstInitial_1$s1name[1]) &
                                                                     a$Lname==as.character(current_df$s1name[1])]
                                       ,unique_id <<- b$unique_id[b$Fname==as.character(result_firstInitial_1$s2name[1]) &
                                                                  b$Lname==as.character(current_df$s2name[1])]
    )
    l1 <<- list(final_list,merged_author_list1)
    final_list <<- rbindlist(l1)
    
  }
  
}

for(i in 1:length(a$patent_no))
{
  df = function1(a,b)
  argument1 = a$Lname[i]

  for(j in 1:length(b$unique_id))
  {
    argument2 = b$Lname[j]
    result = fuzzy1(argument1,argument2)
    df = rbind(df,result)
  }
  
  function2(df,a,b)
}


################################################################################################
#
#lapply(1:nrow(b), f1,
#   a=a
#  ,b=b
#  ,df=df
#  ,argument1 = a$Lname[i]
#  ,j=0
#  )


#tapply(df, b$unique_id, f1)
#j = 0
#lapply(b$unique_id,f1,j=0,argument1 = argument1,a=a,b=b,df=df)
#
#
f1 <- function(argument1,j,a,b,df)
{
  j = j+1
  argument2 = b$Lname[j]
  result = fuzzy1(argument1,argument2)
  df = rbind(df,result)
  function2(df,a,b)
  return (0)
}

