mysassignee_keyid = read.csv("assignee_keyid.csv") # Reading the processed assignee file
#US_data = read.csv("US_data.csv")

rm(dates,out,temp) # To remove objects which are no longer needed..

################################################################################
# Let's find out if one patent has more than one instutuition:

a = unique(newdf$Patent_Number)
b = unique(newdf$dataverse_Assignee)
c = paste(a,b)
length(unique(c)) #44466
length(unique(newdf$Patent_Number)) #44466
# Hence, we can conclude that there is only one assignee for one instituition..

################################################################################



################################################################################
qplot(as.numeric(US_data$year), geom="histogram", binwidth = 1) 
hist(as.numeric(US_data$year))
table(as.numeric(US_data$year))
plot(table(US_data$month))
################################################################################


################################################################################
table(newdf$dataverse_AsgType)
# 0: International
# 1: Assignee Blank
# 2: 
# 3: International*
# 4: Individual*
# 5: Individual (International)*
# 6: Government 
# 7: Government (International)
# 8: State of Oregon
# 9: State of Oregon, University of Houston,..

################################################################################

hist(newdf$dataverse_AsgType)

####################

