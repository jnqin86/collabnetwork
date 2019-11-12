########################################################
#
# Whole network: Windows of time Mark and Jeff used.

# Reducing the network to a size:
#  1. Random sampling (Ask Jeff)
#  2. Purposive sampling (select taxonomic communities for infectious disease and model organisms). 
#  3. Sample to get most-highly connected components. 
#  4. Sample neighboodhood of highly influential or active nodes. 

# "look-back" animation: fade every 5 images? We have only 30 years, feasible? Worth it? 
# possible to do 3D plots?

# install.packages("igraph")
library(igraph)

# we will read in a graph object, saved as .rda. 
# start with data submission, test for one year (1998)

# getwd()
setwd("C:/Users/sarah/Documents/Rscripts/network1992_2018loop/datasubmissions")
load(file = "sub-network-graph1998.rda")


rgb(1,.7,.3,.4) # 4 = opcity
# see visualize_coauthornetwork.R