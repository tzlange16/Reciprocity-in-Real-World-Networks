#============================================
# Detecting reciprocity in empirical networks
#         Amended K-means Script
#         Lange, Smolla, & Waring
#============================================
#adapted from python to r based on: https://www.askpython.com/python/examples/k-means-clustering-from-scratch 
require(tidyverse)

k_means <- function(x){

clusters <- tibble(point = as.character(1:length(x)),x=x)#create tibble assigning unique character identifier to each SPR 
euclidean <- function(a,b){
  sqrt((a-b)^2)#euclidean distance measure
}

b_center <- mean(c(min(x),0))#beneficiary center start at midpoint between 0 and minimum SPR
h_center <- mean(c(max(x),0))#helper center start at midpoint between 0 and maximum SPR

repeat{#create repeat loop 
mutate(clusters,B = euclidean(x,b_center),#calculate distance from beneficiary center
       R = euclidean(x,0),#calculate distance from reciprocator center of 0
       H = euclidean(x,h_center))%>%#calculate distance from helper center 
  gather(key = "cluster",value = "dist",3:5)%>%#transform to long form data
  group_by(point)%>%#group by SPR identifier
  arrange(point,dist)%>%#order by identifier and then distance in descending order
  slice(1)->clusters#assign cluster by selecting first row of each identifier
#this method prioritizes helpers and beneficiaries over reciprocators to produce conservative estiamtes
  
ungroup(clusters)%>%#ungroup by identifier
  filter(cluster %in% c("B","H"))%>%#select out beneficiaries and helpers
  group_by(cluster)%>% #group by cluster
  summarize(center = mean(x))->centers #calculate new K-mean center

if(b_center == centers$center[1] & h_center == centers$center[2]) break#if both new centers equal old centers, stop
clusters <- dplyr::select(clusters,point,x)#reset data to just identifier and SPR
b_center <- centers$center[1]#reassign new beneficiary cluster center
h_center <- centers$center[2]#reassign new helper cluster center
}

mutate(clusters, point = as.numeric(point))%>%#make identifiers neumeric
  arrange(point)->clusters#reorder SPRS according to original order

cluster <- clusters$cluster#assign cluster assignments as vector
centers <- tibble(type = c("Beneficiary","Reciprocator","Helper"), center = c(b_center,0,h_center))#assign cluster centers as a vector

return(list(cluster = cluster, centers = centers))#return cluster assigments and cluster centers as a list object

}




