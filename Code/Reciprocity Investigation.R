#================================
#      Single Assistance
#================================

library(tidyverse)
#load in food club splits
#FC
#====
load("FC_Shadow_edgelist_greatest_need.Rdata")
abbyset <- read_csv("abbyset.csv")
foodclub.edges$granbyfoodcoop.networkable[[92]]<-NULL#defunct graph
foodclub.edges$belfastcohousing.networkable <- NULL #majority non-split
foodclub.edges$groundhognaturals.networkable <- NULL#majority non-split
foodclub.edges$steubenbulk.networkable<-NULL
foodclub.edges$newportnaturals.networkable<-NULL
#function to replace non-splitting orders with an empty tibble, and aggregate all orders into one edge list data set
degree.balance <- function(order){
  edges<- order[[1]]#extract order
  if(is.null(edges)){edges <- tibble()}#if order is null, create empty tibble
  return(edges)
}


FC <- map_df(foodclub.edges,~map_df(.x,degree.balance, .id = "Date"), .id = "Club")#aggregate all orders into one edge list data set


FC$Product <- tolower(FC$Product)#make products all lowercase
FC <- left_join(FC,abbyset, by = c("Product"="Description"))%>% #join with product categories
  select(Club,Date,From,To,Product,Category)#select only required columns of date, from, to, product, and catagory

single.buys <- ungroup(FC)%>% #ungroup by club
  group_by(From,Club)%>%#group by edge sender
  count(Category)%>%#count number of times an item is purchased
  filter(n == 1)#filter to only purchased once

reciprocal.maybe <- inner_join(single.buys,FC)%>%#rejoin with with full dataset 
  select(Club,Date,From,To,Product,Category) #select relevent clubs

single.assistance <- ungroup(reciprocal.maybe)%>%#ungroup by person
  group_by(Club)%>%#group by club to prevent possible overlapping usernames across clubs
  count(From)#count how many times an individual assists a single time

single.recieved <- ungroup(reciprocal.maybe)%>%#ungroup by person
  group_by(Club)%>%#group by club to prevent possible overlapping usernames across clubs
  count(To)#count how many times 

colnames(single.assistance)[3] <- "Given"#rename single assistance given count column from "n" to "Given"
colnames(single.recieved)[3] <- "Received"#rename single assistance received count column from "n" to "Received"

FC <- full_join(single.assistance,single.recieved, by = c( "From" = "To","Club"))%>%#Join columns
  filter(From != "surplus")%>%#remove club account account
  replace_na(list(Given = 0,
                  Received = 0))#change NAs to 0
  
#=====
# BCS
#=====
load("BCS_Shadow_edgelists_greatest_need.Rdata")
abbybcs <- read_csv("abbybcs.csv")
BCS.Graphs$centerfood <- NULL #many-non splits
BCS.Graphs$thefarmconnection <- NULL #one order
BCS.Graphs$nararabuyinggroup <- NULL #majority non-split
BCS.Graphs$naturestouchfarm <- NULL #majority non-split
BCS.Graphs$graintrain.sault <- NULL #majority non-split
BCS.Graphs$graintrain.petoskey <- NULL ##majority non-split
BCS.Graphs$cooplalouve <- NULL #majority non-split
BCS.Graphs$kfi <- NULL #redundant with smfi


BCS <- map_df(BCS.Graphs,~map_df(.x,degree.balance, .id = "Date"),.id = "Club")

bcs <- left_join(BCS,abbybcs, by = c("Product"="Description"))

single.buys <- ungroup(bcs) %>%
  group_by(From,Club)%>%
  count(Category)%>%
  filter(n == 1)
  

reciprical.maybe <- select(bcs,From,To,Product,Club,Category)%>%
  inner_join(single.buys)%>%
  filter(From!=To)


single.assistance <- ungroup(reciprical.maybe)%>%
  group_by(Club)%>%
  count(From, name = "Given")

single.recieved <- ungroup(reciprical.maybe)%>%
  group_by(Club)%>%
  count(To, name = "Received")

BCS <- full_join(single.assistance,single.recieved, by = c( "From" = "To","Club"))%>%
  filter(From != "surplus")%>%#remove club account
  filter(From!="klfi")%>%#remove club account
  filter(From!="admin")%>%#remove club account
  replace_na(list(Given = 0,
                Received = 0))#change NAs to 0


#=====
# All
#=====
BCS$software<-"BCS"
FC$software<-"FC"
All<-bind_rows(BCS,FC) 

group_by(All,Club)%>%
  nest()%>%
  rename(single.assistance = data)%>%unnest()%>%
  right_join(select(all.clubs,data)%>%unnest(), by = c("From"="name1","Club"="name"))%>%
  select(-From,-software)%>%
  group_by(Club,`Club Number`)%>%nest()%>%
  rename(single.assistance = data)%>%
  inner_join(all.clubs, by = c("Club"="name","Club Number"))%>%
  arrange(`Club Number`)->all.clubs
save(all.clubs, file="Reciprocity.Rdata")
