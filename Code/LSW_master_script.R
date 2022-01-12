#============================================
# Detecting reciprocity in empirical networks
#             Master Script
#         Lange, Smolla, & Waring
#============================================

#Required Packages
library(tidyverse)
library(igraph)
library(sandwich)
library(lmtest)
library(lubridate)
#load data
load("Data/Reciprocity.Rdata")
#Read necessary functions from Direct Reciprocity.R
source("Code/reciprocity counts and stability.R")
source("Code/k_means.R")
#===================
#Data Processing ------

#function for calculating members' total in-degrees and out-degrees for Fixed Effect model
degree.balance <- function(graph){#function that finds the net received of each person for an order
  In <- igraph::degree(graph, mode="in")#extract each users in degree
  Out <- igraph::degree(graph, mode="out")#extract each users out degree
  Purchaser <- igraph::get.vertex.attribute(graph,"name")#get purchaser's anonymized name
  SPR <- log((1+Out)/(1+In))#calculate shared purchase ratio
  date <- as_date(get.graph.attribute(graph, "Date"))#add date
  surplus <- tibble(Purchaser = Purchaser,In.Degree = In, Out.Degree = Out,SPR = SPR)#create tibble in degrees, out degrees, and shared purchase ratio
  surplus$Date <- date#add date to tibble
  return(surplus)
}

#Calculate degree balance for all clubs, remove nonsplitting members, and club accounts that manage delivery fees
all.clubs.1 <- dplyr::mutate(all.clubs, db = map(graphs, ~map_df(.x,degree.balance)))%>%#calculate total in-degree, total out-degree, per member per club
  select(`Club Number`,db)%>%unnest()%>%#select just degree balance and unnest data
  filter(!Purchaser %in% c("surplus","Surplus"))%>% #remove surplus accounts that manage delivery
  mutate(t.degree = In.Degree + Out.Degree)%>% #calculate members total degrees
  filter(t.degree != 0)%>% #remove members with zero total degrees, as they do not participate in splits
  group_by(`Club Number`)%>% #regroup by club name
  nest()%>%#re-nest data for joining with reciprocity & stability measurements
  rename(degree.balance = data)#rename column named "data" from nesting process to "degree.balance"



#calculate Within/between order direct/indirect reciprocity & Reciprocal stability using recip.stab function from "reciprocity coutns and stability.R"
all.clubs.2 <- mutate(all.clubs,stab = map(graphs,stability))%>%
  select(`Club Number`,stab)


#combine clubs  
all.clubs <- full_join(all.clubs,all.clubs.1)%>%
  full_join(all.clubs.2)
#=================
#Mixed Effects model----
#All Edges
select(all.clubs,`Club Number`,degree.balance)%>%#select degree balance data
  unnest()%>%#unnest
  group_by(`Club Number`,Purchaser)%>%#group by club name and purchaser
  summarize_at(.vars = c("In.Degree","Out.Degree"),.funs = sum)->RE#aggregate each purchaser's in and out degree

lme4::lmer(Out.Degree ~ In.Degree + (1|`Club Number`) ,data=RE)->re.model#mixed effects model for all edges

summary(re.model)#summary of model

#Singular assistance edges
select(all.clubs,`Club Number`,single.assistance)%>%#select singular assistance data
  unnest()->RE.SA#unnest data
RE.SA.model <- lme4::lmer(Given~Received+(1|`Club Number`),data = RE.SA)#estimate mixed effects model for singular assistance edges
summary(RE.SA.model)#summary of model

#Markov Chain-----
library(markovchain)
library(MASS)

#Function to classify members based off a markov chain
classification <- function(x){
  # using markovchainFit from the markovchain package
  mcFit <- markovchainFit(data=x)
  # look at results
  m <- mcFit$estimate@transitionMatrix#extract transition matrix
  r<-eigen(m)#calculate eigenvalues/vectors
  lvec <- ginv(r$vectors)#invert eigenvectors
  stv <- as.numeric(lvec[1,]/sum(lvec[1,]))#to coerce imaginary eigenvectors to real numbers, normalize first eigenvector as stationary state
  names(stv)<-colnames(m)#name stationary state according to role type
  cat <- names(stv[stv==max(stv)])#find maximum
  if(max(stv)==0)cat<-NA#if no stationary state, default to NA
  if(length(x)==1)cat<-NA#if only one order, default to NA
  if(length(cat)>1)cat<-"R"#if there is a tie - default to reciprocator
  return(cat)}

#function to extract transition matricies for each individual for global and club level average transition probabilities
mat.val <- function(x){
  # using markovchainFit from the markovchain package
  mcFit <- markovchainFit(data=x)
  # Make tibble of all possible transitions
  all.probs <- tibble(from=c("B", "H", "R", "B", "H", "R", "B", "H", "R"),
                      to = c("B", "B", "B", "H", "H", "H", "R", "R", "R"))
  m <- mcFit$estimate@transitionMatrix%>%as_tibble()#extract individual's transition matrix
  m$from <- colnames(m) #name transiton probability rows
  r <- gather(m,"to","prob",1:(ncol(m)-1))%>%#transform matrix to long form
    right_join(all.probs)%>%#join to identify which probabilities are missing
    replace_na(list("prob"=0))#replace NAs with 0's
  group_by(r,from)%>%#group by from roll
    summarize(sum=sum(prob))%>% #sum all from probabilities
    filter(sum>0)%>%#remove any states that aren't reached
    inner_join(r)->r#rejoin with original data set with complete list of possible transitons per individual
  return(r)
  
}

#calculate all member types for all clubs
mutate(all.clubs, member.types = map(degree.balance, ~mutate(.x, cluster = k_means(SPR)$cluster)%>%#assign SPRs to a cluster
                                       group_by(Purchaser)%>%#group by purchaser
                                       arrange(Date)%>%#arrange by date to calculate markov matricies
                                       nest()%>%mutate(type = map(data, ~classification(.x$cluster)))%>%#calculate member types using classification function
                                       dplyr::select(Purchaser,type)%>%unnest()),#select just member name and member type
       #calculate each individual's markov probabilities for assembling global and club level matricies       
       markov.matrix = map(degree.balance, ~mutate(.x, cluster = k_means(SPR)$cluster)%>%#assign SPR's to a cluster
                             group_by(Purchaser)%>%#group by purchaser
                             arrange(Date)%>%#arrange by data to calculate as a markov chain
                             nest()%>%mutate(mm = map(data, ~mat.val(.x$cluster)))%>%#run matrix value function to get transition probabilities
                             dplyr::select(Purchaser,mm)%>%unnest()))->all.clubs#unnest individuals for calculating global and individual matricies

#calculate global transition matrix - Table 2
dplyr::select(all.clubs,`Club Number`,markov.matrix)%>%#select markov probabilities from total dataset
  unnest()%>%#unnest whole dataset
  group_by(from,to)%>%#group by from role and to role
  summarise(N = n(),mean = mean(prob))%>% #calculate global mean and # of people used to calculate that mean
  spread(to,mean)%>%#spread into matrix form
  dplyr::select(from,N,H,R,B)#reorder columns in more intuitive form

save(all.clubs, file = "Reciprocity.Rdata")





