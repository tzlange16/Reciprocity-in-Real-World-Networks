# Lange, Smolla, Waring 2021
# Figure Script
library(tidyverse)
library(igraph)
library(cowplot)
load("Data/Reciprocity.Rdata")
source("Direct reciprocity.R")

names(all.clubs$member.types)<-all.clubs$Club

#Figure 1 (get from Tim)----
par(mfrow=c(1,3))
#some items
items <- c("A","B","C","D","E","F","H","I","J")
#some purchasers
purchasers <- 1:9
#some purchases
set.seed(1)
matrix(rbinom(81,1,.4),ncol=9,nrow=9)->m
colnames(m)<-items
rownames(m)<-purchasers
#make graph of purchases
g<-graph_from_incidence_matrix(m)

plot.igraph(g,layout = layout.bipartite(g),vertex.label = NA,edge.color = "grey25", vertex.shape = c(rep("circle",9),rep("square",9)),vertex.color = c(rep("red",9),rep("blue",9)))
legend(x=-.75,y=1.4,legend =c("members","items"),pch = c(16,15), col = c("red","blue"),horiz = TRUE,,cex = 1.5,bty = "n",pt.cex = 3.5)
legend(x=-.75,y=1.4,legend =c("members","items"),pch = c(21,22),horiz = TRUE,bty = "n",cex = 1.5,pt.cex = 3.5)
bipartite.projection(g)$proj1 ->up
get.adjacency(up,attr = "weight")->m2
graph_from_adjacency_matrix(m2,mode = "undirected")->up
layout.fruchterman.reingold(up)->coords
as.matrix(m2)%>%
  as.data.frame(row.names = TRUE)%>%
  mutate(from=row_number())%>%
  gather(key = "to",value = "n.edges",1:9)%>%
  filter(n.edges>0)->edges
edge.list <- matrix(ncol = 2)
for(i in 1:nrow(edges)){
dyad <- c(edges$from[i],edges$to[i])
matrix(rep(sample(dyad,2),edges$n.edges[i]),ncol = 2,byrow = TRUE)%>%
rbind(edge.list)->edge.list
}
edge.list[-nrow(edge.list),]->edge.list
graph_from_edgelist(edge.list,directed = FALSE)%>%
  simplify(remove.multiple = FALSE)%>%
  plot(layout = coords,vertex.label = NA,vertex.color = "red",edge.color = "grey25")

graph_from_edgelist(edge.list)%>%
  simplify(remove.multiple = FALSE)%>%
  plot.igraph(layout = coords, edge.arrow.size = .5, vertex.label = NA, vertex.color = "red",edge.color = "grey25")

#Figure 2----
select(types,name,stab)%>%#select out reciprocity proportions &stability
  unnest()%>%#unnest tibble
  select(name,pwd,pbd,pwi,pbi)%>%#select club name and reciprocity type proportions
  mutate(pur = 1-pwd-pbd-pwi-pbi)%>%#create percent unreciprocated
  gather("Key","Value",2:6)%>%#transform data to long form
  mutate(time = ifelse(Key %in% c("pwd","pwi"),#define time scale of reciprocity:
                        "W",#within order
                        "B"),#between order
          direction = ifelse(Key %in% c("pwd","pbd"),#define directedness of reciprocity
                             "DR",#direct reciprocity
                             "IR"))%>%#indirect reciprocity
  mutate(time = ifelse(Key == "pur",#define unreciprocated edges temporally 
                       "U",#named not for figure labeling
                       time),
         direction = ifelse(Key == "pur",#define unreciprocated edges directionally
                            "R",#named reciprocated for figure
                            direction))%>%
  unite("T-D",time,direction,sep = "")%>%
  mutate(`T-D` = as_factor(`T-D`))->fig 


group_by(fig,`T-D`)%>%summarize(mean = paste("Mean = ",round(mean(Value),2),sep = ""))%>%#create figure text of mean proportion across clubs
  mutate(x=.75,y=4)->fig.text

fig.text$mean[1]<-"Mean = 0.60"

group_by(fig,`T-D`)%>%summarize(mean = mean(Value))%>%
  right_join(fig)%>%
ggplot(aes(x=Value))+
  geom_histogram(color = "grey25", binwidth = .01,alpha = .75)+
  scale_x_continuous(limits=c(0,1))+
  scale_y_continuous(position="right")+
  facet_grid(`T-D`~.,switch = "both")+
  geom_vline(aes(xintercept=mean),size = 1,)+
  geom_text(data = fig.text, mapping = aes(x=x,y=y,label = mean), family = "Calibri")+
  labs(x="frequency (proportion of edges)",y="number of clubs")+
  theme_classic()+
  theme(text = element_text(family = "Calibri"),
        legend.position = "none")


ggsave("reciprocal_edges.svg", width = 6.5, height = 4,units = "in", path = paste(getwd(),"/Figs/",sep = ""))



#Figure 3----

#reorganize data according to how they are laid out in the supplemental material
assemble.types <- function(x){
  group_by(x,type)%>%
    count()%>%ungroup()%>%
    filter(!is.na(type))%>%
    mutate(prop = n/sum(n),N=sum(n))%>%
    dplyr::select(type,prop,N)%>%
    spread(type,prop,fill = 0)
}
mutate(all.clubs,types = map(member.types, ~assemble.types(.x)))->types

#types<-dplyr::select(types,-one_of(c("N", "Orders", "Avg.member.order", "Club #","B","H","R")))
#mutate(types, N = map(data, ~nrow(count(.x,Purchaser))))%>%
#  mutate(Orders = map(data, ~nrow(count(.x, Date))))%>%
#  mutate(member.ty = map(mt, ~assemble.types(.x)))%>%
#  mutate(Avg.member.order = map(data,~round(mean(count(.x,Date)$n),2)))%>%
#  dplyr::select(-one_of(c("B","H","R","value", "data","mt","first.few","btw","stab","markov")))%>%
#  unnest()%>%ungroup()%>%
#  replace_na(list("B"=0,"H"=0,"R"=0))%>%
#  arrange(desc(R),B,N)%>%
#  mutate(`Club #`=row_number())%>%
#  inner_join(types)->types

#row_number(fig)

#3-C Stacked Bar Graph----
#colors
yellow<-"#DCE319FF"
green<-"#3CBB75FF"
blue<-"#33638DFF"

dplyr::select(types,name,types)%>%
  unnest()%>%
  replace_na(list(B=0,H=0,R=0))%>%
  arrange(desc(R))->fig



  rownames_to_column(fig,"Club Number")%>%
  mutate(`Club Number` = as.numeric(`Club Number`))%>%
  gather("Member Type","percent composition",4:6)%>%
  ggplot(aes(x = as_factor(`Club Number`) , y = `percent composition`, fill = `Member Type`))+
  geom_bar(stat = "identity")+
  scale_x_discrete(labels = c("1","2*",as.character(3:4),"5*",as.character(6:27),"28*",as.character(29:35)))+
  scale_fill_manual(values = c(yellow,blue,green))+
  theme_classic()+
  labs(x = "club number")+
  theme(legend.position = "bottom",
        axis.text.x = element_text(size = 7),
        text = element_text(family = "Calibri"))

ggsave("3-C.svg", width = 5.5, height = 3,units = "in", path = paste(getwd(),"/Figs/Figure 3/",sep = ""))#save

#3-A Example clubs - networks ----

rescale <- function(weight,low,high){#function to rescale edge weights to appropriate size
  min_d <- min(weight)
  max_d <- max(weight)
  rscl <- ((high-low)*(weight-min_d))/(max_d-min_d)+low
  rscl
}
load("~/Dissertation/Chapter 2 Random Stuff/FC_Shadow_Graphs.Rdata")
load("~/Dissertation/Chapter 2 Random Stuff/BCS_Shadow_Graphs.Rdata")
#Green Arrow
map(foodclub.graphs$greenarrow.networkable,~pluck(.x,1))%>%keep(is.igraph)%>%enframe()%>%
  mutate(el = map(value,~f2(.x)))%>%
  dplyr::select(el)%>%
  unnest() -> ga
filter(all.clubs$member.types$greenarrow.networkable,!is.na(type))%>%
  mutate(cluster = ifelse(type == "R",green,ifelse(type=="H",blue,yellow)))->vga#list of vertices that appear in 2 or more dates, as that's who we categorized
filter(ga,V1 %in% vga$Purchaser & V2 %in% vga$Purchaser)%>%
  dplyr::select(V1,V2)%>%
  group_by(V1,V2)%>%
  count(name = "weight")%>%#remove edges involved with those who are not present in our catagorized data set
  graph_from_data_frame(vertices = vga)->gag
#graintrain.alpena
map(BCS.Graphs$graintrain.alpena,~pluck(.x,1))%>%keep(is.igraph)%>%enframe()%>%
  mutate(el = map(value,~f2(.x)))%>%
  dplyr::select(el)%>%
  unnest() -> gta

filter(all.clubs$member.types$graintrain.alpena,!is.na(type))%>%
  mutate(cluster = ifelse(type == "R",green,ifelse(type=="H",blue,yellow)))->vgta#list of vertices that appear in 2 or more dates, as that's who we categorized
filter(gta,V1 %in% vgta$Purchaser & V2 %in% vgta$Purchaser)%>%
  dplyr::select(V1,V2)%>%
  group_by(V1,V2)%>%
  count(name = "weight")%>%#remove edges involved with those who are not present in our catagorized data set
  graph_from_data_frame(vertices = vgta)->gtag
#Graintrain.2
map(BCS.Graphs$graintrain.2,~pluck(.x,1))%>%keep(is.igraph)%>%enframe()%>%
  mutate(el = map(value,~f2(.x)))%>%
  dplyr::select(el)%>%
  unnest() -> gt2

filter(all.clubs$member.types$graintrain.2,!is.na(type))%>%
  mutate(cluster = ifelse(type == "R",green,ifelse(type=="H",blue,yellow)))->vgt2#list of vertices that appear in 2 or more dates, as that's who we categorized
filter(gt2,V1 %in% vgt2$Purchaser & V2 %in% vgt2$Purchaser)%>%
  dplyr::select(V1,V2)%>%
  group_by(V1,V2)%>%
  count(name = "weight")%>%#remove edges involved with those who are not present in our catagorized data set
  graph_from_data_frame(vertices = vgt2)->gt2g



par(mfrow=c(1,3))
plot.igraph(gag,vertex.color = V(gag)$cluster, vertex.label = "",edge.color = "grey40",edge.arrow.size = rescale(E(gag)$weight,.1,1),edge.width = rescale(E(gag)$weight,.25,2.5), layout = layout.kamada.kawai, edge.curved = TRUE)
plot.igraph(gtag,vertex.color = V(gtag)$cluster, vertex.label = "",edge.color = "grey40",edge.arrow.size = rescale(E(gag)$weight,.1,1),edge.width = rescale(E(gtag)$weight,.25,2.5), layout = layout.kamada.kawai, edge.curved = TRUE)
plot.igraph(gt2g,vertex.color = V(gt2g)$cluster, vertex.label = "",edge.color = "grey40",edge.arrow.size = rescale(E(gag)$weight,.1,1),edge.width = rescale(E(gt2g)$weight,.25,2.5), layout = layout.kamada.kawai, edge.curved = TRUE)

#3-B Transition probs----
lay <- cbind(c(-.5,.5,0),c(-.5,-.5,.5))
#Green Arrow
all.clubs$markov.matrix[[3]]%>%ungroup%>%
  dplyr::select(from,to,prob)%>%
  group_by(from,to)%>%
  summarise(prob = mean(prob))%>%
  spread(to,prob)%>%
  column_to_rownames("from")%>%as.matrix()->m
  graph_from_adjacency_matrix(m,weighted = TRUE)->g

V(g)$color = c(yellow,blue,green)
V(g)$weight =  .5+colSums(m)
plot.igraph(g,edge.curved = TRUE,layout = lay, vertex.size = 50*V(g)$weight,edge.loop.angle=c(3,0,0,0,0,0,0,0,4.75),
            edge.width = E(g)$weight*4,edge.color = "grey55",vertex.label.degree = c(pi/2,pi/2,-pi/4),
            edge.label=round(E(g)$weight, 2),edge.label.font = 2,edge.label.cex=1.5,
            edge.label.color = "black", vertex.label.color = "black",edge.label.family = "Calibri",
            vertex.label.family="Calibri",vertex.label.font = 2,vertex.label.cex=1.5)

#Graintrain Alpena
all.clubs$markov.matrix[[9]]%>%ungroup%>%
  dplyr::select(from,to,prob)%>%
  group_by(from,to)%>%
  summarise(prob = mean(prob))%>%
  spread(to,prob)%>%
  column_to_rownames("from")%>%as.matrix()->m
graph_from_adjacency_matrix(m,weighted = TRUE)->g

V(g)$color =  c(yellow,blue,green)
V(g)$weight= .5+colSums(m)
plot.igraph(g,edge.curved = TRUE,layout = lay, vertex.size = 40*V(g)$weight,edge.loop.angle=c(3,0,0,0,0,4.75),
            edge.width = E(g)$weight*4,edge.color = "grey60",
            edge.label=round(E(g)$weight, 2),edge.label.font = 2,edge.label.cex=1.5,
            edge.label.color = "black", vertex.label.color = "black",edge.label.family = "Calibri",
            vertex.label.family="Calibri",vertex.label.font = 2,vertex.label.cex=1.5)


#Graintrain 2
all.clubs$markov.matrix[[20]]%>%ungroup%>%
  dplyr::select(from,to,prob)%>%
  group_by(from,to)%>%
  summarise(prob = mean(prob))%>%
  spread(to,prob)%>%
  column_to_rownames("from")%>%as.matrix()->m
graph_from_adjacency_matrix(m,weighted = TRUE)->g

V(g)$color =  c(yellow,blue,green)
V(g)$weight= .5+colSums(m)
plot.igraph(g,edge.curved = TRUE,layout = lay, vertex.size = 50*V(g)$weight,edge.loop.angle=c(3,0,0,0,0,0,0,0,4.75),
            edge.width = E(g)$weight*7,edge.color = "grey60",
            edge.label=round(E(g)$weight, 2),edge.label.font = 2,edge.label.cex=1.5, vertex.frame.cex = 2,
            edge.label.color = "black", vertex.label.color = "black",edge.label.family = "Calibri",
            vertex.label.family="Calibri",vertex.label.font = 2,vertex.label.cex=1.5)



#screw space ----
select(types,name,stab)%>%
  unnest()%>%
  gather("r-type","prop",6:9)%>%
  group_by(name)%>%
  summarize(r = sum(prop))%>%
  arrange(desc(r))->r


rownames_to_column(fig,"Club Number")%>%
  mutate(`Club Number` = as.numeric(`Club Number`))%>%
  dplyr::select(`Club Number`,name)%>%
  left_join(dplyr::select(all.clubs))->club.numbers
save(club.numbers, file="club_numbers.Rdata")
