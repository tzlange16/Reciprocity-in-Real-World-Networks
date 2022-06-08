# Lange, Smolla, Waring 2021
# Figure Script
library(tidyverse)
library(igraph)
library(cowplot)
library(extrafont)
loadfonts(device = "win")
load("Data/Reciprocity.Rdata")
source("Code/reciprocity counts and stability.R")

names(all.clubs$member.types)<-all.clubs$Club

select(all.clubs,`Club Number`,degree.balance)%>%
  unnest()%>%
  group_by(`Club Number`)%>%
  summarize(start = min(Date),end = max(Date))%>%
  mutate(time = lubridate::interval(start,end)/lubridate::years(1))%>%
  summarize(sum(time))

select(all.clubs,`Club Number`,degree.balance)%>%#select degree balance data
  unnest()%>%#unnest
  group_by(`Club Number`,Purchaser)%>%#group by club name and purchaser
  summarize_at(.vars = c("In.Degree","Out.Degree"),.funs = sum)->RE#aggregate each purchaser's in and out degree

rename(RE,`Out Degree` = Out.Degree,
       `In Degree` = In.Degree)->RE

lme4::lmer(`Out Degree`~`In Degree` + (1|`Club Number`) ,data=RE)->re.model

summary(re.model)

#Figure 1 (get from Tim)----
#set graphical parameters
par(mfrow=c(1,4),mar = c(5,.5,4,.5),family = "Arial")
#simulate sample order
#create some items
items <- c("A","B","C","D","E","F","H","I","J")
#create some some purchasers
purchasers <- 1:9
#create some purchases
set.seed(1)
matrix(rbinom(81,1,.4),ncol=9,nrow=9)->m#adjacency of ones and zeros
colnames(m)<-items#name columns as items
rownames(m)<-purchasers#name rows as purchasers
#make graph of purchases
g<-graph_from_incidence_matrix(m)#create graph from matrix
#plot first graph as bipartite network of items and purchasers
plot.igraph(g,layout = layout.bipartite(g),vertex.label = NA,edge.color = "grey25", vertex.shape = c(rep("circle",9),rep("square",9)),vertex.color = c(rep("red",9),rep("blue",9)))
legend(-1,-1,horiz = TRUE,legend = c("Members","Items"),pch = c(21,22),cex=1.75,border = "white",bg = "transparent",box.lty=0,pt.bg = c("red","blue"),pt.cex = 2.5)#add legend
text(-1,1.35,labels = "A",cex=2)#add label
bipartite.projection(g)$proj1 ->up#project bipartite into item and member networks
get.adjacency(up,attr = "weight")->m2#get the adjacency of member network to make new undirected member network
graph_from_adjacency_matrix(m2,mode = "undirected")->up#create undirected member network
layout.fruchterman.reingold(up)->coords#store coordinates of member network for identical layout
as.matrix(m2)%>%#
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
text(-1,1.35,labels = "B",cex=2)
graph_from_edgelist(edge.list)%>%
  simplify(remove.multiple = FALSE)%>%
  plot.igraph(layout = coords, edge.arrow.size = .5, vertex.label = NA, vertex.color = "red",edge.color = "grey25")
text(-1,1.35,labels = "C",cex=2)
lay <- cbind(c(-.5,.5,0),c(-.5,-.5,.5))
m <- matrix(1,nrow=3,ncol=3)
row.names(m)<-c("B","H","R")
colnames(m)<-c("B","H","R")
g <- graph_from_adjacency_matrix(m)
set.seed(1)
edge.arrow.width <- edge.arrow.size <- edge.width <- rnorm(9,2,.5)
edge.lty <- c(2.25,0,0,0,.75,0,0,0,4.75)
plot(g, edge.lty=0, edge.arrow.size=0, layout=lay,vertex.size=75,
     vertex.shape="none",)
for (e in seq_len(ecount(g))) {
  graph2 <- delete.edges(g, E(g)[(1:ecount(g))[-e]])
  plot(graph2,edge.arrow.width=edge.arrow.size[e]-1, edge.width = edge.width[e],edge.loop.angle = edge.lty[e],edge.arrow.size=.5,
       edge.curved=TRUE, layout=lay,edge.color = "grey25",vertex.size=75,vertex.shape="none",
       vertex.label=NA, add=TRUE)
}
plot(delete.edges(g, E(g)[(1:ecount(g))]), edge.lty=0, edge.arrow.size=0, layout=lay,vertex.size=75,
     vertex.label.color = c("white","black","black"), vertex.label.cex = 2,
     vertex.color = c("black","white","grey50"),vertex.label.degree = c(pi/2,pi/2,-pi/4),vertex.label.family = "Arial",add=TRUE)
text(-1,1.70,labels = "D",cex=2)

#Figure 2----
select(all.clubs,`Club Number`,stab)%>%#select out reciprocity proportions &stability
  unnest()%>%#unnest tibble
  select(`Club Number`,pwd,pbd,pwi,pbi)%>%#select club name and reciprocity type proportions
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

group_by(fig,`T-D`)%>%summarize(mean = mean(Value)*100)%>%
  right_join(fig)%>%
ggplot(aes(y=Value*100,x=as_factor(`T-D`)))+
  geom_violin()+
  #geom_jitter(color = "grey25",alpha = .75,width = .1,shape = 16,height = 0)+scale_y_continuous(limits = c(-1,80),breaks = c(0,20,40,60,80),labels=c("  0"," 20", " 40"," 60"," 80"))+
  geom_point(aes(y=mean), shape = 18,size=4)+
  theme_classic()+
  labs(x="",y="% of total edges")+
  theme(axis.text.x = element_blank(),
        legend.position = "none",
        strip.text = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(family = "Arial"),
        axis.title.y = element_text(family = "Arial"))->f1

ggsave("raw_figs/reciprocity_scatter_raw.pdf",height = 2.5,width = 5,units = "in",device = cairo_pdf)

select(all.clubs,`Club Number`,stab)%>%#select out reciprocity proportions &stability
  unnest()%>%
  select(`Club Number`,contains("CV"))%>%
  gather("Key","Value",2:5)%>%#transform data to long form
  mutate(time = ifelse(Key %in% c("CV.pwd","CV.pwi"),#define time scale of reciprocity:
                       "W",#within order
                       "B"),#between order
         direction = ifelse(Key %in% c("CV.pwd","CV.pbd"),#define directness of reciprocity
                            "DR",#direct reciprocity
                            "IR"))%>%#indirect reciprocity
  mutate(time = ifelse(Key == "pur",#define unreciprocated edges temporally 
                       "U",#named not for figure labeling
                       time),
         direction = ifelse(Key == "pur",#define unreciprocated edges directionally
                            "R",#named reciprocated for figure
                            direction))%>%
  unite("T-D",time,direction,sep = "")%>%
  mutate(`T-D` = as_factor(`T-D`))->fig.2

group_by(fig.2,`T-D`)%>%
  summarize(mean = mean(Value,na.rm = TRUE),min = min(Value,na.rm = TRUE),max=max(Value,na.rm = TRUE))%>%
  bind_rows(tibble(`T-D`="UR",mean=NA,min=NA,max=NA))%>%
  left_join(fig.2)%>%
  ggplot(aes(y=Value,x=reorder(`T-D`,mean)))+
  geom_violin()+
  geom_point(aes(y=mean), shape = 18,size=4)+
  theme_classic()+
  labs(x="",y=bquote("c"["v"]))+
  theme(axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text = element_text(family = "Arial"),
        axis.title = element_text(family = "Arial"))->f2
plot_grid(f1,f2,ncol=1,align="v",axis = "lh",labels=c("A","B"),vjust = 1,scale=1)
ggsave2("raw_figs/recip_combined_raw.pdf",height=5,width = 5,units = "in", device = cairo_pdf)


#ggsave("raw_figs/reciprocity_cv_raw.pdf",height = 2.5,width = 5, units = "in",device = cairo_pdf)



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

dplyr::select(types,`Club Number`,types)%>%
  unnest()%>%
  replace_na(list(B=0,H=0,R=0))%>%
  arrange(desc(R))->fig


 
  gather(fig,"Member Type","percent composition",3:5)%>%
  ggplot(aes(x = as_factor(`Club Number`) , y = `percent composition`, fill = `Member Type`))+
  geom_bar(stat = "identity",color="black",size=.05)+
  scale_fill_manual(values = c("black","white","grey50"),labels = c("beneficiary","helper","reciprocator"))+
    scale_y_continuous(labels = c(0,25,50,75,100))+
  theme_classic()+
  labs(x = "club",
       fill = "",
       )+
  theme(legend.position = "bottom",
        axis.text.x = element_blank(),
        text = element_text(family = "Arial",size=14),
        axis.title = element_text(size=16),
        legend.text = element_text(size=16)
        )

ggsave("raw_figs/3-C.pdf", width = 5.5, height = 3,units = "in")#save

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
plot.igraph(g,edge.curved = TRUE,layout = lay, vertex.size = 40*V(g)$weight,edge.loop.angle=c(3,0,0,0,0,0,0,0,4.75),
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


all.clubs$degree.balance[[1]]$SPR->x



k_means(all.clubs$degree.balance[[1]]$SPR)


mutate(all.clubs, centers = map(degree.balance, ~pluck(k_means(.x$SPR),"centers")))%>%
  select(`Club Number`,centers)%>%
  unnest()%>%
  spread(type,center)

select(all.clubs,`Club Number`, member.types)%>%
  unnest()%>%
  group_by(`Club Number`)%>%
  count(type)%>%
  spread(type,n,fill=0)%>%
  rename(beneficiaries = B,
         helpers = H,
         reciprocators = R, 
         Unclassified=`<NA>`)


lay <- cbind(c(-.5,.5,0),c(-.5,-.5,.5))

m <- matrix(1,nrow=3,ncol=3)
row.names(m)<-c("B","H","R")
colnames(m)<-c("B","H","R")
g <- graph_from_adjacency_matrix(m)
plot.igraph(g,layout = lay,edge.loop.angle=c(3,0,0,0,0,0,0,0,4.75),edge.curved=TRUE,vertex.size = 50, vertex.label.color = "black",vertex.label.cex = 1,
            edge.color = "grey25",vertex.color = "white",vertex.label.degree = c(pi/2,pi/2,-pi/4),edge.label.family = "Calibri")


plot.igraph(g,edge.curved = TRUE,layout = lay, vertex.size = 50*V(g)$weight,edge.loop.angle=c(3,0,0,0,0,0,0,0,4.75),
            edge.width = E(g)$weight*4,edge.color = "grey55",vertex.label.degree = c(pi/2,pi/2,-pi/4),
            edge.label=paste(E(g)$),edge.label.font = 2,edge.label.cex=1.5,
            edge.label.color = "black", vertex.label.color = "black",edge.label.family = "Calibri",
            vertex.label.family="Calibri",vertex.label.font = 2,vertex.label.cex=1.5)

par(mfrow=c(1,1),mar = c(5,0,4,0),family = "Arial")
select(all.clubs,markov.matrix)%>%
  unnest()%>%
  group_by(from,to)%>%
  summarize(sum=sum(prob),n=n())%>%
  mutate(mean=round(sum/n,2)*100)->edges
group_by(edges,to)%>%
  summarize(weight = sum(mean))->verticies

graph_from_data_frame(edges,vertices = verticies)->g

plot.igraph(g,edge.curved = TRUE,layout = lay, vertex.size = V(g)$weight,edge.loop.angle=c(3,0,0,0,0,0,0,0,4.75),
            edge.color = "grey55",vertex.label.degree = c(pi/2,pi/2,-pi/4),vertex.color = c("black","white","grey50"),vertex.label.color = c("white","black","black"),
            vertex.label.cex = c(2,2,4),
            edge.label=paste(E(g)$mean,"%",sep = ""),edge.label.font = 2,edge.label.cex=1.25,
            edge.label.color = "black", vertex.label.color = "black",edge.label.family = "Calibri",
            vertex.label.family="Calibri",vertex.label.font = 2,vertex.label.cex=1.5)
