#============================================
# Detecting reciprocity in empirical networks
#             Master Script
#============================================

#Required Packages
library(tidyverse)

load("Data/FC_sensitivity.Rdata")



extrafont::loadfonts(device = "win")

group_by(sense,recip,analysis)%>%
  summarize(mean = mean(value))%>%
  filter(analysis=="sens")

sense%>%
  mutate(analysis = ifelse(analysis=="og","Share","Time"))%>%
  mutate(order = ifelse(recip == "pwd",1,
                        ifelse(recip == "pbd",2,
                               ifelse(recip=="pwi",3,
                                      ifelse(recip == "pbi",4,5)))))%>%
ggplot(aes(x=analysis,y=value*100,fill=analysis))+
  geom_violin()+
    stat_summary(fun = mean, geom = "point",shape = 18, size = 4)+
    facet_grid(.~as_factor(order))+
  theme_bw()+
  labs(x="",y="% of total edges",fill = "Edge \nDirection \nBasis")+
  theme(legend.position = "none",
        strip.text = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.y = element_text(family = "Arial",size=12),
        axis.title.y = element_text(family = "Arial",size=14),
        axis.text.x = element_blank(),
        axis.title.x = element_blank())
 
mutate(foodclub_edges,parent_id=paste(parent_id,".networkable",sep = ""))%>%
  filter(parent_id %in% Chapter_1$name)->foodclub_edges
  
sense_edges <- foodclub_edges
save(sense_edges,file = "Data/Sensitivity_model.Rdata")

select(sense_edges,parent_id,edges)%>%
  unnest()%>%
  unnest()->model_edges
  
model_edges%>%group_by(parent_id,from)%>%
  count(name = "out.degree")->out
model_edges%>%group_by(parent_id,to)%>%
  count(name = "in.degree")->in.d  
  
inner_join(out,in.d,by=c("from"="to","parent_id"))->sense_model

summary(lme4::lmer(out.degree ~ in.degree + (1|parent_id),data = sense_model))



#assign edges as directly reciprocal and indirectly reciprocal direct reciprocal edges
f <- function(df){
  df%>%
    mutate(cum.x = cumsum(n.x),#cumulative sum of out.degrees
           cum.y = cumsum(n.y),#cumulative sum of in.degrees
           cum.wd = cumsum(w.dr))%>%#cumulative sum of wihin order direct reciprocity
    mutate(surp = n.x - w.dr)%>%#calculate difference between out degrees and within order direct reciprocity
    mutate(cum.dbtw = ifelse(cum.x >= cum.y,#if cumulative out degrees are greater than or equal to in degrees
                             cum.y - cum.wd,#BDR is equal to cumulative in degrees minus cumulative WDR
                             cum.x - cum.wd))->d#else: BDR is equal to cumulative out degrees minus cumulative WDR
  d$cum.dbtw[nrow(d)]
  if(d$cum.dbtw[nrow(d)]==0) return(tibble(date = rep(d$date,d$surp),type = rep("non-direct", sum(d$surp))))#if no between order reciprocity is present, return all remaining edges that can't be BDR
  if(d$cum.dbtw[nrow(d)]>0) return(tibble(date = rep(d$date,d$surp), type = c(rep("non-direct", sum(d$surp)-d$cum.dbtw[nrow(d)]),rep("direct",d$cum.dbtw[nrow(d)]))))#if between order, return edges as number of BDR starting with earliest and number of remaining edges that are possible indirect
}

stability <- function(dta){
  dta <- unnest(dta)
  e <- rename(dta,date = order_date)
  
  e <- group_by(e,from,to,date)%>%#group edge list by dyad from each vertex's perspective per date
    count()%>%#count number of edges from each vertex to another vertex by date
    mutate(date = as.Date(date))#make date column data type "date"
  
  
  full_join(e,e,by = c("from" = "to","to" = "from","date" = "date"))%>%#join counts to each other to line up in and out degrees with in a dyad
    replace_na(list(n.x = 0,n.y=0))%>%#replace NAs with 0s
    ungroup()%>%#ungroup data
    mutate(w.dr = ifelse(n.x >= n.y,#if else function to determine within order direct reciprocity per date
                         n.y,
                         n.x))%>%
    group_by(from,to)%>%#group by dyad from each vertex's point of view
    nest() -> d#nest data to use later on
  
  #Within order direct reciprocity - WDR
  unnest(d)%>%ungroup()%>%#unnest and ungroup
    group_by(from,date)%>%#regroup by from vertex and date
    summarize(out.edges = sum(n.x), in.edges = sum(n.y),within.direct = sum(w.dr)) -> w.d#count total in and out edges and WDR
  
  
  mutate(d,remaining.edges = map(data, ~f(.x))) -> d.1#function to assign edges as definitely BDR or possible indirectly reciprocal using cumulative sums
  
  #within order indirect reciprocity - WIR
  select(d.1,from,to,remaining.edges)%>%#select all remaining non-WDR edges
    unnest()%>%#unnest
    filter(type == "non-direct")%>%#filter edges that can't possibly be within order direct
    select(from,to,date)%>%#remove type variable
    group_by(from,to,date)%>%#group by from, to, and date
    count() -> b #preliminary assignment as b
  
  full_join(b,b,by = c("from" = "to","to" = "from","date" = "date"))%>%#full join b to itself so that each from vertex has a calculatable in and out degree
    replace_na(list(n.x = 0,n.y=0))%>%#replace NA's with 0
    ungroup()%>%#ungroup
    group_by(from,date)%>%#regroup by from vertex
    summarize(out.edge = sum(n.x), in.edges = sum(n.y))%>%#count number of non-directly reciprocated in and out edges there are
    #assign within order indirect reciprocity as the minimum of out and in edges at a given time
    mutate(w.idr = ifelse(out.edge >= in.edges,#if the count of out edges is equal to or greater than in.edges
                          in.edges,#WIR is equal to in edges as it is the smaller of the two quantities
                          out.edge))%>%#WIR is equal to out edges as it is the smaller quantity
    ungroup()%>%group_by(from,date)%>%#group by from and date
    summarize( within.indirect = sum(w.idr)) -> w.idr# assign w.idr as within.indirect column name
  
  #between order direct reciprocity - BDR
  select(d.1,from,to,remaining.edges)%>%#select remaining edges for calculation of between direct
    unnest()%>%#unnest
    filter(type == "direct")%>%select(from,to,date)%>%#filter to definitely BDR
    group_by(from,to,date)%>%#group by from vertex, to vertex, and date
    count()%>%#count total BDR edges by date
    ungroup()%>%group_by(from,date)%>%#regroup by from vertex and date and Date
    summarize(between.direct = sum(n)) -> btw.dr#assign as BDR
  
  #between order indirect reciprocity - BIR
  full_join(w.d,w.idr)%>%full_join(btw.dr)%>%#join WDR, BDR, & WIR together
    replace_na(list(between.direct = 0,within.direct = 0,within.indirect = 0))%>%#replace NA's with zeros
    group_by(from)%>%#group by from vertexes
    summarise_at(.vars = c("out.edges","in.edges","within.direct","within.indirect","between.direct"), .funs = sum)%>%#sum all variables
    mutate(btw.idr = ifelse(out.edges >= in.edges,#calculate BIR if out edges are greater than or equal to in edges
                            in.edges - within.direct-within.indirect-between.direct, #BIR is equal to in edges - all other reciprocitys
                            out.edges - within.direct-within.indirect-between.direct)) -> b.idr #Else BIR is equal to out edges minus all other reciprocitiys 
  
  
  #create full results output
  full_join(w.d,w.idr)%>%full_join(btw.dr)%>%#join WDR, BDR, & WIR together
    replace_na(list(between.direct = 0,within.direct = 0,within.indirect = 0))%>%#replace NAs with zeros
    mutate(st.r = within.direct+within.indirect+between.direct)%>%#subtotal reciprocity
    group_by(from)%>%#group by from
    mutate(av = out.edges-st.r)%>%#count number of available edges for BIR
    nest()%>%#nest according to from vertex
    mutate(available = map(data, ~tibble(available=rep(.x$`date`,.x$av))))%>%#create new nested column of all available dates edges that could be BIR
    full_join(b.idr)%>%#join with BIR set
    mutate(actual.dates = map(available, ~slice_tail(.x, n = btw.idr)))%>%#slice tail end of possible dates by the number of actual BIR edges
    select(actual.dates)%>%#select available dates
    unnest(cols = actual.dates)%>%#unnest 
    group_by(from,available)%>%#group by available date and from vertex
    count(name = "between.indirect")%>%#count so we have exact counts of BIR per order
    full_join(w.d, by = c("from" = "from", "available" = "date"))%>%#full join with WDR
    full_join(w.idr, by = c("from" = "from", "available" = "date"))%>%#full join with WIR
    full_join(btw.dr, by = c("from" = "from", "available" = "date"))%>%#full join with BDR
    replace_na(list(between.direct = 0,within.direct = 0,within.indirect = 0, between.indirect = 0))%>%#replace all NAs
    mutate(check = out.edges >= within.direct + within.indirect + between.direct + between.indirect)%>%#make sure that all recips are either equal to or lesser than total out-degrees
    group_by(available)%>%#group by date
    summarize_at(.vars = c("out.edges","within.direct","within.indirect","between.direct","between.indirect"), .funs = sum)%>%#count total amount of reciprocity of all members combined per order
    #Translate raw counts into percentages of each orders edges
    mutate(pwd = within.direct/out.edges,
           pwi = within.indirect/out.edges,
           pbd = between.direct/out.edges,
           pbi = between.indirect/out.edges)%>%
    ungroup()%>%#ungroup by date
    summarize(#calculate total coefficient of variation of reciprocity percentanges
      CV.pwd = sd(pwd)/mean(pwd)*100, 
      CV.pwi = sd(pwi)/mean(pwi)*100,
      CV.pbd = sd(pbd)/mean(pbd)*100,
      CV.pbi = sd(pbi)/mean(pbi)*100,
      #calculate total percentages of all reciprocity types all orders combined
      pwd = sum(within.direct)/sum(out.edges),
      pwi = sum(within.indirect)/sum(out.edges),
      pbd = sum(between.direct)/sum(out.edges),
      pbi = sum(between.indirect)/sum(out.edges))->output #assign final counts and CVs
  
  return(output)
}








