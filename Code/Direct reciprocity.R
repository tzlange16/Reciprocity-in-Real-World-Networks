

#remove all direct reciprocal edges
f <- function(df){
  df%>%
    mutate(cum.x = cumsum(n.x),
           cum.y = cumsum(n.y),
           cum.wd = cumsum(w.dr))%>%
    mutate(surp = n.x - w.dr)%>%
    mutate(cum.dbtw = ifelse(cum.x >= cum.y,
                             cum.y - cum.wd,
                             cum.x - cum.wd))->d
  d$cum.dbtw[nrow(d)]
  if(d$cum.dbtw[nrow(d)]==0) return(tibble(date = rep(d$date,d$surp),type = rep("non-direct", sum(d$surp))))
  if(d$cum.dbtw[nrow(d)]>0) return(tibble(date = rep(d$date,d$surp), type = c(rep("non-direct", sum(d$surp)-d$cum.dbtw[nrow(d)]),rep("direct",d$cum.dbtw[nrow(d)]))))
}

                                 
#function to extract edge list                                  
edge.list <- function(x){
  ifelse(nrow(get.edgelist(x))==0,
         return(tibble()),
         return(as_tibble(get.edgelist(x))%>%
                  mutate(date = get.graph.attribute(x,"Date"))))
}

stability <- function(dta){
  e <- map_df(dta,~f2(.x))#extract edge.lists
  
  colnames(e) <- c("from","to","date")#rename columns
  
  
  e <- group_by(e,from,to,date)%>%
    count()%>%
    mutate(date = as.Date(date))
  
  
full_join(e,e,by = c("from" = "to","to" = "from","date" = "date"))%>%
    replace_na(list(n.x = 0,n.y=0))%>%
    ungroup()%>%
    mutate(w.dr = ifelse(n.x >= n.y,
                         n.y,
                         n.x))%>%group_by(from,to)%>%nest() -> d
    

unnest(d)%>%ungroup()%>%
    group_by(from,date)%>%
    summarize(out.edges = sum(n.x), in.edges = sum(n.y),within.direct = sum(w.dr)) -> w.d
    

mutate(d,remaining.edges = map(data, ~f(.x))) -> d.1

select(d.1,from,to,remaining.edges)%>%
    unnest()%>%
    filter(type == "non-direct")%>%
    select(from,to,date)%>%
    group_by(from,to,date)%>%
    count() -> b 
  
full_join(b,b,by = c("from" = "to","to" = "from","date" = "date"))%>%
    replace_na(list(n.x = 0,n.y=0))%>%
    ungroup()%>%
    mutate(w.dr = ifelse(n.x >= n.y,
                         n.y,
                         n.x))%>%
    group_by(from,date)%>%
    summarize(out.edge = sum(n.x), in.edges = sum(n.y))%>%
    mutate(w.idr = ifelse(out.edge >= in.edges,
                          in.edges,
                          out.edge))%>%
    ungroup()%>%group_by(from,date)%>%
    summarize( within.indirect = sum(w.idr)) -> w.idr

select(d.1,from,to,remaining.edges)%>%
  unnest()%>%
  filter(type == "direct")%>%select(from,to,date)%>%
  group_by(from,to,date)%>%
  count()%>%
  ungroup()%>%group_by(from,date)%>%
  summarize(between.direct = sum(n)) -> btw.dr
 

full_join(w.d,w.idr)%>%full_join(btw.dr)%>%
  replace_na(list(between.direct = 0,within.direct = 0,within.indirect = 0))%>%
  group_by(from)%>%
  summarise_at(.vars = c("out.edges","in.edges","within.direct","within.indirect","between.direct"), .funs = sum)%>%
  mutate(btw.idr = ifelse(out.edges >= in.edges,
                          in.edges - within.direct-within.indirect-between.direct,
                          out.edges - within.direct-within.indirect-between.direct)) -> test


full_join(w.d,w.idr)%>%full_join(btw.dr)%>%
  replace_na(list(between.direct = 0,within.direct = 0,within.indirect = 0))%>%
  mutate(st.r = within.direct+within.indirect+between.direct)%>%
  group_by(from)%>%
  mutate(av = out.edges-st.r)%>%
  nest()%>%
  mutate(available = map(data, ~tibble(available=rep(.x$`date`,.x$av))))%>%
  full_join(test)%>%
  mutate(actual.dates = map(available, ~slice_tail(.x, n = btw.idr)))%>%
  select(actual.dates)%>%
  unnest(cols = actual.dates)%>%
  group_by(from,available)%>%
  count(name = "between.indirect")%>%
  full_join(w.d, by = c("from" = "from", "available" = "date"))%>%
  full_join(w.idr, by = c("from" = "from", "available" = "date"))%>%
  full_join(btw.dr, by = c("from" = "from", "available" = "date"))%>%
  replace_na(list(between.direct = 0,within.direct = 0,within.indirect = 0, between.indirect = 0))%>%
  mutate(check = out.edges >= within.direct + within.indirect + between.direct + between.indirect)%>%
  group_by(available)%>%
  summarize_at(.vars = c("out.edges","within.direct","within.indirect","between.direct","between.indirect"), .funs = sum)%>%
  mutate(pwd = within.direct/out.edges,
         pwi = within.indirect/out.edges,
         pbd = between.direct/out.edges,
         pbi = between.indirect/out.edges)%>%
  ungroup()%>%
  summarize(CV.pwd = sd(pwd)/mean(pwd)*100, 
            CV.pwi = sd(pwi)/mean(pwi)*100,
            CV.pbd = sd(pbd)/mean(pbd)*100,
            CV.pbi = sd(pbi)/mean(pbi)*100,
            pwd = sum(within.direct)/sum(out.edges),
            pwi = sum(within.indirect)/sum(out.edges),
            pbd = sum(between.direct)/sum(out.edges),
            pbi = sum(between.indirect)/sum(out.edges))->test 

  return(test)
}







