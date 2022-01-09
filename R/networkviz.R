#network
# nodes coloured by reference or citation 

library(networkD3)
library(tidyverse)
library(here)
network <- readRDS(paste0(here(),"/inst/shiny-examples/citationchaser/network.RDS"))
head(network)

input_ids=c("050-168-647-208-295", "109-839-221-635-410")
#input$article_ids
tmp1<-data.frame("IDs"=network$input_lensID, "Group"= network$type)
tmp2<-data.frame("IDs"=network$reference_lensID, "Group"= network$type)
tmp=rbind(tmp1,tmp2)
Nodes=unique(tmp)
Nodes=Nodes %>% 
  mutate(Group2=ifelse(IDs%in%input_ids, 0, Group)) %>% 
  mutate(Group2=as.character(Group2)) %>% 
  mutate(Group2=dplyr::recode(Group2, "0"="input", "1"="reference", "2"="citation"))

Nodes2<-Nodes %>% 
  filter(!Group2=="input")

inputs<-Nodes %>% 
  filter(Group2=="input") %>% 
  distinct(.,IDs,Group2, .keep_all=TRUE)

Nodes=rbind(inputs, Nodes2)

# make a links data frame using the indexes (0-based) of nodes in 'nodes'
links <- data.frame(source = match(network$input_lensID, Nodes$IDs) - 1,
                    target = match(network$reference_lensID,Nodes$IDs) - 1)

links<-links %>% 
  drop_na()

n_net<-forceNetwork(Links = links, Nodes = Nodes, Source = "source",
                        Target = "target", NodeID ="IDs", Group="Group2", 
                    linkColour = "black",
                    
                    opacity = 1, opacityNoHover = 1, zoom=TRUE, legend=TRUE,colourScale = JS('d3.scaleOrdinal().range(["black", "#a50026","#4575b4"]);'))


n_net$x$nodes$hyperlink<-paste0('https://www.lens.org/lens/search/scholar/list?q=lens_id:', Nodes$IDs, '&p=0&n=10&s=_score&d=%2B&f=false&e=false&l=en&authorField=author&dateFilterField=publishedYear&orderBy=%2B_score&presentation=false&stemmed=true&useAuthorId=false')

n_net$x$options$clickAction = 'window.open(d.hyperlink)'
n_net
