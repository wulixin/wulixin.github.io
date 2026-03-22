

##############################社交网络数据分析
library(tidygraph)
library(igraph)
library(dplyr)
file = "C:\\DataTK\\results.csv"
file = "//Users//wulixin//Desktop//可视化.xlsx"
pbi <- read.csv(file, sep = ",", header = TRUE, skip=1)
# Data preparation
links <- pbi[,c("UserName","WorkspaceName")]
colnames(links) <- c("from","to")
# Nodes 
nodes1 <-  pbi %>% group_by(WorkspaceName) %>% summarise(n = n())
nodes1$group <- "WS"
colnames(nodes1) <- c("id","size", "group")
nodes2 <-  pbi %>% group_by(UserName) %>% summarise(n = n())
nodes2$group <- "US"
colnames(nodes2) <- c("id","size","group")
nodes <- rbind(nodes1, nodes2)
# create plot
net <- graph_from_data_frame(d=links, vertices=nodes, directed=TRUE) 
plot(net, edge.arrow.size=.5,vertex.label=V(net)$group)