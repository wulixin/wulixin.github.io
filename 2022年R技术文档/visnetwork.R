


library(visNetwork)
library(threejs)
g <- make_full_graph(5) %du% make_full_graph(5) %du% make_full_graph(5)
g <- add_edges(g, c(1,6, 1,11, 6, 11))
fc <- cluster_fast_greedy(g)
membership(fc)
sizes(fc)

g <- sample_pa(100, m = 2, directed = FALSE)
eb <- cluster_edge_betweenness(g)

g <- make_full_graph(10) %du% make_full_graph(10)
g <- add_edges(g, c(1,11))
eb <- cluster_edge_betweenness(g)
eb



nodes <- data.frame(id = 1:10, label = paste("Label", 1:10), 
                    group = sample(c("A", "B"), 10, replace = TRUE))
edges <- data.frame(from = c(2,5,10), to = c(1,2,10))

visNetwork(nodes, edges) %>%
  visGroups(groupname = "A", color = "red", shape = "database") %>%
  visGroups(groupname = "B", color = "yellow", shape = "triangle") %>%
  visClusteringByGroup(groups = c("B"), label = "Group : ", 
                       shape = "ellipse", color = "blue", force = TRUE) %>%
  visLegend()
