setwd("F:\\git-repos\\graph-word-distance")

rm(list = ls())
options(scipen=999)

library(igraph)

g <- read.graph("word-dist-graph-occ5-len5-lv1.graphml", "graphml")

# number of connected components
conn_comp <- clusters(g)
conn_comp$no

t <- table(conn_comp$csize)
df <- data.frame("size" = as.numeric(names(t)), "freq" = as.vector(t))
df$nodes <- df$size * df$freq
df$share <- df$nodes / sum(df$nodes)
df

# largest cliques
cl <- (maximal.cliques(g, min=6))
lapply(cl,function(i) degree(g, i))

# community detection
com <- edge.betweenness.community(g, directed=FALSE)
g_com <- induced.subgraph(g,V(g)[membership(com) == which.max(sizes(com))])