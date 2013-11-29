setwd("F:\\git-repos\\graph-word-distance")

rm(list = ls())
options(scipen=999)

library(igraph)

g <- read.graph("word-dist-graph-occ5-len5-lv1.graphml", "graphml")

# words and their degree
df <- data.frame(w = V(g)$name, deg = degree(g))
head(df[order(-df$deg),], n=10)

# words and their betweenness
df <- data.frame(w = V(g)$name, between = betweenness(g))
head(df[order(-df$between),],n=10)

# words and their closeness
df <- data.frame(w = V(g)$name, close = closeness(g))
head(df[order(-df$close),],n=10)

# words and their eigen vector centrality
df <- data.frame(w = V(g)$name, evcent = evcent(g)$vector)
df <- df[order(-df$evcent),]
df[1:10,]

# correlations with word length
cor.test(nchar(V(g)$name), degree(g), method="kendall")
cor.test(nchar(V(g)$name), betweenness(g), method="kendall")
cor.test(nchar(V(g)$name), closeness(g), method="kendall")
cor.test(nchar(V(g)$name), evcent(g)$vector, method="kendall")


conn_comp <- clusters(g)

# number of connected components
conn_comp$no

t <- table(conn_comp$csize)
df <- data.frame("size" = as.numeric(names(t)), "freq" = as.vector(t))
df$nodes <- df$size * df$freq
df$share <- df$nodes / sum(df$nodes)



# betweenness distribution


# words and their closness
df <- data.frame(w = V(g)$name, between = closeness(g))
df <- df[order(-df$between),]
df[1:5,]





# largest cliques
cl <- (maximal.cliques(g, min=6))
lapply(cl,function(i) degree(g, i))

# community detection
com <- edge.betweenness.community(g, directed=FALSE)
g_com <- induced.subgraph(g,V(g)[membership(com) == which.max(sizes(com))])