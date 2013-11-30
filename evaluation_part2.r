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
