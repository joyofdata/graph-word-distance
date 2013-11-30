setwd("F:\\git-repos\\graph-word-distance")

rm(list = ls())
options(scipen=999)

library(igraph)

g <- read.graph("word-dist-graph-occ5-len5-lv1.graphml", "graphml")

# correlations with word length
cor.test(nchar(V(g)$name), degree(g), method="kendall")
cor.test(nchar(V(g)$name), betweenness(g), method="kendall")
cor.test(nchar(V(g)$name), closeness(g), method="kendall")
cor.test(nchar(V(g)$name), evcent(g)$vector, method="kendall")
