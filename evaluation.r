setwd("F:\\git-repos\\graph-word-distance")

rm(list = ls())
options(scipen=999)

library(igraph)
library(ggplot2)

g <- read.graph("word-dist-graph-occ5-len5-lv1.graphml", "graphml")

# number of vertices and edges
length(V(g))
length(E(g))

# histogram of degree distribution
ggplot(data = data.frame(deg = degree(g)), aes(x = deg)) + geom_histogram(binwidth = 1, breaks = 1:10, labels = 1:10, color = I("gray")) + scale_x_discrete()

