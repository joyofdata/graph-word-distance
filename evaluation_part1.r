setwd("F:\\git-repos\\graph-word-distance")

rm(list = ls())
options(scipen=999)

library(igraph)
library(ggplot2)
library(psych)
library(grid)

g <- read.graph("word-dist-graph-occ5-len5-lv1.graphml", "graphml")

# number of vertices and edges
num_of_vertices <- length(V(g))
num_of_edges <- length(E(g))
diameter(g, directed=FALSE)

# ER graph for comparison
#g_ER <- erdos.renyi.game(num_of_vertices, num_of_edges, type = "gnm")
#g_ER <- induced.subgraph(g_ER, V(g_ER)[which(degree(g_ER) >= 1)])

# histogram and data of degree distribution

g_sp <- shortest.paths(g)
g_sp <- g_sp[g_sp != 0 & g_sp != Inf]

pushViewport(viewport(layout = grid.layout(1,3)))

p_deg <- ggplot(data = data.frame(degree = degree(g)), aes(x = degree)) + 
	geom_histogram(binwidth = 1, breaks = 1:10, labels = 1:10, color = I("gray"), position = "dodge") + 
	scale_x_discrete() + 
	ggtitle("degree")
	
p_sp <- ggplot(data = data.frame(sp = g_sp), aes(x = sp)) + 
    geom_histogram(binwidth = 2, color = I("gray"), position = "dodge") + 
    ggtitle("shortest path")
	
p_betw <- ggplot(data = data.frame(betw = betweenness(g)), aes(x = betw)) + 
    geom_histogram(binwidth=5, breaks=(1:10*5), color = I("gray"), position = "dodge") + 
    ggtitle("betweenness")

	
print(p_deg, vp=viewport(layout.pos.row = 1, layout.pos.col=1))
print(p_sp, vp=viewport(layout.pos.row = 1, layout.pos.col=2))
print(p_betw, vp=viewport(layout.pos.row = 1, layout.pos.col=3))

describe(degree(g))
describe(g_sp)
describe(betweenness(g))