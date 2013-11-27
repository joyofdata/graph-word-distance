setwd("F:\\git-repos\\graph-word-distance")

rm(list = ls())
options(scipen=999)

library(igraph)
library(ggplot2)
library(psych)

g <- read.graph("word-dist-graph-occ5-len5-lv1.graphml", "graphml")

# number of vertices and edges
num_of_vertices <- length(V(g))
num_of_edges <- length(E(g))

deg_dist <- degree(g)

g_ER <- erdos.renyi.game(num_of_vertices, num_of_edges, type = "gnm")
deg_dist_ER <- degree(g_ER)

df <- data.frame(
	deg <- c(deg_dist, deg_dist_ER),
	type <- c(rep('g',length(deg_dist)), rep('ER',length(deg_dist_ER)))
)

# histogram of degree distribution
ggplot(data = df, aes(x = deg, fill = factor(type))) + 
	geom_histogram(binwidth = 1, breaks = 1:10, labels = 1:10, color = I("gray"), position = "dodge") + 
	scale_x_discrete()
	
# words and their degree
df <- data.frame(w = V(g)$name, deg = degree(g))
head(df[order(-df$deg),], n=10)

# summary stats	
describe(deg_dist)
#  var    n mean   sd median trimmed mad min max range skew kurtosis   se
# 1   1 3678 1.92 1.43      1    1.61   0   1  13    12 2.36     7.82 0.02
describe(deg_dist_ER)	
#  var    n mean   sd median trimmed  mad min max range skew kurtosis   se
# 1   1 3678 1.92 1.38      2    1.82 1.48   0  10    10 0.77     0.75 0.02

conn_comp <- clusters(g)

# number of connected components
conn_comp$no

t <- table(conn_comp$csize)
df <- data.frame("size" = as.numeric(names(t)), "freq" = as.vector(t))
df$nodes <- df$size * df$freq
df$share <- df$nodes / sum(df$nodes)

# SNA2B shortest path
g_sp <- shortest.paths(g)
g_sp <- g_sp[g_sp != 0 & g_sp != Inf]
g_diameter <- max(g_sp)
h <- hist(g_sp, breaks = g_diameter, right=F)

# betweenness distribution
# words and their betweenness
df <- data.frame(w = V(g)$name, between = betweenness(g))
head(df[order(-df$between),],n=10)

# words and their closness
df <- data.frame(w = V(g)$name, between = closeness(g))
df <- df[order(-df$between),]
df[1:5,]

# words and their eigen vector centrality
df <- data.frame(w = V(g)$name, evcent = evcent(g)$vector)
df <- df[order(-df$evcent),]
df[1:10,]

# correlations with word length
cor.test(nchar(V(g)$name), degree(g), method="kendall")
cor.test(nchar(V(g)$name), betweenness(g), method="kendall")
cor.test(nchar(V(g)$name), closeness(g), method="kendall")

# largest cliques
cl <- (maximal.cliques(g, min=6))
lapply(cl,function(i) degree(g, i))

# community detection