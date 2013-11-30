setwd("F:\\git-repos\\graph-word-distance")

rm(list = ls())
options(scipen=999)

library(igraph)
library(grid)
library(ggplot2)

g <- read.graph("word-dist-graph-occ5-len5-lv1.graphml", "graphml")

# correlations with word length
cor.test(nchar(V(g)$name), degree(g), method="kendall")
cor.test(nchar(V(g)$name), betweenness(g), method="kendall")
cor.test(nchar(V(g)$name), closeness(g), method="kendall")
cor.test(nchar(V(g)$name), evcent(g)$vector, method="kendall")

# jitter plots for correlation

pushViewport(viewport(layout = grid.layout(4,1)))

df <- data.frame(length = nchar(V(g)$name), degree = degree(g))
pD <- ggplot(data = df, aes(y = degree, x = factor(length))) + 
    geom_jitter(alpha=.5, position = position_jitter(height = .2)) + 
    xlab("word length")
	
df <- data.frame(length = nchar(V(g)$name), betweenness = betweenness(g))
pB <- ggplot(data = df, aes(y = betweenness, x = factor(length))) + 
    geom_jitter() + 
    xlab("word length")

df <- data.frame(length = nchar(V(g)$name), closeness = closeness(g))
pC <- ggplot(data = df, aes(y = closeness, x = factor(length))) + 
	geom_jitter(alpha=.5, position = position_jitter(height = 10^-8.5)) + 
	xlab("word length")

	
df <- data.frame(length = nchar(V(g)$name), evc = evcent(g)$vector)
pE <- ggplot(data = df, aes(y = evc, x = factor(length))) + 
    geom_jitter(alpha=.4, position = position_jitter(height = .05)) + 
    xlab("word length")
	
print(pD, vp=viewport(layout.pos.row = 1, layout.pos.col=1))
print(pB, vp=viewport(layout.pos.row = 2, layout.pos.col=1))
print(pC, vp=viewport(layout.pos.row = 3, layout.pos.col=1))
print(pE, vp=viewport(layout.pos.row = 4, layout.pos.col=1))