setwd("F:\\git-repos\\graph-word-distance")

rm(list = ls())
options(scipen=999)

library(igraph)
library(ggplot2)

g <- read.graph("word-dist-graph-occ5-len5-lv1.graphml", "graphml")

# correlation of degrees of directly connected vertices
mdeg <- (apply(get.edges(g,E(g)),1,function(v)degree(g,v)))
cor.test(mdeg[1,],mdeg[2,],method="kendall")
cor.test(mdeg[1,],mdeg[2,],method="pearson")

# average degree of nodes (Y) neighbouring a node of degree (X)
ldeg <- lapply(neighborhood(g,order=1),function(v)degree(g,v))
ldeg2 <- list()
dump <- sapply(1:max(degree(g)), function(n) ldeg2[[n]] <<- c("sum" = 0, "N" = 0))
dump <- lapply(ldeg, function(d){
    ldeg2[[d[1]]]["sum"] <<- ldeg2[[d[1]]]["sum"] + sum(d[-1]);
    ldeg2[[d[1]]]["N"] <<- ldeg2[[d[1]]]["N"] + length(d)-1;
})

mdeg <- matrix(unlist(ldeg2),ncol=2,byrow=T)
colnames(mdeg) <- c("sum","N")

ggplot(data = data.frame(x=1:max(degree(g)), y=mdeg[,"sum"]/mdeg[,"N"]),aes(x,y)) + 
	geom_point() + scale_x_continuous(breaks=1:13) + 
	scale_y_continuous(breaks=1:10) + 
	xlab("degree") + 
	ylab("avg degree of neighbouring nodes") +
	ggtitle("average degree of nodes (Y) neighbouring a node of degree (X)")