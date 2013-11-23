setwd("F:\\git-repos\\graph-word-distance")

rm(list = ls())
options(scipen=999)

library(stringr)
library(sqldf)
library(stringdist)
library(doSNOW)

text_to_word_list <- function(t) {
	t <- tolower(t)
	t <- str_replace_all(t,"\r"," ")
	t <- str_replace_all(t,"\n"," ")
	t <- str_replace_all(t,"[^a-z]"," ")
	tv <- str_split(t," +")[[1]]
	tv
}

word_list_to_word_freq_list <- function(wl,min_len) {
	wl <- data.frame(w = wl, stringsAsFactors = FALSE)
	fl <- sqldf(paste("select w, count(*) as n from wl where length(w) >= ",min_len," group by w order by n desc", sep=""));
	fl
}

calculate_all_string_distances <- function(wl) {
	g <- expand.grid(1:length(wl),1:length(wl))
	g <- as.matrix(g[g[,1] < g[,2],])
	
	N <- 100000
	
	gN <- section_sequence(nrow(g), N)
	
	d <- foreach(i=1:nrow(gN), .packages="stringdist", .combine="rbind") %dopar% (function(i) {
		    res <- matrix(rep(NA,3*(gN[i,2]-gN[i,1]+1)),ncol=3)
			
			for(j in gN[i,1]:gN[i,2]) {
				idx <- j-gN[i,1]+1
				
				res[idx, 1] <- wl[g[j,1]]
				res[idx, 2] <- wl[g[j,2]]
				
				res[idx, 3] <- stringdist(wl[g[j,1]],wl[g[j,2]],method="lv")
			}
			
			res
		})(i)
	
	d
}

section_sequence <- function(len, part_len) {
	parts <- ceiling(len / part_len)
    a <- sapply(1:parts, function(k) (k - 1) * part_len + 1)
	b <- sapply(1:parts, function(k) min(k * part_len, len))
	cbind(a, b)
}

create_edge_list_file_for_gephi <- function(file_name, edges, max_dist) {
	names(edges) <- c("Source","Target","Weight")
	write.table(edges[edges$Weight <= max_dist,] ,file_name,sep=",",row.names=FALSE)
}

extract_and_store_distances <- function(file_name, file_name_suffix, min_word_freq, min_word_length, max_dist_gephi, threads) {
	t <- readChar(file_name, file.info(file_name)$size)
	word_list <- text_to_word_list(t)
	freq_list <- word_list_to_word_freq_list(word_list, min_word_length)

	cl <- makeCluster(threads)
	registerDoSNOW(cl)
	distances <- calculate_all_string_distances(freq_list[freq_list$n >= min_word_freq, "w"])
	stopCluster(cl)

	distances <- data.frame("A" = distances[,1], "B" = distances[,2], "d" = as.numeric(distances[,3]), stringsAsFactors = FALSE)
	fn <- paste("full_distances","_",file_name_suffix,".csv",sep="")
	write.table(distances ,fn, sep=",",row.names=FALSE)

	fn <- paste("gephi_distances","_",file_name_suffix,".csv",sep="")
	create_edge_list_file_for_gephi(fn, distances, max_dist_gephi)
}

extract_and_store_distances("texts\\moby-dick\\moby-dick.txt","lv",3,1,8)