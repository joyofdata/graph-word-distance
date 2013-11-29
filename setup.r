setwd("F:\\git-repos\\graph-word-distance")

vtexts <- c("texts\\moby-dick.txt","texts\\great-expectations.txt","texts\\david-copperfield.txt")

rm(list = ls())
options(scipen=999)

##############################################
# functions
##############################################

library(stringr)
library(sqldf)
library(stringdist)
library(doSNOW)

# takes a string and turns it into vector of words
text_to_word_list <- function(t) {
	t <- tolower(t)
	t <- str_replace_all(t,"\r"," ")
	t <- str_replace_all(t,"\n"," ")
	t <- str_replace_all(t,"[^a-z]"," ")
	tv <- str_split(t," +")[[1]]
	tv
}

# takes a vector of words and calculates a frequency table (data frame)
word_list_to_word_freq_list <- function(wl) {
	wl <- data.frame(w = wl, stringsAsFactors = FALSE)
	fl <- sqldf(paste("select w, count(*) as n from wl group by w order by n desc", sep=""));
	fl
}

# calculates the string distances using the specified algorithm for all
# possible unordered word combinations. To speed this up I perform
# parallel computing
calculate_all_string_distances <- function(wl, dist_type, qgram = 2) {
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
				
				res[idx, 3] <- stringdist(wl[g[j,1]],wl[g[j,2]],method=dist_type,q=qgram)
			}
			
			res
		})(i)
	
	d
}

# let's assume that we have to compute the word distances of [len] word combinations.
# This sequence of word combinations with indices from 1 to [len] needs to be split
# into chunks (of length [part_len]) so we can serve it to several threads. 
# the output is a matrix holding a row per sub-sequence with first index in column 1
# and last index in column 2.
section_sequence <- function(len, part_len) {
	parts <- ceiling(len / part_len)
    a <- sapply(1:parts, function(k) (k - 1) * part_len + 1)
	b <- sapply(1:parts, function(k) min(k * part_len, len))
	cbind(a, b)
}

# takes a vector of file names, turns the texts into word vectors and then into
# a frequency table (data frame). the frequency table serves two purposes. we will
# use the frequency information to filter out obscure words. and for generating 
# the word combinations we need anyway a vector holding each word no more than once.
extract_full_freq_list <- function(file_names) {
	word_list <- c()
	for(f in file_names) {
		t <- readChar(f, file.info(f)$size)
		word_list <- c(word_list, text_to_word_list(t))
	}
	
	freq_list <- word_list_to_word_freq_list(word_list)
	freq_list
}

# this function turns the text in the specified files (file_name) into an edge list holding
# all possible unordered word combinations (of words occuring at least [min_word_freq]-times
# and are of length at least [min_word_length]) and calculates their string distance. The
# string distance to be used is specified by [dist_type] and [qgram] and has to comply with
# stringdist of eponymous R package. To spead it up we are going to use [threads] threads
# and write the result to a file.
extract_and_store_distances <- function(dist_type, qgram, file_names, file_name_suffix, min_word_freq, min_word_length, threads) {
	
	freq_list <- extract_full_freq_list(file_names)
	fn <- paste("data\\full_freq-list","_",file_name_suffix,".csv",sep="")
	write.table(freq_list ,fn, sep=",",row.names=FALSE)

	freq_list_subset <- freq_list[freq_list$n >= min_word_freq & nchar(freq_list$w) >= min_word_length, "w"]
	
	cl <- makeCluster(threads)
	registerDoSNOW(cl)
	distances <- calculate_all_string_distances(freq_list_subset, dist_type, qgram)
	stopCluster(cl)

	distances <- data.frame("A" = distances[,1], "B" = distances[,2], "d" = as.numeric(distances[,3]), stringsAsFactors = FALSE)
	fn <- paste("data\\full_distances","_",file_name_suffix,".csv",sep="")
	write.table(distances ,fn, sep=",",row.names=FALSE)

	distances
}

#############################################
# performing the set up
#############################################

library(igraph)

# this is going to take a while (on a very fast computer)
d <- extract_and_store_distances("lv", 0, vtexts, "md-dc-ge_min3_lv", 5, 2, 6)

d_sub <- d[d$d == 1 & nchar(d$A) >= 5 & nchar(d$B) >= 5, c("A","B")]
g <- graph.data.frame(d_sub, directed=F)
write.graph(g, "word-dist-graph-occ5-len5-lv1.graphml", "graphml")