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

word_list_to_word_freq_list <- function(wl) {
	wl <- data.frame(w = wl)
	fl <- sqldf("select w, count(*) as n from wl group by w order by n desc");
	fl
}

calculate_all_string_distances <- function(wl) {
	g <- expand.grid(1:length(wl),1:length(wl))
	g <- as.matrix(g[g[,1] < g[,2],])
	
	
}

section_sequence <- function(len, parts) {
	part_len <- ceiling(len / parts)
    a <- sapply(1:p, function(k) (k - 1) * c + 1)
	b <- sapply(1:p, function(k) min(k * c, l))
	cbind(a, b)
}

file <- ""
t <- readChar(file, file.info(file)$size)
word_list <- text_to_word_list(t)
freq_list <- word_list_to_word_freq_list(word_list)
