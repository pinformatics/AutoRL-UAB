source("code/utils_for_uab.R")

pairs <- read.csv("./result/pairs.csv")

library(igraph)

d <- data.frame()
uniq_pair <- unique(pairs$PairID)

for (i in 1:length(uniq_pair)){
  ids <- pairs[which(pairs$PairID == uniq_pair[i]), "rec_id"]
  d[i,1] <- ids[1]
  d[i,2] <- ids[2]
}

g <- graph.data.frame(d, directed = TRUE, vertices = NULL)
group <- clusters(g)

for (j in 1:nrow(pairs)){
  id <- pairs$rec_id[j]
  pairs[which(pairs$rec_id == id), "GroupID"] <- unname(group$membership[toString(id)])
}


pairs %>% 
    write_csv("./result/uab_final_pairs.csv", na="")
