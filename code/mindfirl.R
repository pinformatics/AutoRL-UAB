source("code/utils_for_uab.R")
##################################################
#             MINDFIRL format
#        formatted pairs for manual review
##################################################

(df_review_pairs <- 
     read_csv("./result/match.csv"))

df_review_pairs_compat <- 
  df_review_pairs %>% 
  select(pair_id,
         starts_with("id"),
         starts_with("fname"),
         starts_with("lname"),
         starts_with("dob"),
         starts_with("g_"), 
         starts_with("race"), 
         starts_with("add"),  
         starts_with("phone"), 
         starts_with("state"),
         starts_with("zip_code"),
         starts_with("email") 
  ) %>% 
  mutate_all(str_to_upper)


df_review_pairs_a <- 
  df_review_pairs_compat %>% 
  select(pair_id, ends_with("_a"))

names(df_review_pairs_a) <- c("PairID",	"rec_id", "first_name",	"last_name",	"dob", "sex", "race",
                              "email", "zip_code", "state", "Rheumatologist_NPI", "Rheumatologist_name")


df_review_pairs_b <- 
  df_review_pairs_compat %>% 
  select(pair_id, ends_with("_b"))

names(df_review_pairs_b) <- c("PairID",	"rec_id", "first_name",	"last_name",	"dob", "sex", "race",
                              "email", "zip_code", "state", "Rheumatologist_NPI", "Rheumatologist_name")

pairs <- df_review_pairs_a %>% 
  bind_rows(df_review_pairs_b) %>% 
  arrange(PairID) %>% 
  mutate(GroupID = 0,
         DB = "A", 
         ID = row_number())

pairs <- pairs[c(1, 13:15, 2:12)]

## add group id
library(igraph)

d <- data.frame()
uniq_pair <- unique(pairs$PairID)

for (i in 1:length(uniq_pair)){
  ids <- pairs[which(pairs$PairID == uniq_pair[i]), "rec_id"]
  d[i,1] <- ids[1, 1]
  d[i,2] <- ids[2, 1]
}

g <- graph.data.frame(d, directed = TRUE, vertices = NULL)
group <- clusters(g)

for (j in 1:nrow(pairs)){
  id <- pairs$rec_id[j]
  pairs[which(pairs$rec_id == id), "GroupID"] <- unname(group$membership[toString(id)])
}

pairs$dob <- as.Date(pairs$dob, format="%m/%d/%Y")
pairs$dob <- format(as.POSIXct(pairs$dob), "%m/%d/%Y")

pairs %>% 
      write_csv("./result/match_for_MINDFIRL.csv", na="")
