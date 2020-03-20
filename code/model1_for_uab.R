  source("code/utils_for_uab.R")
  
  ##################################################
  #                 Preprocessing
  #             generate pair files 
  ##################################################
  
  df_ffreq <- read.csv("./data/ffreq.csv")
  df_lfreq <- read.csv("./data/lfreq.csv")
  
  df_ffreq_lookup <- df_ffreq %>% 
    mutate(ffreq = c(scale(ffreq_raw))) %>% 
    mutate_if(is.factor, as.character) %>% 
    mutate_if(is.character, str_to_lower)
  
  df_lfreq_lookup <- df_lfreq %>% 
    mutate(lfreq = c(scale(lfreq_raw))) %>% 
    mutate_if(is.factor, as.character) %>% 
    mutate_if(is.character, str_to_lower)
  
  
  df_test_raw <- read.csv("./data/pairs.csv") %>%
    preprocess_data() %>% 
    filter(startsWith(file_id_b, 'a'))
  
  df_test_raw <- df_test_raw[!duplicated(df_test_raw[,-1]),]
  
  df_test_final <- df_test_raw %>% 
    add_feature_vector(df_ffreq_lookup, df_lfreq_lookup) 
  
  df_test_final <- df_test_final[!duplicated(df_test_final$pair_id),]
  
  df_test_final %>% 
    write_csv("./result/df_test_final.csv")
  
  ##################################################
  #                 Trained model
  ##################################################
  
  model <- read_rds("./data/model1.rds")  
  
  ##################################################
  #                 Testing
  ##################################################
  
  ## model1
  t1 <- 0.855
  t2 <- 0.273
  
  df_collection = tibble(model_rf_full = list(model),
                         df_ts = list(df_test_final))
  
  df_review <- df_collection %>% 
    mutate(pred_probs_ts = map2(model_rf_full,
                                df_ts,
                                ~predict(.x, .y, na.action = na.pass, type = "prob")$match))
  
  
  df_match <- 
    df_test_final %>% 
    mutate(prob = df_review$pred_probs_ts[[1]]) %>% 
    filter(prob > t1)
  
  df_match %>% 
    write_csv("./result/match.csv")
  
  df_unmatch <- 
    df_test_final %>% 
    mutate(prob = df_review$pred_probs_ts[[1]]) %>% 
    filter(prob < t2)
  
  df_unmatch %>% 
    write_csv("./result/unmatch.csv")
  
  df_review_sub <- 
    df_test_final %>% 
    mutate(prob = df_review$pred_probs_ts[[1]]) %>% 
    filter(prob > t2 & prob < t1) %>% 
    arrange(abs(0.5 - prob))
  
  
  df_review_sub %>% 
    write_csv("./result/pairs_model1.csv")
  
  
  ##################################################
  #             MINDFIRL format
  #        formatted pairs for manual review
  ##################################################
  
  (df_review_pairs <- 
     read_csv("./result/pairs_model1.csv"))
  
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
  
  df_review_pairs_a %>%
    write_csv("./result/review_a.csv",  na = "")
  
  df_review_pairs_b %>%
    write_csv("./result/review_b.csv",  na = "")
  
  
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
  
  
  pairs %>% 
    write_csv("./result/review_pairs.csv", na="")
    