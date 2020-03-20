if(!"pacman" %in% installed.packages()){
  install.packages("pacman")
}


pacman::p_load(tidyverse, stringdist, phonics, glue, caret, lubridate,
               rebus, fs, ModelMetrics, MLmetrics, caretEnsemble,
               PGRdup, pROC, scico, RColorBrewer, ggthemes, plotly)


preprocess_data <- function(df){
  df <- df %>% 
    rename(fname = first_name,
           lname = last_name,
           g = sex,
           add = info1,
           phone = info2,
           state = info3,
           zip_code = info4,
           email = info5) %>%
    mutate_if(is.factor, as.character)
  
  df$dob <- as.Date(df$dob, format="%m/%d/%Y")
  
  pairs <- reshape(transform(df, i = 1:2),
                   idvar = "ID",
                   timevar = "i",
                   direction = "wide")
  pairs <- pairs %>%
    rename_all(~{.x %>% str_replace(".1", "_a")}) %>%
    rename_all(~{.x %>% str_replace(".2", "_b")})
  pairs %>% 
    rename(pair_id = ID,
           id_a = voter_reg_num_a,
           id_b = voter_reg_num_b) %>% 
    mutate_if(is.character, str_to_lower) %>% 
    mutate_at(vars(contains("dob")), as_date, format="%m/%d/%Y")
}

add_freq <- function(df, df_ffreq_lookup, df_lfreq_lookup){
  
  df %>% 
    left_join(df_ffreq_lookup,
              by = c("fname_a" = "fname")) %>% 
    left_join(df_ffreq_lookup,
              by = c("fname_a" = "fname"), suffix = c("_a", "_b")) %>% 
    left_join(df_lfreq_lookup,
              by = c("lname_a" = "lname")) %>% 
    left_join(df_lfreq_lookup,
              by = c("lname_a" = "lname"), suffix = c("_a", "_b")) %>% 
    mutate_at(vars(contains("freq")), 
              ~ifelse(is.na(.x), median(.x, na.rm = T), .x))
  
}

summarise_all_string_metrics <- function(x, y, col_name, 
                                         methods = c("dl", "jw", "soundex", 
                                                     "lcs", "qgram", "cosine"),
                                         p = 0.1, q = 2){
  
  dists <- map_dbl(methods, ~stringdist(x, y, method = .x, p = p, q = q))
  tibble(metric = str_c("metric_", col_name, "_", methods), dists = dists) %>% 
    spread(metric, dists)
}

add_feature_vector <- function(df, df_ffreq_lookup, df_lfreq_lookup){
  # browser()
  
  df %>%
    add_freq(df_ffreq_lookup, df_lfreq_lookup) %>% 
    mutate(
      #dob
      birth_year_a = as.double(year(dob_a)),
      birth_year_b = as.double(year(dob_b)),
      birth_day_a = as.double(day(dob_a)),
      birth_day_b = as.double(day(dob_b)),
      birth_month_a = as.double(month(dob_a)),
      birth_month_b = as.double(month(dob_b)),
      year_diff = as.integer(birth_year_a) - as.integer(birth_year_b),
      metric_dob_dist = stringdist(dob_a, dob_b, "dl"),
      metric_year_dist = stringdist(birth_year_a, birth_year_b, "dl"),
      metric_month_dist = stringdist(birth_month_a, birth_month_b, "dl"),
      metric_day_dist = stringdist(birth_day_a, birth_day_b, "dl"),
      metric_dm_swaps = 
        ((!is.na(dob_a)) & (!is.na(dob_b))) &
        (birth_month_a == birth_day_b) & 
        (birth_month_b == birth_day_a),
      metric_age_a = as.double(ymd("2019-01-01") - dob_a)/365,
      metric_age_b = as.double(ymd("2019-01-01") - dob_b)/365,
      
      # fname
      fname_metrics = map2(fname_a, 
                           fname_b, 
                           summarise_all_string_metrics, 
                           col_name = "fname",
                           methods = c("dl", "jw", "soundex", "lcs", "qgram")),
      str_length_fname_a = str_length(fname_a),
      str_length_fname_b = str_length(fname_b),
      metric_fname_len_max = map2_int(str_length_fname_a, str_length_fname_b, ~max(c(.x,.y))),
      metric_fname_len_min = map2_int(str_length_fname_a, str_length_fname_b, ~min(c(.x,.y))),
      metric_ffreq_a = ffreq_a,
      metric_ffreq_b = ffreq_b,
      # metric_ffreq_mean = map2_dbl(ffreq_a, ffreq_b, ~mean(c(.x,.y))),
      # metric_ffreq_diff = abs(ffreq_a - ffreq_b),
      
      # lname
      lname_metrics = map2(lname_a, 
                           lname_b, 
                           summarise_all_string_metrics, 
                           col_name = "lname",
                           methods = c("dl", "jw", "soundex")),
      str_length_lname_a = str_length(lname_a),
      str_length_lname_b = str_length(lname_b),
      metric_lname_len_max = map2_int(str_length_lname_a, str_length_lname_b, ~max(c(.x,.y))),
      metric_lname_len_min = map2_int(str_length_lname_a, str_length_lname_b, ~min(c(.x,.y))),
      metric_lfreq_a = lfreq_a,
      metric_lfreq_b = lfreq_b,
      # metric_lfreq_mean = map2_dbl(lfreq_a, lfreq_b, ~mean(c(.x,.y))),
      
      # name swaps
      metric_name_swaps = 
        ((!is.na(fname_a)) & (!is.na(fname_b)) & 
           (!is.na(lname_a)) & (!is.na(lname_b))) &
        (lname_a == fname_b) & 
        (lname_b == fname_a),
      
      # gender
      gender_code = map2_chr(g_a, g_b, function(x, y){
        str_c(sort(c(x, y)), collapse = "")
      }),
      metric_gender_code_ff = gender_code  %>% str_count("f"),
      metric_gender_code_mm = gender_code  %>% str_count("m"),
      
      # marriage
      metric_potential_marriage =
        (!is.na(metric_age_a) | !is.na(metric_age_b)) &
        ((metric_age_a >= 20) | (metric_age_b >= 20)) &
        (!is.na(lname_a) & !is.na(lname_b)) &
        (lname_a != lname_b) &
        (g_a == "f" | g_b == "f")
      
      ) %>%
    unnest(cols = c(fname_metrics, lname_metrics)) %>%
    select(pair_id, 
           fname_a, fname_b,
           lname_a, lname_b,
           starts_with("dob"), 
           starts_with("gender_code"), 
           starts_with("metric"),
           everything()) %>%
    as.data.frame()
}