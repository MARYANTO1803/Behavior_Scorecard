# Masukan nama-nama Package untuk suatu Project
list.of.packages <- c(
  "dplyr",
  "inspectdf",
  "caret",
  "scorecard",
  "rsample",
  "GGally",
  "readxl",
  "knitr",
  "scales",
  "gtools",
  "car",
  "rsconnect")

# Periksa Package yang Belum Terinstal:
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]

# Instal Package yang Belum Terinstal:
if(length(new.packages)) install.packages(new.packages, repos = "http://cran.us.r-project.org")

# Memuat Semua Package yang Diperlukan:
library(dplyr) # Wrangling data
library(inspectdf) # add on wrangling data
library(caret) # classification & regression training
library(scorecard) # credit risk scorecard
library(rsample) # resampling data
library(gtools) # programming tools
library(readxl)
library(GGally) # plot heatmap correlation 
library(knitr) # reporting in R
library(scales) # manipulasi skala data untuk visualisasi
library(car) # check multicollinearity


# DO NOT CHANGE
predict_behaviour <- function(data, score_card, cutoff = 538){
  new_score <- scorecard_ply(data, score_card)
  new_score <- new_score %>%
    mutate(
      recommendation = case_when(
        score > cutoff ~ "GOOD",
        TRUE ~ "BAD"
      )
    )
  new_score
}

approval_rate <- function(score, label, positive = 0){
  # membuat list
  score_list <- list(data = score)
  label_list <- list(data = label)
  # membuat gains_table
  g <- gains_table(score = score_list, label = label_list, positive = positive)
  
  final_df <- g %>% 
    mutate(
      count_approved = max(cum_count) - cum_count,
      neg_approved = max(cum_neg) - cum_neg,
      neg_rate = round((neg_approved / count_approved), 4)
    ) %>% 
    replace(is.na(.), 0) %>% 
    select(bin, approval_rate, neg_rate, 
           count_approved, neg_approved,
           count, neg, pos)
  
  final_df
}

