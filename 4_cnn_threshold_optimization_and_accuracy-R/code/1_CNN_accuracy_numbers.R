# requires: model_acc_metric CSV files for each class
# output: aggregated metrics for each beta value
# authors: Julián Grijalba <juliangrijalba@unimayor.edu.co> based on Colin Quinn <cq73@nau.edu>
setwd('C:/Users/Acer/Desktop/TSI/U_VNE_CNN/4_cnn_threshold_optimization_and_accuracy-R/')

library(dplyr)
library(tidyr)
library(ggplot2)
library(dplyr)
library(openxlsx)

data_dir = 'C:/Users/Acer/Desktop/TSI/U_VNE_CNN/4_cnn_threshold_optimization_and_accuracy-R/results/performance_fscore_'

# set the desired F(beta) level without decimals (e.g., 0.75 → 075)
beta = '125/'
csvs = list.files(paste0(data_dir,beta), pattern = "*model_acc_metrics.csv")

csvs_ls = list()
for(i in seq_along(csvs)){
  print(i)
  df = read.csv(paste0(data_dir, beta, csvs[i]))
  df$label = strsplit(csvs[i],"_")[[1]][1] # soundscape classes
  csvs_ls[[i]] = df
}
df = do.call("rbind", csvs_ls)

# manual inspection calls for accuracy metrics
# possible accuracy measures; see 0_CNN_threshold_optimization.R for details
print(unique(df$X))

(temp = df %>% 
    filter(X == "f050")) %>%
  dplyr::select(test, label) 
(temp = df %>% 
    filter(X == "f050") %>%
    summarize(mean(test))) 

(temp = df %>% 
    filter(X == "f075")) %>%
  dplyr::select(test, label) 
(temp = df %>% 
    filter(X == "f075") %>%
    summarize(mean(test))) 

(temp = df %>% 
    filter(X == "F1")) %>%
  dplyr::select(test, label) 
(temp = df %>% 
    filter(X == "F1") %>%
    summarize(mean(test)))

(temp = df %>% 
    filter(X == "Precision")) %>%
  dplyr::select(test, label) 
(temp = df %>% 
    filter(X == "Precision") %>%
    summarize(mean(test)))

(temp = df %>% 
    filter(X == "Recall")) %>%
  dplyr::select(test, label) 
(temp = df %>% 
    filter(X == "Recall") %>%
    summarize(mean(test)))

(temp = df %>% 
    filter(X == "auc")) %>%
  dplyr::select(test, label) 
(temp = df %>% 
    filter(X == "auc") %>%
    summarize(mean(test)))

(temp = df %>% 
    filter(X == "th"))
(temp = df %>% 
    filter(X == "th") %>%
    summarize(mean(test)))

(temp = df %>% 
    filter(X == "Accuracy")) %>%
  dplyr::select(test, label) 
(temp = df %>% 
    filter(X == "Accuracy") %>%
    summarize(mean(test))) 

### SAVE METRICS FOR EACH MODEL ###

# map beta value to the corresponding F-beta name used in the CSV files
beta_map <- c(
  '050' = "f050",
  '075' = "f075",
  '100' = "F1",
  '125' = "f125",
  '150' = "f150"
)

betas <- names(beta_map)

wb <- createWorkbook()

# helper function to safely extract metrics that may be missing
safe_pull <- function(df, metric){
  tmp <- df %>% filter(X == metric) %>% pull(test)
  if(length(tmp)==0){
    return(rep(NA, length(unique(df$label))))
  } else {
    return(tmp)
  }
}

for(beta in betas){
  
  message("Processing beta = ", beta)
  
  folder <- paste0(data_dir, beta, "/")
  csvs <- list.files(folder, pattern = "*model_acc_metrics.csv")
  
  csvs_ls <- list()
  for(i in seq_along(csvs)){
    df <- read.csv(paste0(folder, csvs[i]))
    df$label <- strsplit(csvs[i], "_")[[1]][1]
    csvs_ls[[i]] <- df
  }
  
  df <- do.call("rbind", csvs_ls)
  
  # F-beta metric name to extract
  fbeta_col <- beta_map[[beta]]
  
  # extract class labels
  labels <- df %>% filter(X == "Precision") %>% pull(label)
  
  # build final metrics table
  metrics_df <- data.frame(
    Label     = labels,
    Precision = safe_pull(df, "Precision"),
    Recall    = safe_pull(df, "Recall"),
    Fbeta     = safe_pull(df, fbeta_col),
    AUC       = safe_pull(df, "auc"),
    F1        = safe_pull(df, "F1")
)
  
  # add mean values across classes
  mean_row <- data.frame(
    Label     = "MEAN",
    Precision = mean(metrics_df$Precision, na.rm = TRUE),
    Recall    = mean(metrics_df$Recall,    na.rm = TRUE),
    Fbeta     = mean(metrics_df$Fbeta,     na.rm = TRUE),
    AUC       = mean(metrics_df$AUC,       na.rm = TRUE),
    F1        = mean(metrics_df$F1,        na.rm = TRUE)
  )
  
  metrics_df <- rbind(metrics_df, mean_row)
  
  # write Excel sheet
  sheet_name <- paste0("beta_", beta)
  addWorksheet(wb, sheet_name)
  writeData(wb, sheet_name, metrics_df)
}

# save final Excel file
saveWorkbook(
  wb,
  "C:/Users/Acer/Desktop/TSI/U_VNE_CNN/4_cnn_threshold_optimization_and_accuracy-R/results/summary_metrics_by_beta.xlsx",
  overwrite = TRUE
)

message("Excel successfully generated: summary_metrics_by_beta.xlsx")
``

