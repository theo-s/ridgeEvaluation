library(forestry)
library(ranger)
library(glmnet)
library(grf)
library(ggplot2)
library(tidyverse)
library(reshape)

if (dir.exists("~/Dropbox/ridgeEvaluation/")) {
  setwd("~/Dropbox/ridgeEvaluation/")
} else if (dir.exists("~/ridgeEvaluationCode/")) {
  setwd("~/ridgeEvaluationCode/")
} else if (dir.exists("~/ridgeEvaluation/")) {
  setwd("~/ridgeEvaluation/")
} else {
  stop("wd was not set correctly")
}

# Read in Files ----------------------------------------------------------------
dta_folder <- "replicationCode/estimates/"
file_grid <- dir(dta_folder)

full_data <- data.frame()
for (file in file_grid) {
  # file = file_grid[1]
  print(file)
  full_data <- rbind(full_data, 
                     read.csv(paste0(dta_folder, file)))
  
}
full_data <- full_data %>% tbl_df() %>%
  mutate(estimator_name = as.character(estimator_name),
         estimator_name = ifelse(estimator_name == "cubist", "tuned_cubist", estimator_name), 
         estimator_name = ifelse(estimator_name == "forestry", "untuned_forestry", estimator_name), 
         estimator_name = ifelse(estimator_name == "glmnet", "tuned_glmnet", estimator_name), 
         estimator_name = ifelse(estimator_name == "glmnet_1", "tuned_glmnet_1", estimator_name), 
         estimator_name = ifelse(estimator_name == "glmnet_2", "tuned_glmnet_2", estimator_name), 
         estimator_name = ifelse(estimator_name == "glmnet_3", "tuned_glmnet_3", estimator_name), 
         estimator_name = ifelse(estimator_name == "randomcaretRidgeRF", "tuned_RidgeRF", estimator_name), 
         estimator_name = ifelse(estimator_name == "ranger", "tuned_ranger", estimator_name), 
         estimator_name = ifelse(estimator_name == "ranger_1", "tuned_ranger_1", estimator_name), 
         estimator_name = ifelse(estimator_name == "ranger_2", "tuned_ranger_2", estimator_name), 
         estimator_name = ifelse(estimator_name == "ranger_3", "tuned_ranger_3", estimator_name), 
         estimator_name = ifelse(estimator_name == "ridge_1", "tuned_ridge_1", estimator_name), 
         estimator_name = ifelse(estimator_name == "ridge_2", "tuned_ridge_2", estimator_name), 
         estimator_name = ifelse(estimator_name == "ridge_3", "tuned_ridge_3", estimator_name), 
         estimator_name = ifelse(estimator_name == "ridge_4", "tuned_ridge_4", estimator_name), 
         estimator_name = ifelse(estimator_name == "ridge_5", "tuned_ridge_5", estimator_name), 
         estimator_name = ifelse(estimator_name == "local_RF", "tuned_local_RF", estimator_name)) 

for (ds_name in as.character(unique(full_data$data_name))) {
  # ds_name = as.character(unique(full_data$data_name))[1]
  print(ds_name)
  full_data %>% 
    filter(data_name == ds_name) %>%
    group_by(estimator_name, sampsize) %>%
    summarize(MSE = mean((y_estimate - y_true) ^ 2)) %>%
    ggplot(aes(x = sampsize, y = MSE, color = estimator_name)) +
    geom_line() +
    # scale_y_log10() +
    theme_bw() +
    geom_text(aes(label = estimator_name)) + 
    ggtitle(ds_name)
  
  ggsave(file = paste0("figures/", ds_name, ".pdf"), width = 10, height = 8)
  
}


for (ds_name in as.character(unique(full_data$data_name))) {
  # ds_name = as.character(unique(full_data$data_name))[1]
  print(ds_name)
  full_data %>% 
    filter(data_name == ds_name) %>%
    group_by(estimator_name, sampsize) %>%
    summarize(MSE = mean((y_estimate - y_true) ^ 2)) %>%
    saveRDS(file = paste0(ds_name,".rds"))
  #ggplot(aes(x = sampsize, y = MSE, color = estimator_name)) +
  #geom_line() +
  ## scale_y_log10() +
  #theme_bw() +
  #geom_text(aes(label = estimator_name)) + 
  #ggtitle(ds_name)
  
  #ggsave(file = paste0("figures/", ds_name, ".pdf"), width = 10, height = 8)
  
}
