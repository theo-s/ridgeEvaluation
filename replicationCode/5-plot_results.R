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
full_data <- full_data %>% tbl_df()


for (ds_name in as.character(unique(full_data$data_name))) {
  # ds_name = as.character(unique(full_data$data_name))[1]
  print(ds_name)
  p_l <- full_data %>% 
    filter(data_name == ds_name) %>%
    group_by(estimator_name, sampsize) %>%
    summarize(MSE = mean((y_estimate - y_true) ^ 2)) %>%
    ggplot(aes(x = sampsize, y = MSE, color = estimator_name)) +
    geom_line() +
    scale_y_log10() +
    geom_text(aes(label = estimator_name))
  
  ggsave(p_l, file = paste0("figures/", ds_name, ".pdf"), width = 10, height = 8)
  
}
