if (dir.exists("~/Dropbox/ridgeEvaluation/")) {
  setwd("~/Dropbox/ridgeEvaluation/")
} else if (dir.exists("~/ridgeEvaluationCode/")) {
  setwd("~/ridgeEvaluationCode/")
} else if (dir.exists("~/ridgeEvaluation/")) {
  setwd("~/ridgeEvaluation/")
} else if (dir.exists("/accounts/projects/sekhon/theo_s/gdrive/ridgeEvaluation")) {
  setwd("/accounts/projects/sekhon/theo_s/gdrive/ridgeEvaluation")
} else {
  stop("wd was not set correctly")
}

# install most up to date version of forestry
devtools::install_github("soerenkuenzel/forestry", ref = "master")

library(forestry)
library(ranger)
library(glmnet)
library(grf)
library(tidyverse)
library(reshape)
library(Cubist)
library(caret)
library(clustermq)
# set up cluster to run on the high partition
options(clustermq.scheduler = "slurm", 
        clustermq.template = "~/clustermq_low.tmpl") 
#~/clustermq_low.tmpl ~/clustermq_high.tmpl

dir.create("replicationCode/9-results/", showWarnings = FALSE)

data_folder_name <- "replicationCode/estimates/"
dir.create(data_folder_name, showWarnings = FALSE)

#set.seed(634801) Previous seed with bad LM Medium Trend
set.seed(5387479) 
#source("replicationCode/1.5-generateDataBigtest.R")
source("replicationCode/2-generateEstimators.R")
source("replicationCode/1.8-generateDataBrieman.R")
source("replicationCode/1.9-DS_autos_bike_soe.R")
# generate all the different jobs and save it ----------------------------------
(ds_names <- names(datasets_grid))
(etm_names <- names(estimator_grid))

(all_jobs <- expand.grid(ds_names, etm_names) %>% 
  dplyr::rename(Dataset = Var1, Estimator = Var2)) %>% 
  dplyr::arrange(Dataset)

all_jobs$EMSE <- NA
all_jobs$runtime <- NA


# update EMSE table ------------------------------------------------------------
update_EMSE_table <- function(){
  # Reads in all the predicitons in results and computes the EMSE and saves it 
  # in 9-run_all_cluster_results.csv
  
  # updates all_jobs
  for (file in dir("replicationCode/9-results/")) {
    results <- read.csv(paste0("replicationCode/9-results/", file))
    this_row <- 
      as.character(all_jobs$Dataset) == as.character(results$Dataset) &  
      as.character(all_jobs$Estimator) == as.character(results$Estimator)
    all_jobs[this_row, ] <- results
  }
  
  # save
  EMSE_table <- reshape2::dcast(data = all_jobs,
                                formula = Dataset ~ Estimator,
                                value.var = "EMSE") 
  write.csv(x = EMSE_table, 
            file = "replicationCode/9-run_all_cluster_results.csv")
}

# run the jobs -----------------------------------------------------------------
batch_func <- function(i){
  # i <- 1
  set.seed(6264175)
  
  this_job <- all_jobs[i, ]
  
  # run the current job this will save the results in 9-results/
  
  
  this_job$EMSE <- i
  this_job$runtime <- i
  
  # save the job
  write.csv(x = this_job, 
            file = paste0("replicationCode/9-results/job", i, ".csv"), 
            row.names = FALSE) 
  
  # Update the EMSE table 
  update_EMSE_table()
}

Q(fun = batch_func,
  n_jobs = 2,
  i = 1:5, #nrow(all_jobs),
  export = list(
    trainSet = datasets_grid, 
    estimator_grid = estimator_grid, 
    all_jobs = all_jobs, 
    update_EMSE_table = update_EMSE_table
  ))

read.csv("replicationCode/9-run_all_cluster_results.csv")

