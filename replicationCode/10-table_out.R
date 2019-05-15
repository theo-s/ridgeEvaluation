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
library(tidyverse)
library(xtable)


X <- read.csv("replicationCode/9-run_all_cluster_resultsEMSE.csv", stringsAsFactors = FALSE)
X$Dataset <- gsub(pattern = "_fold[12345]", replacement = "", x = X$Dataset)

X %>% 
  group_by(Dataset) %>% 
  summarize(forestryRF = mean(forestryRF),
            caretRidgeRF = mean(caretRidgeRF),
            caretRidgeTree = mean(caretRidgeTree),
            ranger = mean(ranger),
            glmnet = mean(glmnet),
            cubist = mean(cubist),
            local_RF = mean(local_RF),
            BART = mean(BART)) %>%
  dplyr::rename(RF_forestry = forestryRF,
                Ridge_RF = caretRidgeRF, 
                Ridge_Tree = caretRidgeTree, 
                RF_ranger = ranger) %>%
  dplyr::select(Dataset, RF_forestry, RF_ranger, glmnet, BART, cubist, 
                Ridge_Tree, local_RF, Ridge_RF, everything()) ->
  X_toprint

bold <- function(x) {paste('{\\textbf{',x,'}}', sep = '')}
print(
  xtable(X_toprint, align = rep('r', ncol(X_toprint) + 1)),
  sanitize.rownames.function = bold,
  sanitize.colnames.function = identity,
  sanitize.text.function = identity,
  latex.environments = "flushleft",
  file = "replicationCode/performance_tables.tex")
