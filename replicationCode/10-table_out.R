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

# Output information about the data sets ---------------------------------------
source("replicationCode/1.8-generateDataBrieman.R")
source("replicationCode/1.9-DS_autos_bike_soe.R")

data_set_info <- data.frame()
for (i in 1:length(datasets_grid)) {
  # i = 2
  name <- names(datasets_grid)[i]
  ds <- datasets_grid[[i]]
  dim <- ncol(ds$train)
  ntrain <- nrow(ds$train)
  ntest <- nrow(ds$test)
  
  num_numeric <- 0
  for (j in 1:dim) {
    if (is.numeric(ds$test[,j])) num_numeric <- num_numeric + 1
  }
  
  to_add <- data.frame(name, ntrain, ntest, dim, num_numeric)
  data_set_info <- rbind(data_set_info, to_add)
  
}

data_set_info <- 
  data_set_info[!substr(data_set_info$name, 1,5) %in% c("Bosto", "Ozone", "Servo"), ]

data_set_info <-
  rbind(data_set_info,
        data.frame(
          name = "Ozone",
          ntrain = 330,
          ntest = NA,
          dim = 9,
          num_numeric = 9
        ))
data_set_info <-
  rbind(data_set_info,
        data.frame(
          name = "Servo",
          ntrain = 134+33,
          ntest = NA,
          dim = 13,
          num_numeric = 13
        ))
data_set_info <-
  rbind(data_set_info,
        data.frame(
          name = "Boston",
          ntrain = 406+100,
          ntest = NA,
          dim = 9,
          num_numeric = 9
        ))
data_set_info$name <- as.character(data_set_info$name)
data_set_info <- data_set_info %>% dplyr::arrange(name)

colnames(data_set_info)[5] <- "n numeric feat"
data_set_info$name <- gsub("_", " ", data_set_info$name)

data_set_info$name[2:3] <- c("Autos", "Bike")

for (i in 1:ncol(data_set_info)) {
  data_set_info[,i] <- as.character(data_set_info[,i])
}

print(
  xtable(data_set_info, align = rep('r', ncol(data_set_info) + 1), 
         caption = "The table summarizes the data sets.", label = "tbl:dssummary"),
  include.rownames = FALSE,
  # include.colnames = FALSE, 
  sanitize.colnames.function = identity,
  sanitize.text.function = identity,
  latex.environments = "flushleft",
  file = "~/Dropbox/RidgeForestry_paper/tables/datasetTables.tex"
)

# Output information about the performance -------------------------------------

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

# Save the table ------------------------------------------------
bold <- function(x) {paste('{\\textbf{',x,'}}', sep = '')}

# print(
#   xtable(X_toprint, align = rep('r', ncol(X_toprint) + 1)),
#   sanitize.rownames.function = bold,
#   sanitize.colnames.function = identity,
#   sanitize.text.function = identity,
#   latex.environments = "flushleft",
#   file = "replicationCode/performanceTables.tex"
# )
X_toprint[,-1] <- sqrt(X_toprint[,-1])
X_toprint <- as.data.frame(X_toprint)

minimizer_pos <- apply(X_toprint[,-1], 1, which.min) + 1

X_toprint_char <- X_toprint
for (i in 2:ncol(X_toprint_char)) {
  X_toprint_char[ , i] <- as.character(round(X_toprint_char[ , i], 2))
}
X_toprint_char$Dataset <- gsub("_", " ", X_toprint_char$Dataset)
colnames(X_toprint_char) <- gsub("_", " ", colnames(X_toprint_char))
X_toprint_char$Dataset[4] <- "Boston"

X_toprint_char
X_toprint_char <- rbind(c("", "forestry", "ranger", "glmnet", "dbarts", "Cubist", "forestry", "grf", "forestry"), 
      X_toprint_char)
colnames(X_toprint_char) <- c("", "RF", "RF", "RLM", "BART", "Cubist", "RCART", "local RF", "Ridge RF")

for (i in 2:nrow(X_toprint_char)) {
  # i <- 2
  X_toprint_char[i, minimizer_pos[i - 1]] <-
    paste0("\\textbf{", X_toprint_char[i, minimizer_pos[i - 1]], "}")
}
dir.create("~/Dropbox/RidgeForestry_paper/tables/", showWarnings = FALSE)
print(
  xtable(X_toprint_char, align = rep('r', ncol(X_toprint_char) + 1), 
         caption = "Performance of the estimators.", label = "tbl:performance"),
  include.rownames = FALSE,
  # include.colnames = FALSE, 
  sanitize.colnames.function = identity,
  sanitize.text.function = identity,
  latex.environments = "flushleft",
  file = "~/Dropbox/RidgeForestry_paper/tables/performanceTables.tex"
)
