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
library(reshape)
X <- read.csv("replicationCode/9-run_all_cluster_resultsEMSE.csv", stringsAsFactors = FALSE)
X <- X[22:36,-1]
X$n <- as.numeric(gsub(pattern = "([^0123456789])", replacement = "", X$Dataset))
X$Dataset <- gsub(pattern = "([-0123456789])", replacement = "", X$Dataset)
X <- melt(data = X, id.vars = c("Dataset", "n"))
X$dsname <- NA
X$dsname[X$Dataset == "artificialLM"] <- "Experiment 1"
X$dsname[X$Dataset == "simulatedStepFunction"] <- "Experiment 2"
X$dsname[X$Dataset == "simulatedStepLinearFunction"] <- "Experiment 3"
X %>% filter(!is.na(value)) %>%
  filter(variable %in% c("ranger", "glmnet")) %>%
  ggplot(aes(x = n, y = value, color = variable))  +
  geom_line() +
  facet_wrap(.~Dataset, scales = "free_y") +
  geom_text(aes(label = variable)) +
  theme_bw() + 
  # ylim(0, 5) +
  theme(legend.position = "none")

for (expnm in unique(X$dsname)) {
  X %>% filter(!is.na(value) & dsname == expnm & variable != "cubist" & variable != "caretRidgeTree") %>%
    ggplot(aes(x = n, y = value, color = variable))  +
    geom_line() +
    geom_text(aes(label = variable)) +
    theme_bw() + 
    # ylim(0, 10) +
    theme(legend.position = "none") +
    # ggtitle(label = expnm) +
    ggtitle(label = "") +
    xlab("Sample Size") +
    ylab("EMSE") + 
    geom_point()
  
  ggsave(file = paste0("figures/varyn_", expnm, ".pdf"), height = 3.3, width = 6)
  ggsave(file = paste0("~/Dropbox/RidgeForestry_paper/figures/2-VaryNSim/varyn_", 
                       expnm, ".pdf"), height = 3.3, width = 6)
  
}




