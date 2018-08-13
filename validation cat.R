library(tidyverse)
library(caret)
library(broom)
library(plotROC)
library(grid)
library(gridExtra)
library(DescTools)

confusion_matrix <- function(actual, predictions, threshold) {
  classes <- cut(predictions, breaks = c(-Inf, threshold, Inf), labels = levels(actual))
  res <- table(Actual = actual, Prediction = classes)
  res
}

Model <- NULL
Accuracy <- NULL
Kappa <- NULL
Precision <- NULL
Recall <- NULL
F1 <- NULL
Balanced_Accuracy <- NULL
AUC <- NULL

p <- seq(0, 1, 0.05)
P <- NULL
opt_Accuracy <- NULL
opt_Kappa <- NULL
opt_Precision <- NULL
opt_Recall <- NULL
opt_F1 <- NULL
opt_Balanced_Accuracy <- NULL
cat_performance_opt <- NULL

p0 <- 0.5

setwd("~/MT-Data/validation/final")

val <- read_csv2("val_10.csv")
val <- val %>% 
  mutate(truth = as.factor(truth))

fashion <- read.csv2(paste("results","/","fashion_v1_gv_p2.csv",sep = "" ), dec = ".")
val_fashion <- merge(val, fashion, by = "filename") %>% 
  filter(CAT == "Fashion") %>% 
  select(filename, truth, good)
sport <- read.csv2(paste("results","/","sport_v1_p1.csv",sep = "" ), dec = ".")
val_sport <- merge(val, sport, by = "filename") %>% 
  filter(CAT == "Sport") %>% 
  select(filename, truth, good)
nature <- read.csv2(paste("results","/","nature_v1_p1.csv",sep = "" ), dec = ".")
val_nature <- merge(val, nature, by = "filename") %>% 
  filter(CAT == "Nature") %>% 
  select(filename, truth, good)

base <- read.csv2(paste("results","/","baseline_v1_p1.csv",sep = "" ), dec = ".")
val_fashion_base <- merge(val, base, by = "filename") %>% 
  filter(CAT == "Fashion") %>% 
  select(filename, truth, good)
val_sport_base <- merge(val, base, by = "filename") %>% 
  filter(CAT == "Sport") %>% 
  select(filename, truth, good)
val_nature_base <- merge(val, base, by = "filename") %>% 
  filter(CAT == "Nature") %>% 
  select(filename, truth, good)


cat_list <- c("fashion", "fashion_base", "sport", "sport_base", "nature", "nature_base")

for (i in 1:length(cat_list)) {
  
  modelname <- cat_list[i]
  
  val_tmp <- get(paste0("val_", modelname))
  val_tmp <- val_tmp %>% mutate(pred = cut(as.numeric(good), breaks = c(-Inf, p0, Inf), 
                                           labels = c("not", "good")))
  conf <- confusionMatrix(data = val_tmp$pred, 
                          reference = val_tmp$truth,
                          positive = "good", mode = "everything")
  
  assign(paste0("conf_", modelname), conf)
  AUC_tmp <- psych::AUC(t = conf$table, plot = "n")
  
  Model[i] <- modelname
  Accuracy[i] <- conf$overall[1]
  Kappa[i] <- conf$overall[2]
  Precision[i] <- conf$byClass[5]
  Recall[i] <- conf$byClass[6]
  F1[i] <-conf$byClass[7]
  Balanced_Accuracy[i] <- conf$byClass[11]
  AUC[i] <- AUC_tmp$AUC
  
  for (i in 1:length(p)) {
    val_tmp <- val_tmp %>% mutate(pred = cut(as.numeric(good), breaks = c(-Inf, p[i], Inf), 
                                             labels = c("not", "good")))
    conf <- confusionMatrix(data = val_tmp$pred,
                            reference = val_tmp$truth,
                            positive = "good", mode = "everything")
    opt_Accuracy[i] <- conf$overall[1]
    opt_Kappa[i] <- conf$overall[2]
    opt_Precision[i] <- conf$byClass[5]
    opt_Recall[i] <- conf$byClass[6]
    opt_F1[i] <- conf$byClass[7]
    opt_Balanced_Accuracy[i] <- conf$byClass[11]
  }
  
  optimal_tmp <- tibble(p,
                        Accuracy = opt_Accuracy,
                        Kappa = opt_Kappa,
                        Precision = opt_Precision,
                        Recall = opt_Recall,
                        F1 = opt_F1,
                        Balanced_Accuracy = opt_Balanced_Accuracy)
  optimal_tmp <- optimal_tmp %>% arrange(desc(Kappa))
  
  p_tmp <- as.numeric(optimal_tmp[1,1])
  val_tmp <- get(paste0("val_", modelname))
  val_tmp <- val_tmp %>% mutate(pred = cut(as.numeric(good), breaks = c(-Inf, p_tmp, Inf), 
                                           labels = c("not", "good")))
  conf <- confusionMatrix(data = val_tmp$pred, 
                          reference = val_tmp$truth,
                          positive = "good", mode = "everything")
  assign(paste0(modelname), conf)
  
  new_batch <- rbind(
    cbind(Model = modelname,
          Rank = "1st",
          optimal_tmp[1,],
          Diff = as.double(optimal_tmp[1,3] - optimal_tmp %>% filter(p == 0.5) %>% select(Kappa))),
    cbind(Model = modelname,
          Rank = "2nd",
          optimal_tmp[2,],
          Diff = as.double(optimal_tmp[2,3] - optimal_tmp %>% filter(p == 0.5) %>% select(Kappa))),
    cbind(Model = modelname,
          Rank = "3rd",
          optimal_tmp[3,],
          Diff = as.double(optimal_tmp[3,3] - optimal_tmp %>% filter(p == 0.5) %>% select(Kappa)))
  )
  
  cat_performance_opt <- rbind(cat_performance_opt, new_batch)
  
}

cat_performance <- tibble(Model,
                                 Accuracy,
                                 Kappa,
                                 Precision,
                                 Recall,
                                 F1,
                                 Balanced_Accuracy,
                                 AUC)
cat_performance
cat_performance_opt %>% 
  filter(Rank == "1st")

