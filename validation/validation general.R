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

setwd("~/MT-Data/validation/final")

filelist <- list.files("~/MT-Data/validation/final/results")
val <- read_csv2("val_10.csv")
val <- val %>% 
  mutate(truth = as.factor(truth))

Model <- NULL
Accuracy <- NULL
Kappa <- NULL
Precision <- NULL
Recall <- NULL
F1 <- NULL
Balanced_Accuracy <- NULL
AUC <- NULL

p0 <- 0.5




for (i in 1:length(filelist)) {
    
    modelname <- substr(filelist[i], 1, nchar(filelist[i])-4)
    
    res <- read.csv2(paste("results","/",filelist[i],sep = "" ), dec = ".")
    
    val_tmp <- val %>% 
      merge(res, by = "filename") %>% 
      select(filename, truth, good)
    val_tmp <- val_tmp %>% mutate(good = cut(as.numeric(good), breaks = c(-Inf, p0, Inf), 
                                               labels = c("not", "good")))
    conf <- confusionMatrix(data = val_tmp$good, 
                    reference = val_tmp$truth,
                    positive = "good", mode = "everything")
    
    assign(paste0("conf_",modelname), conf)
    AUC_tmp <- psych::AUC(t = conf$table, plot = "n")
    
    Model <- c(Model, modelname, use.names = FALSE)
    Accuracy <- c(Accuracy, conf$overall[1], use.names = FALSE)
    Kappa <- c(Kappa, conf$overall[2], use.names = FALSE)
    Precision <- c(Precision, conf$byClass[5], use.names = FALSE)
    Recall <- c(Recall, conf$byClass[6], use.names = FALSE)
    F1 <- c(F1, conf$byClass[7], use.names = FALSE)
    Balanced_Accuracy <- c(Balanced_Accuracy, conf$byClass[11], use.names = FALSE)
    AUC[i] <- AUC_tmp$AUC

}

validation_performance <- tibble(Model,
                      Accuracy,
                      Kappa,
                      Precision,
                      Recall,
                      F1,
                      Balanced_Accuracy,
                      AUC)
validation_performance %>% arrange(desc(F1))


