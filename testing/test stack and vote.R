library(tidyverse)
library(caret)
library(broom)
library(plotROC)
library(grid)
library(gridExtra)
library(DescTools)
p <- seq(0, 1, 0.05)

optimal_stack <- function(x, p, m) {
  
  val_tmp <- merge(x$pred, x$bestTune) %>% 
    select(obs, good)
  
  P <- NULL
  Accuracy <- NULL
  Kappa <- NULL
  Precision <- NULL
  Recall <- NULL
  F1 <- NULL
  Balanced_Accuracy <- NULL
  optimalP <- NULL
  
  for (i in 1:length(p)) {
    val_tmp <- val_tmp %>% mutate(pred = cut(as.numeric(good), breaks = c(-Inf, p[i], Inf), 
                                             labels = c("not", "good")))
    conf <- confusionMatrix(data = val_tmp$pred,
                            reference = val_tmp$obs,
                            positive = "good", mode = "everything")
    Accuracy[i] <- conf$overall[1]
    Kappa[i] <- conf$overall[2]
    Precision[i] <- conf$byClass[5]
    Recall[i] <- conf$byClass[6]
    F1[i] <- conf$byClass[7]
    Balanced_Accuracy[i] <- conf$byClass[11]
  }
  optimal_tmp <- tibble(p,
                        Accuracy,
                        Kappa,
                        Precision,
                        Recall,
                        F1,
                        Balanced_Accuracy)
  optimal_tmp <- optimal_tmp %>% arrange(desc(get(m)))
  
  out <- rbind(
    cbind(Rank = "1st",
          optimal_tmp[1,],
          Diff = as.double(optimal_tmp[1,m] - optimal_tmp %>% filter(p == 0.5) %>% select(m))),
    cbind(Rank = "2nd",
          optimal_tmp[2,],
          Diff = as.double(optimal_tmp[2,m] - optimal_tmp %>% filter(p == 0.5) %>% select(m))),
    cbind(Rank = "3rd",
          optimal_tmp[3,],
          Diff = as.double(optimal_tmp[3,m] - optimal_tmp %>% filter(p == 0.5) %>% select(m))))
  out
}

f1 <- function (data, lev = NULL, model = NULL) {
  tbl <- table(data[, "pred"], data[, "obs"]
  )
  
  out <- c(precision(tbl, relevant = "good"),
           recall(tbl, relevant = "good"),
           F_meas(tbl, relevant = "good"))
  names(out) <- c("Precision", "Recall","F1")
  out
}

round_df <- function(x, digits) {
  # round all numeric variables
  # x: data frame 
  # digits: number of digits to round
  numeric_columns <- sapply(x, mode) == 'numeric'
  x[numeric_columns] <-  round(x[numeric_columns], digits)
  x
}

sixStats <- function(...) c(twoClassSummary(...),defaultSummary(...),f1(...))

setwd("~/MT-Data/testing/final")

filelist <- list.files("~/MT-Data/testing/final/results")
test <-  read_csv2("test_10.csv") %>% 
  mutate(truth = as.factor(truth)) %>% 
  select(filename, truth)

for (i in 1:length(filelist)) {
  
  modelname <- substr(filelist[i], 1, nchar(filelist[i])-8)
  assign(
    paste(modelname),
    read.csv2(dec = ".", paste("results","/", filelist[i], sep = "" )) %>% 
      select(filename, good)
  ) 
}

split_15_85 <- split_15_85 %>% mutate(split_15_85 = good) %>% select(filename, split_15_85)
split_25_75 <- split_25_75 %>% mutate(split_25_75 = good) %>% select(filename, split_25_75)
gap_baseline <- gap_baseline %>% mutate(gap_baseline = good) %>% select(filename, gap_baseline)
gap_15_85 <- gap_15_85 %>% mutate(gap_15_85 = good) %>% select(filename, gap_15_85)
gap_30_70 <- gap_30_70 %>% mutate(gap_30_70 = good) %>% select(filename, gap_30_70)

all_test <- reduce(list(split_15_85, split_25_75, gap_baseline, gap_15_85, gap_30_70,
                        test),
                    merge)

setwd("~/MT-Data/validation/final")

filelist <- list.files("~/MT-Data/validation/final/results")
val_stack <- read_csv2("val_10.csv") %>% 
  mutate(truth = as.factor(truth)) %>% 
  select(filename, truth)

for (i in 1:length(filelist)) {
  
  modelname <- substr(filelist[i], 1, nchar(filelist[i])-4)
  assign(
    paste(modelname),
    read.csv2(dec = ".", paste("results","/", modelname, ".csv",sep = "" )) %>% 
      select(filename, good)
  ) 
}

split_15_85 <- general_15 %>% mutate(split_15_85 = good) %>% select(filename, split_15_85)
gap_baseline <- general_gap_high_v1_p1 %>% mutate(gap_baseline = good) %>% select(filename, gap_baseline)
gap_15_85 <- m10_10_80_v1_p1 %>% mutate(gap_15_85 = good) %>% select(filename, gap_15_85)
gap_30_70 <- general_gap_low_v1_p1 %>% mutate(gap_30_70 = good) %>% select(filename, gap_30_70)



all_stack <- reduce(list(split_15_85, gap_30_70, gap_15_85,
                         val_stack),
                    merge)

setwd("~/MT-Data/testing/final")

tc <- trainControl(
  method = "repeatedcv", 
  classProbs = TRUE,
  savePredictions = "final",
  number=10, 
  repeats = 5, 
  sampling = "smote",
  summaryFunction = sixStats)


f_stack_final <- formula("truth ~ split_15_85 + gap_30_70 + gap_15_85")

set.seed(1)
stack_final <- train(form=f_stack_final,
                           data=all_stack,
                           method="glm",
                           trControl=tc,
                           metric="F1",
                           family=binomial(link="logit"))

p_opt_stack <- optimal_stack(stack_final, p, "F1")[1,2]*100

cbind(filename = all_test$filename, 
      predict(stack_final, newdata = all_test, type = "prob")) %>% 
  write.table(paste0("results/Stacking_p", p_opt_stack, ".csv"), 
              dec = ".", quote = FALSE, sep = ";", row.names = FALSE)

#-----------------------------

soft_vote <- tibble(filename = as.character(all_test$filename), good = rowMeans(all_test[2:6]))
soft_vote %>% 
  write.table(file = "results/soft_vote_p60.csv", 
              sep = ";",  
              row.names = FALSE,
              quote = FALSE)
