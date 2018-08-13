library(tidyverse)
library(caret)
library(broom)
library(plotROC)
library(grid)
library(gridExtra)
library(DescTools)
library(doParallel)

f1 <- function (data, lev = NULL, model = NULL) 
{
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

baseline_v1_p1 <- baseline_v1_p1 %>% mutate(baseline_v1_p1 = good) %>% select(filename, baseline_v1_p1)
general_10 <- general_10 %>% mutate(general_10 = good) %>% select(filename, general_10)
general_15 <- general_15 %>% mutate(general_15 = good) %>% select(filename, general_15)
general_25 <- general_25 %>% mutate(general_25 = good) %>% select(filename, general_25)
general_gap_high_v1_p1 <- general_gap_high_v1_p1 %>% mutate(general_gap_high_v1_p1 = good) %>% select(filename, general_gap_high_v1_p1)
general_gap_low_v1_p1 <- general_gap_low_v1_p1 %>% mutate(general_gap_low_v1_p1 = good) %>% select(filename, general_gap_low_v1_p1)
general_split_high_v1_p1 <- general_split_high_v1_p1 %>% mutate(general_split_high_v1_p1 = good) %>% select(filename, general_split_high_v1_p1)
general_split_low_v1_p1 <- general_split_low_v1_p1 %>% mutate(general_split_low_v1_p1 = good) %>% select(filename, general_split_low_v1_p1)
fashion_v1_gv_p2 <- fashion_v1_gv_p2 %>% mutate(fashion_v1_gv_p2 = good) %>% select(filename, fashion_v1_gv_p2)
nature_v1_p1 <- nature_v1_p1 %>% mutate(nature_v1_p1 = good) %>% select(filename, nature_v1_p1)
sport_v1_p1 <- sport_v1_p1 %>% mutate(sport_v1_p1 = good) %>% select(filename, sport_v1_p1)
m10_10_80_v1_p1 <- m10_10_80_v1_p1 %>% mutate(m10_10_80_v1_p1 = good) %>% select(filename, m10_10_80_v1_p1)
m35_10_55_v1_p1 <- m35_10_55_v1_p1 %>% mutate(m35_10_55_v1_p1 = good) %>% select(filename, m35_10_55_v1_p1)
m20_20_60_v1_p1 <- m20_20_60_v1_p1 %>% mutate(m20_20_60_v1_p1 = good) %>% select(filename, m20_20_60_v1_p1)
m15_20_65_v1_p1 <- m15_20_65_v1_p1 %>% mutate(m15_20_65_v1_p1 = good) %>% select(filename, m15_20_65_v1_p1)
gap_25_75_v1_p1 <- gap_25_75_v1_p1 %>% mutate(gap_25_75_v1_p1 = good) %>% select(filename, gap_25_75_v1_p1)



all_stack <- reduce(list(baseline_v1_p1,
                         general_10,
                         general_15,
                         general_25,
                         general_gap_high_v1_p1,
                         general_gap_low_v1_p1,
                         general_split_high_v1_p1,
                         general_split_low_v1_p1,
                         fashion_v1_gv_p2,
                         nature_v1_p1,
                         sport_v1_p1,
                         m10_10_80_v1_p1,
                         m35_10_55_v1_p1,
                         m20_20_60_v1_p1,
                         m15_20_65_v1_p1,
                         val_stack,
                         gap_25_75_v1_p1),
                   merge)

tc <- trainControl(
                   method = "repeatedcv", 
                   classProbs = TRUE,
                   savePredictions = "final",
                   number=10, 
                   repeats = 5, 
                   sampling = "smote",
                   summaryFunction = sixStats)

f_stack_high_opt <- formula("truth ~ general_15 + general_gap_low_v1_p1 + m10_10_80_v1_p1")
set.seed(1)
high_stack_opt <- train(form=f_stack_high_opt,
                    data=all_stack,
                    method="glm",
                    trControl=tc,
                    metric="F1",
                    family=binomial(link="logit"))

high_stack_opt$results %>% 
  select(ROC:F1)
high_stack_opt$finalModel %>% tidy() %>% round_df(3)

optimal_stack(high_stack_opt, p, "F1") 

stack_val <- high_stack_opt$pred[c("obs", "good")] %>% mutate(good = cut(as.numeric(good), breaks = c(-Inf, 0.55, Inf), 
                                                            labels = c("not", "good")))
conf_stack_val <- confusionMatrix(data = stack_val$good, 
                        reference = stack_val$obs,
                        positive = "good", mode = "everything")
conf_stack_val
