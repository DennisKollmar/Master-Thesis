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
## Do after validation final and stacking 

setwd("~/MT-Data/validation/final")

p <- seq(0, 1, 0.05)
P <- NULL
Accuracy <- NULL
Kappa <- NULL
Precision <- NULL
Recall <- NULL
F1 <- NULL
Balanced_Accuracy <- NULL
BasicP <- NULL

#----------------------------------------
opt_metric <- "F1"
#----------------------------------------

val <- read_csv2("val_10.csv")
val <- val %>% 
  mutate(truth = as.factor(truth))

filelist <- list.files("~/MT-Data/validation/final/results")

for (i in 1:length(filelist)) {
  
  modelname <- substr(filelist[i], 1, nchar(filelist[i])-4)
  
  res <- read.csv2(paste("results","/",filelist[i],sep = "" ), dec = ".")

  val_tmp <- val %>% 
    merge(res, by = "filename") %>% 
    select(filename, truth, good)
  for (i in 1:length(p)) {
    val_tmp <- val_tmp %>% mutate(pred = cut(as.numeric(good), breaks = c(-Inf, p[i], Inf), 
                                           labels = c("not", "good")))
    conf <- confusionMatrix(data = val_tmp$pred,
                            reference = val_tmp$truth,
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
  optimal_tmp <- optimal_tmp %>% arrange(desc(get(opt_metric)))
  
  new_batch <- rbind(
    cbind(Model = modelname,
          Rank = "1st",
          optimal_tmp[1,],
          Diff = as.double(optimal_tmp[1,opt_metric] - optimal_tmp %>% filter(p == 0.5) %>% select(opt_metric))),
    cbind(Model = modelname,
          Rank = "2nd",
          optimal_tmp[2,],
          Diff = as.double(optimal_tmp[2,opt_metric] - optimal_tmp %>% filter(p == 0.5) %>% select(opt_metric))),
    cbind(Model = modelname,
          Rank = "3rd",
          optimal_tmp[3,],
          Diff = as.double(optimal_tmp[3,opt_metric] - optimal_tmp %>% filter(p == 0.5) %>% select(opt_metric)))
  )
  
  BasicP <- rbind(BasicP, new_batch)
  
}
BasicP %>% 
  filter(Rank == "1st") %>% arrange(desc(F1))

#------------------------------------------- 

#p_general_stack <- thresholder(general_stack_old, p, final = TRUE)
#p_general_stack %>% 
#  arrange(Dist)

stacklist <- c("high_stack_opt")
StackP <- NULL

for (i in 1:length(stacklist)) {
  
  new_batch <- cbind(Stack = stacklist[i], optimal_stack(get(stacklist[i]), p, "F1"))
  
  StackP <- rbind(StackP, new_batch)
}
StackP %>% 
  filter(Rank == "1st")

merge(high_stack_opt$pred, high_stack_opt$bestTune)
