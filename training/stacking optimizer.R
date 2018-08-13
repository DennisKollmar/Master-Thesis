# DO AFTER "STACKING"
library(gtools)
p <- seq(0, 1, 0.05)
modelgrid <- NULL

tc <- trainControl(
  method = "repeatedcv", 
  classProbs = TRUE,
  allowParallel = TRUE,
  savePredictions = "final",
  number=10, 
  repeats = 5, 
  sampling = "smote",
  summaryFunction = sixStats)

modellist <- substr(filelist, 1, nchar(filelist)-4)
modellist <- as_tibble(modellist) %>% 
  filter(value != "all_vote",
         value != "high_vote",
         value != "m35_10_55_v1_p1",
         value != "nature_v1_p1",
         value != "sport_v1_p1",
         value != "fashion_v1_gv_p2",
         value != "general_10")
modellist <- modellist[[1]]

for (i in 2:length(modellist)) {
  mg <- combinations(n = length(modellist), r = i, v = modellist, repeats.allowed = F)
  
  for (j in 1:nrow(mg)) {
    nf <- paste0("truth ~ ",paste(mg[j,], collapse = " + "))
    modelgrid <- c(modelgrid, nf)
    
  }
}

modeltab <- NULL
Stack_opt <- NULL

for (i in 1:length(modelgrid)) {

  modelform <- formula(modelgrid[i])
  
  #cl <- makeCluster(6)
  #registerDoParallel(cl)
  
  set.seed(1)  
  model <- train(form=modelform,
                        data=all_stack,
                        method="glm",
                        trControl=tc,
                        metric="F1",
                        family=binomial(link="logit"))
  
  #stopCluster(cl)
  #registerDoSEQ()
  
  new_batch <- cbind(Stack = modelText, optimal_stack(model, p, "F1"))
  Stack_opt <- rbind(Stack_opt, new_batch)
    
  modelresult <- model$results %>%
      select(ROC:F1)
    
  modeltab <- rbind(modeltab,cbind(num_models = "3",modelText, modelresult))

  print(paste0(modelgrid[i], " -->", i, " out of ", length(modelgrid)))
}
Stack_opt

p1 <- Stack_opt %>% filter(Rank =="1st") %>% mutate(Stack = modelgrid)
p1 %>% arrange(desc(F1)) %>% 
  write.csv2("opt_stack_rank1.csv")
