# DO AFTER "STACKING"
library(gtools)
p <- seq(0, 1, 0.05)
r <- 1
modelgrid <- NULL
opt_vote <- NULL

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
  
  for (d in 1:nrow(mg)) {
    
    votename <- paste0(mg[d,], collapse = "+")
    
    soft <- all_stack[mg[d,]] %>% 
    rowMeans()
    softp <- cut(as.numeric(soft), breaks = c(-Inf, 0.5, Inf), 
               labels = c("not", "good"))
    
    conf <- confusionMatrix(data = softp, 
                          reference = all_stack$truth,
                          positive = "good", mode = "everything")
    Kappa <- conf$overall[2]
    Precision <- conf$byClass[5]
    Recall <- conf$byClass[6]
    F1 <- conf$byClass[7]
    Accuracy <- conf$overall[1]
    
    optimal_tmp <- tibble(Vote = votename, P = 0.5, Kappa, Precision, Recall, F1, Accuracy)
    opt_vote <- rbind(opt_vote, optimal_tmp)
    
    print(paste("added model", votename))
    
    Accuracy <- NULL
    Kappa <- NULL
    Precision <- NULL
    Recall <- NULL
    F1 <- NULL
    
    for (j in 1:length(p)) {
    softp <- cut(as.numeric(soft), breaks = c(-Inf, p[j], Inf), 
                 labels = c("not", "good"))
    
    conf <- confusionMatrix(data = softp, 
                            reference = all_stack$truth,
                            positive = "good", mode = "everything")
    Kappa[j] <- conf$overall[2]
    Precision[j] <- conf$byClass[5]
    Recall[j] <- conf$byClass[6]
    F1[j] <- conf$byClass[7]
    Accuracy[j] <- conf$overall[1]
    }
    
    optimal_tmp <- tibble(Vote = paste0(votename, "_opt"), P = p, Kappa, Precision, Recall, F1, Accuracy)
    optimal_tmp <- optimal_tmp %>% arrange(desc(F1))
    optimal_tmp <- optimal_tmp[1,]
    
    opt_vote <- rbind(opt_vote, optimal_tmp)
    
    print(paste("added model", votename, "opt", "--->", d, "out of", nrow(mg)))
    
  }
  print(paste("modellist", i, "out of",length(modellist), "finished"))
}
opt_vote %>% arrange(desc(F1)) %>% tail()

l <- c("general_15", "general_25", "general_gap_high_v1_p1", "general_gap_low_v1_p1", "m10_10_80_v1_p1")

soft <- all_stack[l] %>% 
  rowMeans()
softp <- cut(as.numeric(soft), breaks = c(-Inf, 0.6, Inf), 
             labels = c("not", "good"))

conf <- confusionMatrix(data = softp, 
                        reference = all_stack$truth,
                        positive = "good", mode = "everything")
conf
