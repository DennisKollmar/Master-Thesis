library(tidyverse)
library(caret)
library(broom)
library(plotROC)
library(grid)
library(gridExtra)
library(DescTools)

setwd("~/MT-Data/validation/final")

baseline_v1_p1 <- read.csv2(dec = ".", paste("results","/","baseline_v1_p1.csv",sep = "" )) %>% 
  mutate(baseline_v1_p1 = as.numeric(good)) %>% 
  select(filename, baseline_v1_p1)
general_gap_high_v1_p1 <- read.csv2(dec = ".", paste("results","/","general_gap_high_v1_p1.csv",sep = "" )) %>% 
  mutate(general_gap_high_v1_p1 = as.numeric(good)) %>% 
  select(filename, general_gap_high_v1_p1)
general_gap_low_v1_p1 <- read.csv2(dec = ".", paste("results","/","general_gap_low_v1_p1.csv",sep = "" )) %>% 
  mutate(general_gap_low_v1_p1 = as.numeric(good)) %>% 
  select(filename, general_gap_low_v1_p1)
general_split_high_v1_p1 <- read.csv2(dec = ".", paste("results","/","general_split_high_v1_p1.csv",sep = "" )) %>% 
  mutate(general_split_high_v1_p1 = as.numeric(good)) %>% 
  select(filename, general_split_high_v1_p1)
general_split_low_v1_p1 <- read.csv2(dec = ".", paste("results","/","general_split_low_v1_p1.csv",sep = "" )) %>% 
  mutate(general_split_low_v1_p1 = as.numeric(good)) %>% 
  select(filename, general_split_low_v1_p1)


general_10 <- read.csv2(dec = ".", paste("results","/","general_10.csv",sep = "" )) %>% 
  mutate(general_10 = as.numeric(good)) %>% 
  select(filename, general_10)
general_15 <- read.csv2(dec = ".", paste("results","/","general_15.csv",sep = "" )) %>% 
  mutate(general_15 = as.numeric(good)) %>% 
  select(filename, general_15)
general_25 <- read.csv2(dec = ".", paste("results","/","general_25.csv",sep = "" )) %>% 
  mutate(general_25 = as.numeric(good)) %>% 
  select(filename, general_25)

val_vote <- read_csv2("val_10.csv") %>% 
  select(filename, truth) %>% 
  mutate(truth = as.factor(truth))

all_pred <- reduce(list(baseline_v1_p1, 
                  general_gap_high_v1_p1,
                  general_gap_low_v1_p1,
                  general_split_high_v1_p1, 
                  general_split_low_v1_p1,
                  general_10,
                  general_15,
                  general_25,
                  val_vote),
             merge)

all_vote <- cbind(all_pred, 
                  good = all_pred %>% 
                    select(baseline_v1_p1:general_25) %>% 
                    rowMeans()) %>% 
  select(filename, good)

all_vote %>% 
  write.table(file = "results/all_vote.csv", 
              sep = ";",  
              row.names = FALSE,
              quote = FALSE)

high_pred <- reduce(list(baseline_v1_p1, 
                        general_gap_high_v1_p1,
                        general_15,
                        val_vote),
                   merge)

high_vote <- cbind(high_pred, 
                  good = high_pred %>% 
                    select(baseline_v1_p1:general_15) %>% 
                    rowMeans()) %>% 
  select(filename, good)

high_vote %>% 
  write.table(file = "results/high_vote.csv", 
              sep = ";",  
              row.names = FALSE,
              quote = FALSE)
