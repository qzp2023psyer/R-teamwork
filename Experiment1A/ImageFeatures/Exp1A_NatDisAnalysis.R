# Exp1A_NatDisAnalysis.R
# Authors: Edward Vessel
# Last modified: 2023-06-16
# Description: Analyze image ratings of Naturalness and Disorder for Exp. 1A, write out average ratings
# Vessel, Pasqualette, Uran, Koldehoff, Bignardi, Vinck (2023). Self-relevance predicts aesthetic appeal for real and synthetic artworks generated via neural style transfer. Psychological Science.


setwd("") #set directory

library(tidyverse)
library(magrittr)
library(SciViews)
library(ggplot2)
#library(dplyr)
library(ltm)
library(PerformanceAnalytics)


# === compute test-retest scores for each participant ========

ND_imgData_retestSorted <-  read_delim("Exp1A_NatDis_retestSorted_27ss.csv", delim = ";", col_names = TRUE) #27 participants
ND_imgData_retestSorted %>% rename(Subj = jatosResID, t1_Nat = t1_x1, t2_Nat = t2_x1, t1_Dis = t1_x2, t2_Dis = t2_x2) -> ND_imgData_retestSorted

nImg = 6
nSub = 27


ND_imgData_retestSorted %>% 
  group_by(Subj) %>%
  group_split() -> retestSplit

# Create an empty tibble with two columns
testRetest_corr <- tibble(Subj = double(), ccNat = double(), ccDis = double())

for (i in 1:nSub){
  
  # Compute correlation and store it temporarily
  cortemp_Nat <- cor(retestSplit[[i]]$t1_Nat, retestSplit[[i]]$t2_Nat, use = "complete.obs", method = "pearson")
  cortemp_Dis <- cor(retestSplit[[i]]$t1_Dis, retestSplit[[i]]$t2_Dis, use = "complete.obs", method = "pearson")
  
  # Add the relevant values to the tibble
  testRetest_corr %<>% dplyr::add_row(Subj = i, ccNat = cortemp_Nat, ccDis = cortemp_Dis)
  
}

# No participant has r-values below 0.5 for BOTH Naturalness and Disorder.
# All participants are retained

# ======== average test data over all participants ======

ND_imgData_testSorted <- read_delim("Exp1A_NatDis_testSorted_27ss.csv", delim = ";", col_names = TRUE) #27 participants
#data is sorted by image, image is listed by image name (character string)
#x1 = naturalness
#x2 = disorder

nImg = 148
nSub = 27

imgNum <- rep(1:nImg,nSub)
ND_imgData_testSorted <- cbind(ND_imgData_testSorted,imgNum)

ND_imgData_testSorted %>% 
  rename(Natur=x1,Disord=x2,Trial=trial_main,Sub=jatosResID) -> ND_imgData_testSorted

ND_imgData_testSorted$Sub  = as.factor(ND_imgData_testSorted$Sub)
ND_imgData_testSorted$imgNum = as.factor(ND_imgData_testSorted$imgNum)



#Compute means across participants
ND_imgData_testSorted %>%
  group_by(imgNum) %>%
  summarise_at(vars(Natur, Disord), list(mean)) -> ND_means

imgNames <- ND_imgData_testSorted$image[1:nImg]
ND_means <- cbind(ND_means,imgNames)

#write out
#write.csv(ND_means,"ND_means.csv", row.names=FALSE)




