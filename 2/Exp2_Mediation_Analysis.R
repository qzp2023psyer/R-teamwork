# Exp2_Mediation_Analysis.R
#
# Authors: Edward Vessel, Laura Pasqualette, R. Muralikrishnan
# Last modified: 2023-06-16
# Description: compute mediation analysis and generate plot for Exp. 2
# Vessel, Pasqualette, Uran, Koldehoff, Bignardi, Vinck (2023). Self-relevance predicts aesthetic appeal for real and synthetic artworks generated via neural style transfer. Psychological Science.

setwd("c:/Users/小矜持/Desktop/R-homework/2")

#载入包
library(MASS)
library(tidyverse)
library(mediation)

#Open data
DR_final <- read_delim("Dataratings_Final.csv", delim = ";", col_names = TRUE)
#Define factors and rename columns
DR_final %>% mutate(Subj = as.factor(Subj), Cat = as.factor(Cat))%>% #将Subj和Cat转化为因子型
  rename(Aes = b1, Aes2 = b2, SelfR = b3, Fam = b4, 
         Condition = Cat, Image =Imgname) -> DR_final #重命名

#Exclude further subjects 排除编号为2和11的被试
DR_final %>%
  filter(!Subj %in% c("s02", "s11")) -> DR_final2 #without Subjects 2 and 11, which didn't 

#Re-scale Data 对数据进行处理和转换
DR_final2 %>%
  mutate(Aes = Aes/100) %>%
  mutate(SelfR = SelfR/100) %>%
  mutate(Fam = replace(Fam, Fam == 1, 1))%>%
  mutate(Fam = replace(Fam, Fam == 2, 0.5))%>%
  mutate(Fam = replace(Fam, Fam == 3, 0)) -> DR_scaled


#############################Mediation analysis####################################
#使用了两个线性回归模型来预测两个不同的因变量，即自我相关性预测熟悉度，自我相关性和熟悉度预测审美吸引力评级

fitM <- lm(Fam ~ SelfR, data=DR_scaled) #Predict familiarity from self-relevance
fitY <- lm(Aes ~ SelfR + Fam, data=DR_scaled) #predict aesth appeal from selfR and familiarity

#Bootstrap法检验熟悉度在自我相关性和审美吸引力评级
fitMedBoot <- mediate(fitM, fitY, boot=TRUE, sims=1000, treat="SelfR", mediator="Fam")
summary(fitMedBoot)
plot(fitMedBoot)

