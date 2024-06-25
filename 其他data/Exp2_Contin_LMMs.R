# Exp2_Contin_LMMs.R
#
# Authors: Edward Vessel, Laura Pasqualette, R. Muralikrishnan
# Last modified: 2023-06-16
# Description: compute continuous linear mixed models and generate plots for Exp. 2
# Vessel, Pasqualette, Uran, Koldehoff, Bignardi, Vinck (2023). Self-relevance predicts aesthetic appeal for real and synthetic artworks generated via neural style transfer. Psychological Science.

setwd("c:/Users/小矜持/Desktop/R-homework/2")

#Load basic libraries 加载包
library(tidyverse)
library(multcomp)
library(lme4)
library(lmerTest)



#Open data 使用read_delim函数从名为"Dataratings_Final.csv"的文件中读取数据
DR_final <- read_delim("Dataratings_Final.csv", delim = ";", col_names = TRUE)
#Define Subj and Condition as factors 将Subj和Cat两列转换成因子类型，将处理后的数据框保存回原来的DR_final中
DR_final %>% mutate(Subj = as.factor(Subj), Cat = as.factor(Cat)) -> DR_final

############### Running linear mixed models #####################
#Arrange data first (rename columns, Fam as factor, relevel factors)
DR_final %>%
  rename(Aes = b1, Aes2 = b2, SelfR = b3, Fam = b4, Condition = Cat, Image =Imgname) %>% #重新命名
  mutate(Fam = replace(Fam, Fam == 1, 1))%>%
  mutate(Fam = replace(Fam, Fam == 2, 0.5))%>%
  mutate(Fam = replace(Fam, Fam == 3, 0)) %>% ##Fam列的原始值被替换为1、0.5和0
  mutate(Subj = as.factor(Subj)) %>% 
  mutate(Condition = as.factor(Condition)) %>% 
  mutate(Condition = fct_relevel(Condition, "RA", "GA", "SR", "NR")) %>% #将Subj和Condition列转换为因子类型
  mutate(Fam = as.factor(Fam)) %>% #将Fam列转换为因子类型
  mutate(Fam = fct_relevel(Fam, "1", "0.5", "0")) %>%
  mutate(SelfR = SelfR/100)%>% #将SelfR和Aes列的值转换为原来的百分比值
  mutate(Aes = Aes/100) ->DR_final #将处理后的数据框重新保存回DR_final中


DR_final %>%
  filter(!Subj %in% c("s02", "s11")) -> DR_final2 #without Subjects 2 and 11, which didn't 
                                                #understand the familiarity task 排除编号为2和11的被试

# LMM for prediction of Aes by SelfR with random intercepts for Subjs and Condition Aes是因变量，SelfR是第一个固定效应的变量，Condition和Subj是随机效应的变量
aes_contin.model1 = lmer(Aes ~ SelfR + (1|Condition) +
                     (1|Subj), 
                   data=DR_final2)
summary(aes_contin.model1) #获取模型的摘要信息
coef(aes_contin.model1) #获取模型系数的估计值
ranef(aes_contin.model1) #获取模型中随机效应的估计值

#anova(aes_contin.model1)
#F_to_eta2(607.61,1,2940.2)


#Model for prediction of Aesthetic per SelfR and Familiarity as fixed effects
#random intercepts for Subjs and Condition
contrast_list <- rbind(REFAvUN = c(0.5, 0.5, -1), REvFA = c(1, -1, 0)) #创建一个对比矩阵contrast_list，它包含了两组对比的系数，这些系数决定了如何比较不同的条件或处理
cMat <- ginv(contrast_list) 
colnames(cMat) <- c("REFAvUN", "REvFA")

#拟合线性混合效应模型，并输出一系列值
aes_contin.model2 = lmer(Aes ~ SelfR + Fam +
                     (1|Subj) + (1|Condition), 
                   data=DR_final2,
                   contrasts = list(Fam = cMat))
summary(aes_contin.model2)
coef(aes_contin.model2)
ranef(aes_contin.model2)

#Model for prediction of Aesthetic by interaction of SelfR and Familiarity 
#Random intercept for Subjs and Condition #拟合线性混合效应模型，并输出一系列值
aes_contin.model3 = lmer(Aes ~ SelfR + Fam + 
                     (SelfR|Condition) + (SelfR|Subj), 
                   data=DR_final2, 
                   contrasts = list(Fam = cMat))
summary(aes_contin.model3)
coef(aes_contin.model3)
ranef(aes_contin.model3)
#这里得出的slope值不太一样

############## Model comparisons 比较两个或多个线性混合效应模型，以确定哪个模型提供了更好的拟合##########################

anova(aes_contin.model1, aes_contin.model2) #model 2 is better

anova(aes_contin.model2, aes_contin.model3) #model 3 is better


##################### Spaghetti Plots 绘制热图，线性混合效应模型的可视化 ##########################
ggplot(DR_final2, aes(y = Aes, x = SelfR, group=Subj))+ 
  stat_smooth(method="lm", se=FALSE, size=.5, color="lightblue") + # slopes for different subjects
  stat_smooth(aes(group=1), method="lm", color="blue", size=1.5)+  # average slope with Standard error
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))+
  theme_classic() + 
  xlab("Self-relevance Ratings") + ylab("Aesthetic Ratings") #设置图形的主题，包括字体、颜色、边距等


ggplot(DR_final2, aes(y = Aes, x = Fam, group=Subj))+ 
  stat_smooth(method="lm", se=FALSE, size=.5, color="lightpink3") + # slopes for different subjects
  stat_smooth(aes(group=1), method="lm", color="brown1", size=1.5)+  # average slope with Standard error
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))+
  theme_classic() + 
  xlab("Familiarity Ratings") + ylab("Aesthetic Ratings") 

##### Loess plots ggplot包创建散点图，其中包含了线性局部加权回归（loess）平滑线 ###########

ggplot(DR_final2, aes(x=SelfR,y=Aes))+
  geom_point(color="#222222",size=.3)+
  geom_smooth(method = "loess")+
  labs(x="Self-Relevance", y="Aesthetic Appeal")+
  theme_classic()

