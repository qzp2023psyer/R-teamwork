# Exp2_QuestionClass.R
#
# Authors: Edward Vessel, Laura Pasqualette, R. Muralikrishnan
# Last modified: 2023-06-16
# Description: analysis of effect of question class on aestheic ratings for Exp. 2
# Vessel, Pasqualette, Uran, Koldehoff, Bignardi, Vinck (2023). Self-relevance predicts aesthetic appeal for real and synthetic artworks generated via neural style transfer. Psychological Science.

setwd("c:/Users/小矜持/Desktop/R-homework/2")

#载入包
library(tidyverse)
library(lme4)
library(lmerTest)
library(ggpubr)
library(ggstatsplot)

#读取文件
DR_ImgQuest <- read_delim("DatawithImgQuest.csv", delim = ",", col_names = TRUE)

#Organize file 对数据进行转换整理
DR_ImgQuest %>%
  mutate(Subj = as.factor(Subj)) %>%
  mutate(Fam = as.factor(Fam)) %>%
  mutate(Fam = fct_relevel(Fam, "1", "0.5", "0")) -> DR_ImgQuest

# adding the classification of each question从DR_ImgQuest数据框中选择特定的列，根据Question列的值创建一个新的列Quest_Type，并将Quest_Type列转换为因子类型，然后根据Subj和Trial列对数据框进行分组。处理后的数据框被重命名为DR_ImgQuest
#将问题分为几类，在文中AM是指"特定的自传体记忆（Autobio. Memory)",ID是指"个人身份"，EP是指"表达的偏好(Expressed Preferences)"，CA是指"共同活动(Common Activities)",IN是指兴趣，MI是指混合。
DR_ImgQuest %>%
  dplyr::select(Subj:Question) %>% #select the existing columns
  #create a new column with the desired values depending on the column "Question"
  mutate(
    Quest_Type = case_when(
      Question == 1 ~ "AM",
      Question == 2 ~ "AM",
      Question == 3 ~ "AM",
      Question == 4 ~ "IN",
      Question == 7 ~ "EP",
      Question == 8 ~ "ID",
      Question == 9 ~ "CA",
      Question == 13 ~ "EP",
      Question == 14 ~ "EP",
      Question == 15 ~ "ID",
      Question == 16 ~ "EP",
      Question == 17 ~ "EP",
      TRUE           ~ "MI"
    )
  ) %>%
  mutate(Quest_Type=as.factor(Quest_Type)) %>%
  group_by(Subj, Trial)->DR_ImgQuest

#add the general artworks columns
# extract GA data from ###original### file
#读取文件，并将读取的数据存储在一个名为DR_final的数据框中，使用filter()函数筛选出Cat列值为’GA’的所有行，并将筛选后的数据框重命名为GA_data
DR_final <- read_delim("Dataratings_Final.csv", delim = ";", col_names = TRUE)
DR_final%>%
  filter(Cat == 'GA') -> GA_data

#organize this data to be similar to the data frame we want to combine it with 进行一系列的重命名、转换和计算
GA_data %>%
  rename(Aes = b1, Aes2 = b2, SelfR = b3, Fam = b4, Condition = Cat, Image =Imgname) %>%
  mutate(Fam = replace(Fam, Fam == 1, 1))%>%
  mutate(Fam = replace(Fam, Fam == 2, 0.5))%>%
  mutate(Fam = replace(Fam, Fam == 3, 0)) %>%
  mutate(Subj = as.factor(Subj)) %>%
  mutate(Fam = as.factor(Fam)) %>%
  mutate(Fam = fct_relevel(Fam, "1", "0.5", "0")) %>%
  mutate(SelfR = SelfR/100)%>%
  mutate(Aes = Aes/100) %>%
  add_column('Question' = 30)%>% #add column with random number for the question number
  add_column('Quest_Type' = 'GA') -> GA_data #add it as GA as question type
# Combine both SR and GA data frames 合并
full_join(DR_ImgQuest, GA_data) ->new_DRImgQuest

#Reorganize factors so that GA comes first and it is considered the intercept 将new_DRImgQuest数据框中的Quest_Type列转换为因子类型，并重新等级化Quest_Type列的水平，将"GA"作为参考水平
new_DRImgQuest$Quest_Type <- as.factor(new_DRImgQuest$Quest_Type)
new_DRImgQuest%>% group_by(Subj, Trial) %>%
  mutate(Quest_Type = fct_relevel(Quest_Type, "GA", "CA", "EP",
                                  "AM", "ID", "IN", "MI"))->new_DRImgQuest


#### Running linear mixed model analysis on the data
## Aesthetic ratings as the predicted values
#拟合线性混合模型，并输出结果
Aes_byQtype.model = lmer(Aes ~ Quest_Type + (1|Subj), 
                   data=new_DRImgQuest)
summary(Aes_byQtype.model)
coef(Aes_byQtype.model)
ranef(Aes_byQtype.model)
confint.merMod(Aes_byQtype.model)


#plotting it 1 文中的图4，更加直观地展示自我相关性中的6类对审美吸引力评级的预测作用
ggstatsplot::ggcoefstats(Aes_byQtype.model, title = "Question Type predicting Aesthetic Ratings", 
                         point.args = list(color = "pink", shape = 8),
                         exclude.intercept = TRUE, effects = "fixed") +
  ggplot2::scale_y_discrete(labels = c("Common Activities", 
                                       "Expressed Preferences","Autobiographical Memory",
                                       "Identity", "Interest", "Mixed")) +
  ggplot2::labs(x = "Regression Coefficient", y = "Question Types")

## Self-relevance ratings as the predicted values
SelfR_byQtype.model = lmer(SelfR ~ Quest_Type + (1|Subj), 
              data=new_DRImgQuest)
summary(SelfR_byQtype.model)
coef(SelfR_byQtype.model)
ranef(SelfR_byQtype.model)
confin.merMod(SelfR_byQtype.model)

#plotting it 2 文中未给出，画图：自我相关性中的6类对自我相关性评级的预测作用
ggstatsplot::ggcoefstats(SelfR_byQtype.model, title = "Question Type predicting Self-Relevance Ratings", 
                         point.args = list(color = "pink", shape = 8),
                         exclude.intercept = TRUE, effects = "fixed") +
  ggplot2::scale_y_discrete(labels = c("Common Activities", 
                                       "Expressed Preferences","Autobiographical Memory",
                                       "Identity", "Interest", "Mixed")) +
  ggplot2::labs(x = "Regression Coefficient", y = "Question Types")

