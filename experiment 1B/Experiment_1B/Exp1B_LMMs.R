# Exp1B_LMMs.R
# Authors: Edward Vessel, Laura Pasqualette, Sarah Koldehoff
# Last modified: 2023-06-16
# Description: compute linear mixed models and generate plots for Exp. 1B
# Vessel, Pasqualette, Uran, Koldehoff, Bignardi, Vinck (2023). Self-relevance predicts aesthetic appeal for real and synthetic artworks generated via neural style transfer. Psychological Science.


# Script to perform primary linear mixed models for Exp 1B
#Experiment 1B的代码
setwd("")

#加载R包
library(tidyverse)
library(lme4) #doesn't include p values for fixed effects
library(lmerTest) #package that includes p values 
library(effectsize)

#读取数据，其中原代码的分隔符“;”改为“,”
testData_Orig <- read_delim("Exp1B_ImgData_testSorted_243ss.csv",delim=",", col_names = TRUE)

#筛选低信度及没有通过注意测试的被试，生成新的数据集testdata，208*42=8736
#Filter out low reliability subjects, plus those that didn't pass attention check
testData_Orig %>%
  filter(!jatosResID %in% c(
    1904,1908,2005,2011,2041,2055,2081,2090,2114,2117,2126,2132,2188,2199,2248,2256,
    2273,2287,2300,2312,2371,2416,2426,2431,2458,2461,2484,2591,2594,2629,2700,
    2163,2264,2299,2506
    )) -> testData

# #ALT: Only filter out those that didn't pass attention check (control analysis in Supplement)
# testData_Orig %>%
#   filter(!jatosResID %in% c(
#     1908,2163,2199,2264,2458,2299,2506,2594,2700
#   )) -> testData


#重命名一些变量，jatoresID变成Subj，Image变成image，并将这两个变量转化为因子；
#对该工作表进行描述，被试编号、图像、模块一的试次（42主要+2个重复）、反应时、感动得分、美感得分、模块2的试次（42+2个重复）、反应时、自我参照得分
#Rename variables, convert Subj and image to factors
testData %>% rename(Subj = jatosResID, Image = image) %>% 
  mutate(Subj = as.factor(Subj), Image = as.factor(Image)) -> testData

summary(testData)

#Baseline model, intercept only
moved.model0 = lmer(Moved ~ (1|Subj) + (1|Image),data=testData)
summary(moved.model0)


#使用线性混合模型来分析self relevance（自我相关性，以下简称SR）预测审美吸引力（感动moved和美感beauty）的结果，同时比较了不同的模型
#(1|Subj)表示SR的随机截距，(SelfRelev|Subj)表示SR随机截距和随机斜率
#(1|Image)表示考虑图像的随机截距

# Model with SR but no random slope
#模型1：包含了参与者特定的SR的随机截距（表示考虑了不同参与者对艺术作品审美吸引力有着不同的基线水平）
moved.model1 = lmer(Moved ~ SelfRelev + (1|Subj),data=testData)
summary(moved.model1)


# Model with SR and Subj Slope for SR
#模型2：包含了参与者特定的SR的随机截距和随机斜率（表示考虑了不同个体对自我相关性评分敏感度的差异）
moved.model2 = lmer(Moved ~ SelfRelev + (SelfRelev|Subj),data=testData)
summary(moved.model2)


# Primary model with SR and Subj Slope for SR, and Intercept for Image
#模型3：包含了参与者SR的随机截距和图片的随机截距（考虑图像的随机截距是因为有些图像本身就吸引人或不够吸引人）
moved.model3 = lmer(Moved ~ SelfRelev + (SelfRelev|Subj)+(1|Image),data=testData)
summary(moved.model3)


#additional stats for paper
#附加统计信息
#anova(moved.model3)：执行模型的方差分析
#summary(moved.model3)：模型摘要
#confint.merMod(moved.model3)：计算置信区间

anova(moved.model3)
summary(moved.model3)
confint.merMod(moved.model3)
F_to_eta2(294.12,1,232.88)  #208 subjs

#模型4：考虑了参与者SR的随机截距和斜率以及图像的随机截距和斜率
moved.model4 = lmer(Moved ~ SelfRelev + (SelfRelev|Subj)+(SelfRelev|Image),data=testData)
summary(moved.model4)
coef(summary(moved.model4))
anova(moved.model4)
F_to_eta2(232.21,1,154.55)  #208 subjs
confint.merMod(moved.model4)


# Model comparison
#比较四个模型的AIC值，发现模型4 AIC值更小且P小于0.05
anova(moved.model1,moved.model2,moved.model3,moved.model4)


#Spaghetti plot
##自我相关性预测审美吸引力（感动）的图
ggplot(testData, aes(x=SelfRelev, y=Moved, group=Subj))+
  stat_smooth(method="lm", se=FALSE, size=.5, color="lightblue") + # slopes for different subjects
  stat_smooth(aes(group=1), method="lm", color="blue", size=1.5)+  # average slope with SE
  theme_classic() + 
  #labs(title ="Linear Mixed Effect Model AR~SR")+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))+
  labs(x = "Self-relevance Ratings", y = "Moved Ratings") + scale_y_continuous(breaks=c(0,0.25,.5,.75,1))


## same for Beauty


#Baseline model, intercept only
#模型0：包含了参与者SR的截距和图像的截距
beauty.model0 = lmer(Beauty ~ (1|Subj) + (1|Image),data=testData)
summary(beauty.model0)

# Model with SR but no random slope
#模型1：包含了参与者SR特定的随机截距（表示考虑了不同参与者对艺术作品审美吸引力有着不同的基线水平）
beauty.model1 = lmer(Beauty ~ SelfRelev + (1|Subj),data=testData)
summary(beauty.model1)

# Model with SR and Subj Slope for SR
#模型2：包含了参与者SR特定的随机截距和随机斜率（表示考虑了不同个体对自我相关性评分敏感度的差异）
beauty.model2 = lmer(Beauty ~ SelfRelev + (SelfRelev|Subj),data=testData)
summary(beauty.model2)

# Primary model with SR and Subj Slope for SR, and Intercept for Image
#模型3：包含了参与者SR的随机截距和图片的随机截距（考虑图像的随机截距是因为有些图像本身就吸引人或不够吸引人）
beauty.model3 = lmer(Beauty ~ SelfRelev + (SelfRelev|Subj)+(1|Image),data=testData)
summary(beauty.model3)
coef(summary(beauty.model3))
anova(beauty.model3)
confint.merMod(beauty.model3)

F_to_eta2(469.67,1,230.98) #208 subj

#模型4：包含了参与者SR的截距和斜率以及图片的截距和斜率
beauty.model4 = lmer(Beauty ~ SelfRelev + (SelfRelev|Subj)+(SelfRelev|Image),data=testData)
summary(beauty.model4)
coef(summary(beauty.model4))
anova(beauty.model4)
F_to_eta2(397.7,1,176.28) #208 subj
confint.merMod(beauty.model4)

# Model comparison
#比较四个模型的AIC值，发现模型4 AIC值更小且P小于0.05
anova(beauty.model1,beauty.model2,beauty.model3,beauty.model4)

#自我相关性预测审美吸引力（美感）的图
ggplot(testData, aes(x=SelfRelev, y=Beauty, group=Subj))+
  stat_smooth(method="lm", se=FALSE, size=.5, color="lightblue") + # slopes for different subjects
  stat_smooth(aes(group=1), method="lm", color="blue", size=1.5)+  # average slope with SE
  theme_classic() + 
  #labs(title ="Linear Mixed Effect Model AR~SR")+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))+
  labs(x = "Self-relevance Ratings", y = "Beauty Ratings") + scale_y_continuous(breaks=c(0,0.25,.5,.75,1))


###### Loess plots #######
#Loess（局部加权回归散点图）是一种非参数的局部回归方法，用于展示变量之间的关系。
#Loess plots 是基于 Loess 方法绘制的图形，用于可视化变量之间的关系，特别是对于连续型变量的关系。
#以下两个图分别展示了SR和moved/beauty的关系

ggplot(testData, aes(x=SelfRelev,y=Moved))+
  geom_point(color="#222222",size=.3)+
  geom_smooth(method = "loess")+
  labs(x="Self-Relevance", y="Being Moved")+
  theme_classic()

ggplot(testData, aes(x=SelfRelev,y=Beauty))+
  geom_point(color="#222222",size=.3)+
  geom_smooth(method = "loess")+
  labs(x="Self-Relevance", y="Beauty")+
  theme_classic()



#### Demographic Data ######
#人口学变量在Exp1B_QData_243ss.csv这个文件夹里面
QData_Orig <- read.delim("Exp1B_QData_243ss.csv", sep = ";" , header = TRUE)

#demographic data of original sample 
#分析全部被试（N=243）年龄的平均值、标准差以及范围，女性的占比
mean(QData_Orig$Age)
sd(QData_Orig$Age)
range(QData_Orig$Age)
QData_Orig %>% count(Gender, sort = TRUE)

#Filter our low reliability subjects, plus those that didn't pass attention check
#筛选低信度被试
QData_Orig %>% rename(jatosResID = jatosResId) %>%
  filter(!jatosResID %in% c(
    1904,1908,2005,2011,2041,2055,2081,2090,2114,2117,2126,2132,2188,2199,2248,2256,
    2273,2287,2300,2312,2371,2416,2426,2431,2458,2461,2484,2591,2594,2629,2700,
    2163,2264,2299,2506
  )) -> QData

# #ALT: Only filter out those that didn't pass attention check (control analysis in Supplement)
# QData_Orig %>% rename(jatosResID = jatosResId) %>%
#   filter(!jatosResID %in% c(
#     1908,2163,2199,2264,2458,2299,2506,2594,2700
#   )) -> QData
#分析已经筛除低信度和没有通过注意测试的被试（N=208）的年龄的平均值、标准差以及范围，女性的占比
mean(QData$Age)
sd(QData$Age)
range(QData$Age)
QData %>% count(Gender, sort = TRUE)
