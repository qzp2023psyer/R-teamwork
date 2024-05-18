# Exp2_Categorical_LMMs_ViolinPlots.R
#
# Authors: Edward Vessel, Laura Pasqualette, R. Muralikrishnan
# Last modified: 2023-06-16
# Description: compute main categorical linear mixed models and generate plots for Exp. 2
# Vessel, Pasqualette, Uran, Koldehoff, Bignardi, Vinck (2023). Self-relevance predicts aesthetic appeal for real and synthetic artworks generated via neural style transfer. Psychological Science.

setwd("c:/Users/小矜持/Desktop/R-homework/2")

#Load basic libraries 载入包
library(MASS)
library(tidyverse)
library(multcomp)
library(lme4)
library(lmerTest)
library(effects)
library(ggpubr)


#Load data 读取数据
DR_final <- read_delim("Dataratings_Final.csv", delim = ";", col_names = TRUE)
#set Subj and Condition as factors
#rescaling Aes and SR to 0-1 **
#将 "Subj" 列和 "Cat" 列转换为因子变量（factor）
DR_final %>% mutate(Subj = as.factor(Subj), Cat = as.factor(Cat))%>%
#将 "b1" 列重命名为 "Aes"，"b2" 列重命名为 "Aes2"，"b3" 列重命名为 "SelfR"，"b4" 列重命名为 "Fam"
rename(Aes = b1, Aes2 = b2, SelfR = b3, Fam =b4,  Condition = Cat)%>%
#将这三列的值从0到100的范围转换到0到1的范围内
mutate(SelfR = SelfR/100)%>%mutate(Aes = Aes/100)%>%mutate(Aes2 = Aes2/100) -> DR_final


##################### Analysis using contrasts###########################

########## Block 1: Aesthetic Appeal ratings ##############

#Extract block 1
#选择了 "DR_final" 数据框中的 "Subj"、"Condition" 和 "Aes" 列，并将结果存储在名为 "Block1" 的新数据框中
DR_final %>% dplyr::select(Subj, Condition, Aes)-> Block1
#这一行创建了一个矩阵 con_list，其中每一行代表一个条件对比。在这个矩阵中，每行的元素分别对应于条件对比矩阵中的条目
con_list <- rbind(SRvNR = c(0, -1, 0, 1), RAvGA = c(-1, 0, 1, 0 ), SRvRA = c(0, 0, -1, 1))
#con_list <- rbind(SR = c(0, 0, 0, 1), NR = c(0, 1, 0, 0 ), RA = c(0, 0, 1, 0))
#计算了条件对比矩阵的广义逆矩阵
cMat <- ginv(con_list)
#为 cMat 矩阵的列设置了新的列名。在这里，它将列名分别设置为 "SRvOR"、"RAvCG" 和 "SRvRA"
colnames(cMat) <- c("SRvOR", "RAvCG","SRvRA")
#colnames(cMat) <- c("SR","NR","RA")


#使用了线性混合效应模型（LMM）来拟合数据，然后通过summary()函数和confint.merMod()函数进行模型的摘要统计和置信区间计算。
b1_contrast.model <- lmer(Aes ~ Condition + (1|Subj), data=Block1, contrasts = list(Condition = cMat),REML=FALSE)
summary(b1_contrast.model)
confint.merMod(b1_contrast.model)

#Compute Cohen's d effect sizes
#Estimate of diff / sqrt(all random FX variance comps)
#计算线性混合模型中固定效应的 Cohen's d 效应大小，以帮助评估模型中不同因素之间的差异程度
lmer_out.coef = coef(summary(b1_contrast.model))
lmer_out.var = as.data.frame(VarCorr(b1_contrast.model))
b1_contrast.cohD = lmer_out.coef[2:4] / sqrt(sum(lmer_out.var[,4]))

# Compute centered data for generating violin plot
Block1%>%
  group_by(Subj) %>%
  mutate(Aes_mean = mean(Aes)) %>% #计算了每个Subj 组中 Aes 列的均值，并将结果存储在新的列 Aes_mean 中
  mutate(Aes_center = Aes - Aes_mean) %>%
  group_by(Subj, Condition) %>%
  mutate(Aes_subjmean = mean(Aes_center))-> Block1_centered #计算了每个 Subj 和 Condition 组合内 Aes_center 列的均值，并将结果存储在新的列 Aes_subjmean 中

#用于拟合一个线性混合模型（lmer），其中因变量为中心化后的Aes（Aes_center），自变量为Condition，随机效应包括每个被试（Subj）的随机截距
b1_contrast.model_C <- lmer(Aes_center ~ Condition + (1|Subj), data=Block1_centered, contrasts = list(Condition = cMat),REML=FALSE)
summary(b1_contrast.model_C)

#get the mean, SE and 95% CI of the Condition effects in the model
ef1 <- effect("Condition", b1_contrast.model_C)
summary(ef1)
# transform in data frame
c1 <- as.data.frame(ef1) #转化为数据框

#violin plot

# Calculate the overall mean per condition 按照 Condition 变量分组、计算每个分组内 Aes_center 列的均值、去除不必要的列，并保留唯一的行
Block1_centered %>%
  group_by(Condition) %>%
  mutate(Aes_meancond = mean(Aes_center)) %>%
  select(- c(Aes, Aes_mean, Aes_center)) %>%
  distinct() -> Block1_centered_subjMean
  
# Now add Condition CI's to tibble 按照 Condition 变量分组、计算每个分组内 Aes_meancond 列的上下置信区间，并将结果存储在相应的列中
Block1_centered_subjMean %>%
  group_by(Condition) %>%
  mutate(upper_CI = Aes_meancond+(1.959964*c1$se)) %>% # upper 95% CI
  mutate(lower_CI = Aes_meancond-(1.959964*c1$se)) -> Block1_centered_subjMean # lower 95% CI

#使用 ggplot2 包创建了一个小提琴图，并在图上添加了点、误差线和颜色填充
ggplot(Block1_centered_subjMean, aes(x=Condition, y=Aes_subjmean, color=Condition, fill = Condition, alpha = 0.3)) + 
  geom_violin(trim=TRUE) +
  geom_jitter(aes(colour = Condition), alpha=1, shape=16, position=position_jitter(0.1)) +
  scale_x_discrete(limits = c("SR", "NR", "GA", "RA"), 
                   labels = c('Self-relevant','Other-relevant','Control Generated',
                              'Real artworks')) +
  scale_color_manual(values = c("gray", 
                                "steelblue",
                                "thistle",
                                "brown1"))+
  scale_fill_manual(values=c("gray", "steelblue", "thistle", "brown1"))+
  theme_classic()  + rremove("legend") +
  ggtitle("Contrasts for Aesthetic Evaluation") +
  theme(plot.title = element_text(size = 14, hjust = 0.5), axis.text = element_text(size = 12),
        axis.title = element_text(size = 13), ) +
  ylab("Aesthetic Ratings") + geom_point(aes(x=Condition, y=Aes_meancond), size=3, color = "Black") +
  geom_errorbar(aes(x=Block1_centered_subjMean$Condition, ymax=Block1_centered_subjMean$upper_CI, 
                    ymin=Block1_centered_subjMean$lower_CI), stat='identity', width=.25, color = "Black") #+
#scale_y_continuous(limits = c(0,100)) 




############ Block 3:  Self Relevance ratings  ################
DR_final %>% dplyr::select(Subj, Condition, SelfR) -> Block3


#primary model 拟合了一个线性混合模型 b3_contrast.model，其中因变量是 SelfR，自变量是 Condition，随机效应是每个 Subj 的随机截距
b3_contrast.model <- lmer(SelfR ~ Condition + (1|Subj), data=Block3, contrasts = list(Condition = cMat),REML=FALSE)
summary(b3_contrast.model)
confint.merMod(b3_contrast.model)

#post-hoc comparisons using  multcomp::glht to get OR vs CG estimate (e.g. NR vs GA in old terminology)
#对线性混合模型 b3_contrast.model 中的 Condition 变量进行 Tukey 校正后的多重比较
summary(glht(b3_contrast.model, linfct = mcp(Condition = "Tukey")),test = adjusted("holm"))

#run a second model to get confidence interval estimates and t-value for OR vs CG contrast
#创建了一个比较矩阵 con_list2。该矩阵指定了三组比较，分别是 SR 对 NR、RA 对 GA、NR 对 GA。矩阵中的每一行代表一个比较，每个数字代表了对应 Condition 水平的权重
con_list2 <- rbind(SRvNR = c(0, -1, 0, 1), RAvGA = c(-1, 0, 1, 0 ), NRvGA = c(-1, 1, 0, 0))
#用 ginv 函数计算了比较矩阵的广义逆矩阵
cMat2 <- ginv(con_list2)
#给广义逆矩阵的列重新命名，分别对应 SR 对 OR、RA 对 CG、OR 对 CG 的比较
colnames(cMat2) <- c("SRvOR", "RAvCG","ORvCG")
#重新拟合了一个线性混合模型 b3_contrast.model2
b3_contrast.model2 <- lmer(SelfR ~ Condition + (1|Subj), data=Block3, contrasts = list(Condition = cMat2),REML=FALSE)
#生成了重新拟合模型 b3_contrast.model2 的摘要统计信息
summary(b3_contrast.model2)
#计算了拟合模型 b3_contrast.model2 的参数估计的置信区间
confint.merMod(b3_contrast.model2)

#Compute Cohen's d effect sizes 计算线性混合模型 b3_contrast.model2 的 Cohen's d 效应大小
#Estimate of diff / sqrt(all random FX variance comps)
lmer_out3.coef = coef(summary(b3_contrast.model2))
lmer_out3.var = as.data.frame(VarCorr(b3_contrast.model2))
b3_contrast.cohD = lmer_out3.coef[2:4] / sqrt(sum(lmer_out3.var[,4]))
print(b3_contrast.cohD)

# Compute centered data for generating violin plot 对一个名为Block3的数据框进行一系列的操作，最终得到一个中心化处理后的新数据框Block3_centered
Block3%>%
  group_by(Subj) %>%
  mutate(SelfR_mean = mean(SelfR)) %>%
  mutate(SelfR_center = SelfR - SelfR_mean) %>%
  group_by(Subj, Condition) %>%
  mutate(SelfR_subjmean = mean(SelfR_center))-> Block3_centered

#用lme4包中的lmer()函数来拟合一个线性混合效应模型
b3_contrast.model_C <- lmer(SelfR_center ~ Condition + (1|Subj), data=Block3_centered, contrasts = list(Condition = cMat),REML=FALSE)
summary(b3_contrast.model_C)

#get the mean, SE and 95% CI of the Condition effects in the model
ef3 <- effect("Condition", b3_contrast.model_C)
summary(ef3)
# transform in data frame
c3 <- as.data.frame(ef3)

#violin plot

# Calculate the overall mean per condition
Block3_centered %>%
  group_by(Condition) %>%
  mutate(SelfR_meancond = mean(SelfR_center)) %>%
  select(- c(SelfR, SelfR_mean, SelfR_center)) %>%
  distinct() -> Block3_centered_subjMean

# Now add Condition CI's to tibble 使用dplyr包中的group_by()和mutate()函数来对一个已经按照Condition分组的数据框Block3_centered_subjMean进行操作，目的是计算每个条件下SelfR_meancond的平均值上下95%的置信区间
Block3_centered_subjMean %>%
  group_by(Condition) %>%
  mutate(upper_CI = SelfR_meancond+(1.959964*c1$se)) %>% # upper 95% CI
  mutate(lower_CI = SelfR_meancond-(1.959964*c1$se)) -> Block3_centered_subjMean # lower 95% CI

#使用 ggplot 包创建了一个小提琴图，并在图上添加了点、误差线和颜色填充
ggplot(Block3_centered_subjMean, aes(x=Condition, y=SelfR_subjmean, color=Condition, fill = Condition, alpha = 0.3)) + 
  geom_violin(trim=TRUE) +
  geom_jitter(aes(colour = Condition), alpha=1, shape=16, position=position_jitter(0.1)) +
  scale_x_discrete(limits = c("SR", "NR", "GA", "RA"), 
                   labels = c('Self-relevant','Other-relevant','Control Generated',
                              'Real artworks')) +
  scale_color_manual(values = c("gray", 
                                "steelblue",
                                "thistle",
                                "brown1"))+
  scale_fill_manual(values=c("gray", "steelblue", "thistle", "brown1"))+
  theme_classic()  + rremove("legend") +
  ggtitle("Contrasts for Self-Relevance Ratings") +
  theme(plot.title = element_text(size = 14, hjust = 0.5), axis.text = element_text(size = 12),
        axis.title = element_text(size = 13), ) +
  ylab("Self-Relevance Ratings") + geom_point(aes(x=Condition, y=SelfR_meancond), size=3, color = "Black") +
  geom_errorbar(aes(x=Block3_centered_subjMean$Condition, ymax=Block3_centered_subjMean$upper_CI, 
                    ymin=Block3_centered_subjMean$lower_CI), stat='identity', width=.25, color = "Black") #+
#scale_y_continuous(limits = c(0,100)) 
