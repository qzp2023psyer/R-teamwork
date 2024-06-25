# Exp2_Reliability.R
#
# Authors: Edward Vessel, Laura Pasqualette, R. Muralikrishnan
# Last modified: 2023-06-16
# Description: test-retest reliability analysis for Exp. 2
# Vessel, Pasqualette, Uran, Koldehoff, Bignardi, Vinck (2023). Self-relevance predicts aesthetic appeal for real and synthetic artworks generated via neural style transfer. Psychological Science.
setwd("c:/Users/小矜持/Desktop/R-homework/2")

#加载各种包
library(tidyverse)
library(magrittr)
library(ggpubr)
library(psych)
library(ggplot2)



#################### Load raw rating data ###############################
#读取名为"Exp2_Dataratings_Full.csv"的文件，文件中的分隔符是分号，第一行是包含列名,将其内容存储到名为 "Ratings_AllSubj_raw" 的数据框中.
Ratings_AllSubj_raw <- read_delim("Exp2_Dataratings_Full.csv", delim = ";", col_names = TRUE)

#full dataset 数据框将首先按照"Subj"列进行排序，然后在"Subj"列相同的情况下按照"Block"列进行排序,最后将排序后的结果保存到名为Ratings_AllSubj的新数据框中
Ratings_AllSubj_raw %>%
  arrange(Subj, Block) -> Ratings_AllSubj



############Reliability test (test-retest) and hist plot for aesthetic appeal ##################

#Use full dataset 提取 "Block" 列值为 "b1" 的行，另一个包含了 "Block" 列值为 "b2" 的行
Ratings_AllSubj %>% filter(Block == "b1") %>% select(Subj, Trial, Response) -> b1
Ratings_AllSubj %>% filter(Block == "b2") %>% select(Subj, Trial, Response) -> b2

#对 b1 和 b2 数据框进行内连接，并按照 "Subj" 列进行分组，然后将分组后的结果拆分成多个子数据框，并存储到一个名为 ft12 的列表中
b1 %>% inner_join(b2, by = c("Subj", "Trial")) %>%
  group_by(Subj) %>%
  group_split() -> ft12

# Create an empty tibble with two columns 建立一个包含两列的新数据框（一列用于存放被试编号，一列用于存放可靠性系数）
#ft12_cortib <- tibble(Subj = double(), Reliability_Coeff = double())
ft12_cortib <- tibble(Subj = char(""), Reliability_Coeff = double())

#loop to compute the reliability score (Pearson r)
#循环，每次迭代会计算一个子数据框中 "Response.x" 和 "Response.y" 列的相关系数，并将结果添加到ft12_cortib中，最终得到一个包含了所有相关系数的数据框
for (i in 1:length(ft12)){
  # Run the correlation and store it temporarily
  cortab_temp <- cor(ft12[[i]]$Response.x, ft12[[i]]$Response.y, use = "complete.obs", method = "pearson")
  # Add the relevant values to the tibble
  ft12_cortib %<>% dplyr::add_row(Subj = ft12[[i]]$Subj[1], Reliability_Coeff = cortab_temp)

#通过 Fisher's z 分数转换和标准化得到了相关系数的平均值和标准差，并计算了95%的置信区间
}

rz<- fisherz(ft12_cortib$Reliability_Coeff) #convert r to z
sd(rz) #get the SD for the z-scores 0.3286387
mean(rz) #mean of z-scores 1.031765
#calculate the CI 95%
Lower_limit <-  mean(rz) - (1.96)*( sd(rz)) #  # (z(mean) -+ (1.96)*(SD))
Upper_limit <-  mean(rz) + (1.96)*( sd(rz)) # 
fisherz2r(mean(rz)) #convert z to r 0.77
fisherz2r(Lower_limit) # 0.37
fisherz2r(Upper_limit) #   0.93

#histogram 使用了ggplot2包来绘制一个直方图，展示了相关系数的分布情况。
ft12_cortib %>%
  ggplot(aes(x=Reliability_Coeff)) + theme_classic()+
  geom_histogram(binwidth = 0.02, color = "thistle4", fill='thistle1')+
  labs(title ="Aesthetic Ratings Reliability Test", x = "Correlation coefficient (r)", y = "# Participants")+
  theme(plot.title = element_text(hjust = 0.5, face = "bold")) + coord_cartesian(xlim=c(0,1))
range(ft12_cortib$Reliability_Coeff)


#Identify low reliability observers 找出相关系数低于0.5的观察者，并将这些观察者排除在后续的分析中
subset(ft12_cortib, Reliability_Coeff < 0.5)

#NOTE: These 5 observers were excluded from Dataratings_Final.csv, which is used for the remaining analyses

####### Compute average reliability again after removing these participants 剔除五名被试数据重新进行相关系数的计算


#Excluding subjs s23, s37, s42, s49 and s50 for reliability test below 0.5
Ratings_AllSubj_raw %>%
  arrange(Subj, Block) %>%
  filter(!Subj %in% c("s23", "s37", "s42", "s49", "s50")) -> Ratings_ReliableSubj

#Use full dataset
Ratings_ReliableSubj %>% filter(Block == "b1") %>% select(Subj, Trial, Response) -> b1r
Ratings_ReliableSubj %>% filter(Block == "b2") %>% select(Subj, Trial, Response) -> b2r


b1r %>% inner_join(b2r, by = c("Subj", "Trial")) %>%
  group_by(Subj) %>%
  group_split() -> ft12r

# Create an empty tibble with two columns
#ft12_cortib <- tibble(Subj = double(), Reliability_Coeff = double())
ft12r_cortib <- tibble(Subj = char(""), Reliability_Coeff = double())

#loop to compute the reliability score (Pearson r)
for (i in 1:length(ft12r)){
  # Run the correlation and store it temporarily
  cortab_temp <- cor(ft12r[[i]]$Response.x, ft12r[[i]]$Response.y, use = "complete.obs", method = "pearson")
  # Add the relevant values to the tibble
  ft12r_cortib %<>% dplyr::add_row(Subj = ft12r[[i]]$Subj[1], Reliability_Coeff = cortab_temp)
  
}

rzr<- fisherz(ft12r_cortib$Reliability_Coeff) #convert r to z
sd(rzr) #get the SD for the z-scores 0.3286387
mean(rzr) #mean of z-scores 1.031765
#calculate the CI 95%
Lower_limit_r <-  mean(rzr) - (1.96)*( sd(rz)) #  # (z(mean) -+ (1.96)*(SD))
Upper_limit_r <-  mean(rz) + (1.96)*( sd(rz)) # 
fisherz2r(mean(rzr)) #convert z to r 0.80
fisherz2r(Lower_limit_r) # 0.43
fisherz2r(Upper_limit_r) #   0.93

# Estimate repeatable variance, using average of squared reliability scores计算了相关系数的平方的平均值，用于估计可重复方差
mean((ft12r_cortib$Reliability_Coeff)^2)
