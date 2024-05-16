# Exp1A_Reliability.R
# Authors: Laura Pasqualette, Edward Vessel, R. Muralikrishnan, Sarah Koldehoff
# Last modified: 2023-06-16
# Description: Compute test-retest reliability scores for Exp. 1A
# Vessel, Pasqualette, Uran, Koldehoff, Bignardi, Vinck (2023). Self-relevance predicts aesthetic appeal for real and synthetic artworks generated via neural style transfer. Psychological Science.

setwd("C:/Users/86189/Desktop/R/Experiment1A") #set directory

library(tidyverse)
library(magrittr)
library(SciViews)
library(ggpubr)
library(PerformanceAnalytics)
library(lmtest)
library(olsrr)

SRdata <- read_delim("Exp1A_Data.txt", delim = "\t", col_names = TRUE) #33 participants  ##读取名为"Exp1A_Data.txt"的文本文件
summary(SRdata)  ##生成SRdata数据框的摘要统计信息


######################## Reliability Scores #######################################

## Aesthetic

SRdata %>% filter(Block == "1") %>% select(Subj, Image, Rating) -> b1  
##从SRdata数据框中筛选出Block列中值为"1"的行，并且只选择Subj、Image和Rating这三列，然后将结果存储在b1数据框中。
SRdata %>% filter(Block == "3") %>% select(Subj, Image, Rating) -> b3  
##从SRdata数据框中筛选出Block列中值为"3"的行，并且只选择Subj、Image和Rating这三列，然后将结果存储在b3数据框中。

b1 %>% inner_join(b3, by = c("Subj", "Image")) %>%  
  ##将数据框b1和b3按照Subj和Image列进行内连接，即只保留两个数据框中Subj和Image列都匹配的行。
  group_by(Subj) %>%  ##这一部分对结果进行按照Subj列的分组。
  group_split() -> ft13  ##这一部分将按照Subj列分组后的结果拆分成多个独立的数据框，并将它们存储在一个列表中，列表的名称为ft13。

# Create an empty tibble with two columns  
ft13_cortib <- tibble(Subj = double(), Correlation_Coefficient = double())  
##创建了一个名为ft13_cortib的新tibble对象。这个对象有两列，一列名为Subj，另一列名为Correlation_Coefficient，它们的数据类型都是double，即双精度浮点数。

#loop to run the reliability test
for (i in 1:length(ft13)){   
  ##这行代码是一个for循环的开始。它的作用是对ft13这个列表中的元素进行遍历，其中i从1逐渐增加到列表长度的值。
  
  # Run the correlation and store it temporarily
  cortab_temp <- cor(ft13[[i]]$Rating.x, ft13[[i]]$Rating.y, use = "complete.obs", method = "pearson")  
  ##计算了ft13列表中第i个元素的两列数据（Rating.x和Rating.y）之间的Pearson相关系数，并将结果存储在cortab_temp中。
  
  # Add the relevant values to the tibble
  ft13_cortib %<>% dplyr::add_row(Subj = i, Correlation_Coefficient = cortab_temp)
  ##代码将新计算得到的Pearson相关系数（cortab_temp）添加到名为ft13_cortib的tibble中作为新的行。
}

Correlation_Coefficients_Ae <- ft13_cortib %>% ##代码对名为ft13_cortib的tibble进行操作
  rename(Corr_Coeff_Ae = Correlation_Coefficient) ##将ft13_cortib中的列名"Correlation_Coefficient"重命名为"Corr_Coeff_Ae"。这意味着现在ft13_cortib中的相关系数列被重命名为"Corr_Coeff_Ae"。

# plot it
Correlation_Coefficients_Ae %>%  ##代码使用了ggplot2包中的ggplot函数来创建一个直方图。首先，它将名为Correlation_Coefficients_Ae的数据框传递给ggplot函数。
  ggplot(aes(x = Corr_Coeff_Ae)) + geom_histogram(binwidth = 0.05)  
##使用geom_histogram函数创建直方图层，其中binwidth参数指定了直方图条的宽度为0.05。这意味着数据将被划分为一系列具有0.05宽度的区间，并且直方图将显示每个区间中数据的频数。


## Self-Relevance

SRdata %>% filter(Block == "2") %>% select(Subj, Image, Rating) -> b2  ##表示从SRdata数据框中筛选出Block列中值为"2"的行，并且只选择Subj、Image和Rating这三列，然后将结果存储在b2数据框中。
SRdata %>% filter(Block == "4") %>% select(Subj, Image, Rating) -> b4  ##表示从SRdata数据框中筛选出Block列中值为"4"的行，并且只选择Subj、Image和Rating这三列，然后将结果存储在b4数据框中。

b2 %>% inner_join(b4, by = c("Subj", "Image")) %>%
  group_by(Subj) %>%
  group_split() -> ft24  ##代码首先对数据框b2和b4进行内连接，即只保留两个数据框中Subj和Image列都匹配的行。然后按照Subj列进行分组，并将每个分组拆分成独立的数据框，最终存储在一个名为ft24的列表中。

# Create an empty tibble with two columns
ft24_cortib <- tibble(Subj = double(), Correlation_Coefficient = double())  ##代码创建了一个新的tibble对象，名为ft24_cortib。这个对象有两列，一列名为Subj，另一列名为Correlation_Coefficient，它们的数据类型都是double，即双精度浮点数。

#run the reliability test with the loop
for (i in 1:length(ft24)){  ##代码是一个for循环的开始。它的作用是对ft24这个列表中的元素进行遍历，其中i从1逐渐增加到列表长度的值。
  
  # Run the correlation and store it temporarily
  cortab_temp <- correlation(ft24[[i]]$Rating.x, ft24[[i]]$Rating.y, use = "complete.obs", method = "pearson")  ##代码计算了ft24列表中第i个元素的两列数据（Rating.x和Rating.y）之间的Pearson相关系数，并将结果存储在cortab_temp中。这里使用了"complete.obs"参数来指定在计算相关系数时只考虑完整的观测值对，并使用了"pearson"方法来计算Pearson相关系数。
 
   # Add the relevant values to the tibble
  ft24_cortib %<>% dplyr::add_row(Subj = i, Correlation_Coefficient = cortab_temp[2])
  ##向ft24_cortib中添加一行。新行的Subj列被设置为i，而Correlation_Coefficient列被设置为cortab_temp中的第二个值。
}

Correlation_Coefficients_SR <- ft24_cortib %>%
  rename(Corr_Coeff_SR = Correlation_Coefficient)  ##将ft24_cortib中的列名"Correlation_Coefficient"重命名为"Corr_Coeff_SR"。

# plot it
Correlation_Coefficients_SR %>% 
  ggplot(aes(x = Corr_Coeff_SR)) + geom_histogram(binwidth = 0.05)
##代码使用了ggplot2包中的ggplot函数来创建一个直方图。

# Merge, Take average
library(psych)
Reliability <- full_join(Correlation_Coefficients_Ae,Correlation_Coefficients_SR) 
##代码首先加载了psych包，然后将两个数据框Correlation_Coefficients_Ae和Correlation_Coefficients_SR进行完全连接（full_join），并将结果存储在名为Reliability的新数据框中。
Reliability <- mutate(Reliability,Avg_Rel= fisherz2r((fisherz(Corr_Coeff_Ae) + fisherz(Corr_Coeff_SR)) / 2))
##代码使用mutate函数对Reliability数据框进行修改。在这里，它将新列Avg_Rel添加到Reliability数据框中。这一列的值是通过对Corr_Coeff_Ae和Corr_Coeff_SR的Fisher变换后取平均值计算得到的。

#plot it
Reliability %>% ggplot(aes(x = Avg_Rel)) + geom_histogram(binwidth = 0.02)
##创建一个直方图，使用geom_histogram函数创建直方图层，其中binwidth参数指定了直方图条的宽度为0.02。

#find those below 0.5
filter(Reliability,Avg_Rel < 0.5)
##从名为Reliability的数据框中筛选出满足条件“Avg_Rel < 0.5”的行，并返回这些行组成的新的数据框。

#ONLY SUBJECT: SUBJ 3
