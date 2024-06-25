# Exp2_Categorical_LMMs_ViolinPlots.R
#
# Authors: Edward Vessel, Laura Pasqualette, R. Muralikrishnan
# Last modified: 2023-06-16
# Description: compute main categorical linear mixed models and generate plots for Exp. 2
# Vessel, Pasqualette, Uran, Koldehoff, Bignardi, Vinck (2023). Self-relevance predicts aesthetic appeal for real and synthetic artworks generated via neural style transfer. Psychological Science.

setwd("Experiment_2")

#Load basic libraries
library(MASS)
library(tidyverse)
library(multcomp)
library(lme4)
library(lmerTest)
library(effects)
library(ggpubr)


#Load data
DR_final <- read_delim("Dataratings_Final.csv", delim = ";", col_names = TRUE)
#set Subj and Condition as factors
#rescaling Aes and SR to 0-1 **
DR_final %>% mutate(Subj = as.factor(Subj), Cat = as.factor(Cat))%>%
  rename(Aes = b1, Aes2 = b2, SelfR = b3, Fam =b4,  Condition = Cat)%>%
  mutate(SelfR = SelfR/100)%>%mutate(Aes = Aes/100)%>%mutate(Aes2 = Aes2/100) -> DR_final


##################### Analysis using contrasts###########################

########## Block 1: Aesthetic Appeal ratings ##############

#Extract block 1
DR_final %>% dplyr::select(Subj, Condition, Aes)-> Block1

con_list <- rbind(SRvNR = c(0, -1, 0, 1), RAvGA = c(-1, 0, 1, 0 ), SRvRA = c(0, 0, -1, 1))
#con_list <- rbind(SR = c(0, 0, 0, 1), NR = c(0, 1, 0, 0 ), RA = c(0, 0, 1, 0))
cMat <- ginv(con_list)
colnames(cMat) <- c("SRvOR", "RAvCG","SRvRA")
#colnames(cMat) <- c("SR","NR","RA")

b1_contrast.model <- lmer(Aes ~ Condition + (1|Subj), data=Block1, contrasts = list(Condition = cMat),REML=FALSE)
summary(b1_contrast.model)
confint.merMod(b1_contrast.model)

#Compute Cohen's d effect sizes
#Estimate of diff / sqrt(all random FX variance comps)
lmer_out.coef = coef(summary(b1_contrast.model))
lmer_out.var = as.data.frame(VarCorr(b1_contrast.model))
b1_contrast.cohD = lmer_out.coef[2:4] / sqrt(sum(lmer_out.var[,4]))

# Compute centered data for generating violin plot
Block1%>%
  group_by(Subj) %>%
  mutate(Aes_mean = mean(Aes)) %>%
  mutate(Aes_center = Aes - Aes_mean) %>%
  group_by(Subj, Condition) %>%
  mutate(Aes_subjmean = mean(Aes_center))-> Block1_centered

b1_contrast.model_C <- lmer(Aes_center ~ Condition + (1|Subj), data=Block1_centered, contrasts = list(Condition = cMat),REML=FALSE)
summary(b1_contrast.model_C)

#get the mean, SE and 95% CI of the Condition effects in the model
ef1 <- effect("Condition", b1_contrast.model_C)
summary(ef1)
# transform in data frame
c1 <- as.data.frame(ef1)

#violin plot

# Calculate the overall mean per condition
Block1_centered %>%
  group_by(Condition) %>%
  mutate(Aes_meancond = mean(Aes_center)) %>%
  select(- c(Aes, Aes_mean, Aes_center)) %>%
  distinct() -> Block1_centered_subjMean
  
# Now add Condition CI's to tibble
Block1_centered_subjMean %>%
  group_by(Condition) %>%
  mutate(upper_CI = Aes_meancond+(1.959964*c1$se)) %>% # upper 95% CI
  mutate(lower_CI = Aes_meancond-(1.959964*c1$se)) -> Block1_centered_subjMean # lower 95% CI


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


#primary model
b3_contrast.model <- lmer(SelfR ~ Condition + (1|Subj), data=Block3, contrasts = list(Condition = cMat),REML=FALSE)
summary(b3_contrast.model)
confint.merMod(b3_contrast.model)

#post-hoc comparisons using  multcomp::glht to get OR vs CG estimate (e.g. NR vs GA in old terminology)
summary(glht(b3_contrast.model, linfct = mcp(Condition = "Tukey")),test = adjusted("holm"))

#run a second model to get confidence interval estimates and t-value for OR vs CG contrast
con_list2 <- rbind(SRvNR = c(0, -1, 0, 1), RAvGA = c(-1, 0, 1, 0 ), NRvGA = c(-1, 1, 0, 0))
cMat2 <- ginv(con_list2)
colnames(cMat2) <- c("SRvOR", "RAvCG","ORvCG")
b3_contrast.model2 <- lmer(SelfR ~ Condition + (1|Subj), data=Block3, contrasts = list(Condition = cMat2),REML=FALSE)
summary(b3_contrast.model2)
confint.merMod(b3_contrast.model2)

#Compute Cohen's d effect sizes
#Estimate of diff / sqrt(all random FX variance comps)
lmer_out3.coef = coef(summary(b3_contrast.model2))
lmer_out3.var = as.data.frame(VarCorr(b3_contrast.model2))
b3_contrast.cohD = lmer_out3.coef[2:4] / sqrt(sum(lmer_out3.var[,4]))


# Compute centered data for generating violin plot
Block3%>%
  group_by(Subj) %>%
  mutate(SelfR_mean = mean(SelfR)) %>%
  mutate(SelfR_center = SelfR - SelfR_mean) %>%
  group_by(Subj, Condition) %>%
  mutate(SelfR_subjmean = mean(SelfR_center))-> Block3_centered

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

# Now add Condition CI's to tibble
Block3_centered_subjMean %>%
  group_by(Condition) %>%
  mutate(upper_CI = SelfR_meancond+(1.959964*c1$se)) %>% # upper 95% CI
  mutate(lower_CI = SelfR_meancond-(1.959964*c1$se)) -> Block3_centered_subjMean # lower 95% CI


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
