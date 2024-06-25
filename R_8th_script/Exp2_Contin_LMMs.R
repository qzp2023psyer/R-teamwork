# Exp2_Contin_LMMs.R
#
# Authors: Edward Vessel, Laura Pasqualette, R. Muralikrishnan
# Last modified: 2023-06-16
# Description: compute continuous linear mixed models and generate plots for Exp. 2
# Vessel, Pasqualette, Uran, Koldehoff, Bignardi, Vinck (2023). Self-relevance predicts aesthetic appeal for real and synthetic artworks generated via neural style transfer. Psychological Science.

setwd("Experiment_2")

#Load basic libraries
library(tidyverse)
library(multcomp)
library(lme4)
library(lmerTest)



#Open data
DR_final <- read_delim("Dataratings_Final.csv", delim = ";", col_names = TRUE)
#Define Subj and Condition as factors
DR_final %>% mutate(Subj = as.factor(Subj), Cat = as.factor(Cat)) -> DR_final

############### Running linear mixed models #####################
#Arrange data first (rename columns, Fam as factor, relevel factors)
DR_final %>%
  rename(Aes = b1, Aes2 = b2, SelfR = b3, Fam = b4, Condition = Cat, Image =Imgname) %>%
  mutate(Fam = replace(Fam, Fam == 1, 1))%>%
  mutate(Fam = replace(Fam, Fam == 2, 0.5))%>%
  mutate(Fam = replace(Fam, Fam == 3, 0)) %>%
  mutate(Subj = as.factor(Subj)) %>%
  mutate(Condition = as.factor(Condition)) %>%
  mutate(Condition = fct_relevel(Condition, "RA", "GA", "SR", "NR")) %>%
  mutate(Fam = as.factor(Fam)) %>%
  mutate(Fam = fct_relevel(Fam, "1", "0.5", "0")) %>%
  mutate(SelfR = SelfR/100)%>%
  mutate(Aes = Aes/100) ->DR_final


DR_final %>%
  filter(!Subj %in% c("s02", "s11")) -> DR_final2 #without Subjects 2 and 11, which didn't 
                                                #understand the familiarity task

# LMM for prediction of Aes by SelfR with random intercepts for Subjs and Condition
aes_contin.model1 = lmer(Aes ~ SelfR + (1|Condition) +
                     (1|Subj), 
                   data=DR_final2)
summary(aes_contin.model1)
coef(aes_contin.model1)
ranef(aes_contin.model1)

#anova(aes_contin.model1)
#F_to_eta2(607.61,1,2940.2)


#Model for prediction of Aesthetic per SelfR and Familiarity as fixed effects
#random intercepts for Subjs and Condition
contrast_list <- rbind(REFAvUN = c(0.5, 0.5, -1), REvFA = c(1, -1, 0))
cMat <- ginv(contrast_list) 
colnames(cMat) <- c("REFAvUN", "REvFA")

aes_contin.model2 = lmer(Aes ~ SelfR + Fam +
                     (1|Subj) + (1|Condition), 
                   data=DR_final2,
                   contrasts = list(Fam = cMat))
summary(aes_contin.model2)
coef(aes_contin.model2)
ranef(aes_contin.model2)

#Model for prediction of Aesthetic by interaction of SelfR and Familiarity 
#Random intercept for Subjs and Condition
aes_contin.model3 = lmer(Aes ~ SelfR + Fam + 
                     (SelfR|Condition) + (SelfR|Subj), 
                   data=DR_final2, 
                   contrasts = list(Fam = cMat))
summary(aes_contin.model3)
coef(aes_contin.model3)
ranef(aes_contin.model3)

############## Model comparisons ##########################

anova(aes_contin.model1, aes_contin.model2) #model 2 is better

anova(aes_contin.model2, aes_contin.model3) #model 3 is better


##################### Spaghetti Plots ##########################
ggplot(DR_final2, aes(y = Aes, x = SelfR, group=Subj))+ 
  stat_smooth(method="lm", se=FALSE, size=.5, color="lightblue") + # slopes for different subjects
  stat_smooth(aes(group=1), method="lm", color="blue", size=1.5)+  # average slope with Standard error
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))+
  theme_classic() + 
  xlab("Self-relevance Ratings") + ylab("Aesthetic Ratings") 


ggplot(DR_final2, aes(y = Aes, x = Fam, group=Subj))+ 
  stat_smooth(method="lm", se=FALSE, size=.5, color="lightpink3") + # slopes for different subjects
  stat_smooth(aes(group=1), method="lm", color="brown1", size=1.5)+  # average slope with Standard error
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))+
  theme_classic() + 
  xlab("Familiarity Ratings") + ylab("Aesthetic Ratings") 

##### Loess plots ###########

ggplot(DR_final2, aes(x=SelfR,y=Aes))+
  geom_point(color="#222222",size=.3)+
  geom_smooth(method = "loess")+
  labs(x="Self-Relevance", y="Aesthetic Appeal")+
  theme_classic()

