# Exp1B_LMMs.R
# Authors: Edward Vessel, Laura Pasqualette, Sarah Koldehoff
# Last modified: 2023-06-16
# Description: compute linear mixed models and generate plots for Exp. 1B
# Vessel, Pasqualette, Uran, Koldehoff, Bignardi, Vinck (2023). Self-relevance predicts aesthetic appeal for real and synthetic artworks generated via neural style transfer. Psychological Science.

#
# Script to perform primary linear mixed models for Exp 1B

setwd("")

library(tidyverse)
library(lme4) #doesn't include p values for fixed effects
library(lmerTest) #package that includes p values 
library(effectsize)

testData_Orig <- read_delim("Exp1B_ImgData_testSorted_243ss.csv",delim=";", col_names = TRUE)

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


#Rename variables, convert Subj and image to factors
testData %>% rename(Subj = jatosResID, Image = image) %>% 
  mutate(Subj = as.factor(Subj), Image = as.factor(Image)) -> testData

summary(testData)

#Baseline model, intercept only
moved.model0 = lmer(Moved ~ (1|Subj) + (1|Image),data=testData)
summary(moved.model0)

# Model with SR but no random slope
moved.model1 = lmer(Moved ~ SelfRelev + (1|Subj),data=testData)
summary(moved.model1)

# Model with SR and Subj Slope for SR
moved.model2 = lmer(Moved ~ SelfRelev + (SelfRelev|Subj),data=testData)
summary(moved.model2)

# Primary model with SR and Subj Slope for SR, and Intercept for Image
moved.model3 = lmer(Moved ~ SelfRelev + (SelfRelev|Subj)+(1|Image),data=testData)
summary(moved.model3)

#additional stats for paper
anova(moved.model3)
summary(moved.model3)
confint.merMod(moved.model3)
F_to_eta2(294.12,1,232.88)  #208 subjs


moved.model4 = lmer(Moved ~ SelfRelev + (SelfRelev|Subj)+(SelfRelev|Image),data=testData)
summary(moved.model4)
coef(summary(moved.model4))
anova(moved.model4)
F_to_eta2(232.21,1,154.55)  #208 subjs
confint.merMod(moved.model4)


# Model comparison
anova(moved.model1,moved.model2,moved.model3,moved.model4)


#Spaghetti plot
ggplot(testData, aes(x=SelfRelev, y=Moved, group=Subj))+
  stat_smooth(method="lm", se=FALSE, size=.5, color="lightblue") + # slopes for different subjects
  stat_smooth(aes(group=1), method="lm", color="blue", size=1.5)+  # average slope with SE
  theme_classic() + 
  #labs(title ="Linear Mixed Effect Model AR~SR")+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))+
  labs(x = "Self-relevance Ratings", y = "Moved Ratings") + scale_y_continuous(breaks=c(0,0.25,.5,.75,1))


## same for Beauty


#Baseline model, intercept only
beauty.model0 = lmer(Beauty ~ (1|Subj) + (1|Image),data=testData)
summary(beauty.model0)

# Model with SR but no random slope
beauty.model1 = lmer(Beauty ~ SelfRelev + (1|Subj),data=testData)
summary(beauty.model1)

# Model with SR and Subj Slope for SR
beauty.model2 = lmer(Beauty ~ SelfRelev + (SelfRelev|Subj),data=testData)
summary(beauty.model2)

# Primary model with SR and Subj Slope for SR, and Intercept for Image
beauty.model3 = lmer(Beauty ~ SelfRelev + (SelfRelev|Subj)+(1|Image),data=testData)
summary(beauty.model3)
coef(summary(beauty.model3))
anova(beauty.model3)
confint.merMod(beauty.model3)

F_to_eta2(469.67,1,230.98) #208 subj


beauty.model4 = lmer(Beauty ~ SelfRelev + (SelfRelev|Subj)+(SelfRelev|Image),data=testData)
summary(beauty.model4)
coef(summary(beauty.model4))
anova(beauty.model4)
F_to_eta2(397.7,1,176.28) #208 subj
confint.merMod(beauty.model4)

# Model comparison
anova(beauty.model1,beauty.model2,beauty.model3,beauty.model4)

ggplot(testData, aes(x=SelfRelev, y=Beauty, group=Subj))+
  stat_smooth(method="lm", se=FALSE, size=.5, color="lightblue") + # slopes for different subjects
  stat_smooth(aes(group=1), method="lm", color="blue", size=1.5)+  # average slope with SE
  theme_classic() + 
  #labs(title ="Linear Mixed Effect Model AR~SR")+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))+
  labs(x = "Self-relevance Ratings", y = "Beauty Ratings") + scale_y_continuous(breaks=c(0,0.25,.5,.75,1))


###### Loess plots #######

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

QData_Orig <- read.delim("Exp1B_QData_243ss.csv", sep = ";" , header = TRUE)

#demographic data of original sample 
mean(QData_Orig$Age)
sd(QData_Orig$Age)
range(QData_Orig$Age)
QData_Orig %>% count(Gender, sort = TRUE)

#Filter our low reliability subjects, plus those that didn't pass attention check
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

mean(QData$Age)
sd(QData$Age)
range(QData$Age)
QData %>% count(Gender, sort = TRUE)
