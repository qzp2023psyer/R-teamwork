# Exp1A_LinMixedModels.R
# Authors: Edward Vessel, Laura Pasqualette, R. Muralikrishnan, Sarah Koldehoff
# Last modified: 2023-06-16
# Description: Run linear mixed models and generate plots for Exp. 1A
# Vessel, Pasqualette, Uran, Koldehoff, Bignardi, Vinck (2023). Self-relevance predicts aesthetic appeal for real and synthetic artworks generated via neural style transfer. Psychological Science.

setwd("C:/Users/86189/Desktop/R/Experiment1A") #set directory

library(tidyverse)
library(lme4) #doesn't include p values for fixed effects
library(lmerTest) #package that includes p values 
library(effectsize)


SRdata <- read_delim("Exp1A_Data.txt", delim = "\t", col_names = TRUE) #fixed variables are already considered categorical

#filter out Low Reliability Subjects (Subj#3)
SRdata %>%
  filter(!Subj %in% c(3)) -> SRdata

SRdata %>% 
  arrange(Subj, Block, Image) ->new_table #arrange variable according to Subj, Block and Image

new_table %>%
  filter(Block =="1") -> block_table1 #filter only block 1

new_table %>%
  filter(Block == "3") -> block_table3 #filter only block 3

new_table %>% # Filter only the images from block 1 that are present in block 3
  filter(Block == "1", Image%in%block_table3$Image) -> block_table1_2

new_table %>%
  filter(Block == "2") -> block_table2 #filter only block 2

new_table %>%
  filter(Block == "4") -> block_table4 #filter only block 4

new_table %>% # Filter only the images from block 2 that are present in block 4
  filter(Block == "2", Image%in%block_table4$Image) -> block_table2_2

#organizing table names
block_table1%>% 
  rename(AE=Rating,AETime=Time,AETrial=Trial) %>%
  select(Subj,Image,AE,AETime,AETrial)%>% 
  right_join(block_table2%>% 
               rename(SR=Rating,SRTime=Time,SRTrial=Trial)%>%
               select(Subj,Image,SR,SRTime,SRTrial)) -> final_table

#recode subject and image as factors
final_table$Subj <- as_factor(final_table$Subj)
final_table$Image <- as_factor(final_table$Image)

block_table1_2%>% 
  rename(AE=Rating,AETime=Time,AETrial=Trial) %>%
  select(Subj,Image,AE,AETime,AETrial)%>% 
  right_join(block_table2_2%>% 
               rename(SR=Rating,SRTime=Time,SRTrial=Trial)%>%
               select(Subj,Image,SR,SRTime,SRTrial)) -> tmpOut

block_table3%>%
  rename(AE2=Rating,AE2Time=Time,AE2Trial=Trial) %>%
  select(Subj,Image,AE2,AE2Time,AE2Trial)%>% 
  right_join(block_table4%>%
               rename(SR2=Rating,SR2Time=Time,SR2Trial=Trial) %>%
               select(Subj,Image,SR2,SR2Time,SR2Trial))  -> tmpOut2
tmpOut%>% right_join(tmpOut2) -> final_table2

# take a look at the differences between subjects in AE
ggplot(block_table1, aes(x = Subj, y = Rating, group= Subj))+
  geom_boxplot()+
  theme_classic() 
  
# take a look at the differences between subjects in SR
ggplot(block_table2, aes(x = Subj, y = Rating, group = Subj))+
  geom_boxplot()+
  theme_classic()

# LMM for how much SR explains AE + random intercepts for subj
# NOTE: data is already rescaled to between 0 and 1

AE_Test.model1 = lmer(AE ~ SR +
                     (1|Subj), 
                   data=final_table)
summary(AE_Test.model1)
coef(AE_Test.model1) #specify the intercept per subj and different slopes (when done)
ranef(AE_Test.model1)


# LMM for how much SR explains AE + random intercepts for SR/subj
AE_Test.model2 = lmer(AE ~ SR +
                     (SR|Subj), 
                   data=final_table)
summary(AE_Test.model2)
coef(AE_Test.model2)
ranef(AE_Test.model2)


# LMM for how much SR explains AE + random intercepts for SR/subj and image
AE_Test.model3 = lmer(AE ~ SR + (SR|Subj)+(1|Image),data=final_table)
summary(AE_Test.model3)
coef(AE_Test.model3)
ranef(AE_Test.model3)

confint.merMod(AE_Test.model3)

anova(AE_Test.model3)
F_to_eta2(153.51,1,32.268)


# LMM for how much SR explains AE + random intercepts for subj + image
#random slope model: different intercepts and slopes for the effect of SR
AE_Test.model4 = lmer(AE ~ SR +
                    (SR|Subj)+
                    (SR|Image), 
                  data=final_table)
summary(AE_Test.model4)
coef(AE_Test.model4)
ranef(AE_Test.model4)

#COMPARE MODELS
anova(AE_Test.model1,AE_Test.model2,AE_Test.model3,AE_Test.model4)
#model 3 wins

#baseline model, intercept only
AE_Test.model0 = lmer(AE ~ (1|Subj) + (1|Image),data=final_table)
summary(AE_Test.model0)

library(MuMIn)
r.squaredGLMM(AE_Test.model3)

## RETEST DATA ###

AE_Retest.model = lmer(AE2 ~ SR2 + (SR2|Subj)+(1|Image),data=final_table2)
summary(AE_Retest.model)
coef(summary(AE_Retest.model))
confint.merMod(AE_Retest.model)

anova(AE_Retest.model)
F_to_eta2(221.43,1,30.2)



######################### SPAGHETTI PLOTS #######################################
library(gridExtra) #package to bind together different graphs
#se = FALSE > turn off standard errors
#group = 1 > group the model in one average line


 ggplot(final_table, aes(x=SR, y=AE, group=Subj))+
  stat_smooth(method="lm", se=FALSE, size=.5, color="lightblue") + # slopes for different subjects
  stat_smooth(aes(group=1), method="lm", color="blue", size=1.5)+  # average slope with SE
  theme_classic() + 
  #labs(title ="Linear Mixed Effect Model AR~SR")+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))+
  labs(x = "Self-relevance Ratings", y = "Aesthetic Ratings")

 
 ######### Loess Plots ############
 
 
 ggplot(final_table, aes(x=SR,y=AE))+
   geom_point(color="#222222",size=.3)+
   geom_smooth(method = "loess")+
   labs(x="Self-Relevance", y="Aesthetic Appeal")+
   theme_classic()

 