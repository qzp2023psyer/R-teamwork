#Author: Giacomo Bignardi
#Adapted from: Vessel et al., 2022 https://osf.io/6zxc5/
#Date: 09-02-2023
#Last modified: 05-05-2023
#Description: 
#-1-Extend LMM to include Image Features as predictors
#-2-Apply dominance analysis to confirm major relative contribution of self-relevance to aesthetic appeal
#Program: Review_Image_Features(IF) n'j'j
# Vessel, Pasqualette, Uran, Koldehoff, Bignardi, Vinck (2023). Self-relevance predicts aesthetic appeal for real and synthetic artworks generated via neural style transfer. Psychological Science.

------------------------------------------------------------------------------------------------------------------------------

#load packages
  
conflict_prefer("filter", "dplyr")
conflict_prefer("lag", "dplyr")library(conflicted)
library(tidyverse)
library(tidylog)
library(readr)
library(lme4)
library(umx) #include function to residualize AR 
library(patchwork) #makes plotting multuple figures easier



#additional packages needed: wesanderson, domir

#clean working environment 
rm(list = ls())

#set Open Access working directories
wdOA = setwd("C:/Users/86189/Desktop/R/Experiment1A")

#load dataFrames:
conflicts_prefer(tidylog::rename)
SRdata  = read_delim(sprintf("Exp1A_Data.txt"))
IFdata  = read_csv(sprintf("ImageFeatures/Exp1A_ImageFeatures_148x19.csv"))
IFdata = IFdata %>% rowid_to_column() %>% rename(Image = rowid)

#merge dataframe
data = merge(SRdata,IFdata, by = "Image", all  = T)

#load functions to compute Variance Component Analysis (VCA) via mixed linear modeling (MLM)
source("rcvca.r")
source("vca_exposure.r")

#Prepare data####
#This part follows the Exp1_LinMixedModels.R
#filter out Low Reliability Subjects (Subj#3)
data %>%
  filter(!Subj %in% c(3)) -> data

data %>% 
  arrange(Subj, Block, Image) -> new_table #arrange variable according to Subj, Block and Image

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
conflicts_prefer(dplyr::right_join)
block_table1 %>% 
  rename(AE = Rating, AETime = Time, AETrial = Trial) %>%
  select(Subj, Image, AE, AETime, AETrial, hsv_h:Disorder) %>%
  right_join(block_table2 %>% 
               rename(SR = Rating, SRTime = Time, SRTrial = Trial) %>%
               select(Subj, Image, SR, SRTime, SRTrial)) -> final_table


#recode subject and image as factors
final_table$Subj <- as_factor(final_table$Subj)
final_table$Image <- as_factor(final_table$Image)

block_table1_2%>% 
  rename(AE=Rating,AETime=Time,AETrial=Trial) %>%
  select(Subj,Image,AE,AETime,AETrial:hsv_h:Disorder)%>% 
  right_join(block_table2_2%>% 
               rename(SR=Rating,SRTime=Time,SRTrial=Trial)%>%
               select(Subj,Image,SR,SRTime,SRTrial)) -> tmpOut

block_table3%>%
  rename(AE2=Rating,AE2Time=Time,AE2Trial=Trial) %>%
  select(Subj,Image,AE2,AE2Time,AE2Trial:hsv_h:Disorder)%>% 
  right_join(block_table4%>%
               rename(SR2=Rating,SR2Time=Time,SR2Trial=Trial) %>%
               select(Subj,Image,SR2,SR2Time,SR2Trial))  -> tmpOut2
tmpOut%>% right_join(tmpOut2) -> final_table2

##Image Features####
#LMM for how much SR & IF  explains AE + random intercepts for SR/subj and image
#first check for highly correlated variables (quick way)
cor(final_table %>% select(hsv_h:spatialFreqCircularVariance))>.9 #hsv_v and rgbLuminance 
#Select final IF
names(final_table %>% select(hsv_h:spatialFreqCircularVariance) %>% select(-c(rgbLuminance, memorability))) -> IF
length(IF)
#Scale  IF from 0 to 1 to keep consistent with SR and AA
conflicts_prefer(tidylog::mutate_at)
scale01 <- function(x){(x - min(x)) / (max(x) - min(x))}
final_table <- final_table %>% mutate_at(c(IF), scale01) 
#check variables are properly scaled
conflicts_prefer(tidylog::summarise_all)
final_table %>% select(AE:SR) %>%  summarise_all(range)
#Select final IF
names(final_table %>% select(hsv_h:Disorder) %>% select(-c(rgbLuminance))) -> allIF
length(allIF)

#Spaghetti plot residual AR Supplementary######
#regress allIF from Aesthetic Ratings
conflicts_prefer(tidylog::mutate)
final_table_plot = umx_residualize("AE",c(allIF), data = final_table)
final_table_plot <- final_table_plot %>% mutate(AE = scale01(AE)) 

#plot previous Figure 3.1 + adapted Figure accounting for confounders
##FS1 ab####
ggplot(final_table, aes(x=SR, y=AE, group=Subj))+
  stat_smooth(method="lm", se=FALSE, linewidth=.5, color="lightblue") + # slopes for different subjects
  stat_smooth(aes(group=1), method="lm", color="blue", size=1.5)+  # average slope with SE
  theme_classic(base_size = 12)+
  #labs(title ="Linear Mixed Effect Model AR~SR")+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))+
  labs(x = "Self-relevance Ratings", y = "Aesthetic Ratings")+
  ylim(0,1)|
  ggplot(final_table_plot, aes(x=SR, y=AE, group=Subj))+
  stat_smooth(method="lm", se=FALSE, linewidth=.5, color="lightblue") + # slopes for different subjects
  stat_smooth(aes(group=1), method="lm", color="blue", size=1.5)+  # average slope with SE
  theme_classic(base_size = 12)+
  #labs(title ="Linear Mixed Effect Model AR~SR")+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))+
  labs(x = "Self-relevance Ratings", y = "Residual Aesthetic Ratings")+
  ylim(0,1)

#Fit LMM####
#data for lmm (with exposure)
conflicts_prefer(tidylog::pivot_wider)
SRdata %>%
  filter(!Subj %in% c(3)) -> SRdata
SRdata %>% filter(Question == "1") %>% select(-one_of("Block")) %>% rename(Sub=Subj,Obj=Image,Block=TestRe) -> SRdata.AEonly
#relative contribution to VPC (rcVPC)####
SRdata %>% select(-one_of("Block")) %>% select(-c(Time, Trial)) %>%  pivot_wider(names_from = Question, values_from = Rating)  %>% rename(Sub=Subj,Obj=Image,Block=TestRe,AE = `1`, SR = `2`) -> SRdata_extended
merge(SRdata_extended,IFdata %>% rename(Obj = Image), by = "Obj", all  = T) -> SRIFdata_extended
# Repeat scaling
SRIFdata_extended %>% mutate_at(c(IF), scale01) -> SRIFdata_extended_scaled
#check variables are properly scaled
SRIFdata_extended_scaled %>% select(AE:Disorder) %>%  summarise_all(range)

#define null model for "observational-variance" (drop exposure term)
varparAE.model_exposure = lmer(AE ~  1+ ((1|Sub) + (1 |Obj) + (1|Sub:Obj) + (1|Block) + (1|Block:Sub) + (1|Block:Obj)),data=SRIFdata_extended_scaled)
varparSR.model_exposure = lmer(SR ~  1+ ((1|Sub) + (1 |Obj) + (1|Sub:Obj) + (1|Block) + (1|Block:Sub) + (1|Block:Obj)),data=SRIFdata_extended_scaled)
varparAE.model = lmer(AE ~  1+ ((1|Sub) + (1 |Obj) + (1|Sub:Obj)),data=SRIFdata_extended_scaled)
#sanity check that df are equivalent
lmer(Rating ~ ((1|Sub) + (1 |Obj) + (1|Sub:Obj)),data=SRdata.AEonly)
#Include fixed effects as predictors
varparAE_SR.model = lmer(AE ~ SR  + ((1|Sub) + (1 |Obj) + (1|Sub:Obj)), data=SRdata_extended)
LMM_IF <- paste0("AE ~", paste0(allIF, collapse = " + "), " +((1|Sub) + (1 |Obj) + (1|Sub:Obj))")
LMM_SRIF <- paste0("AE ~ SR + ", paste0(allIF, collapse = " + "), " +((1|Sub) + (1 |Obj) + (1|Sub:Obj))")
varparAE_IF.model =  lmer(LMM_IF, data=SRIFdata_extended_scaled)
varparAE_SRIF.model =  lmer(LMM_SRIF, data=SRIFdata_extended_scaled)

#check variance of the null
conflicts_prefer(tidylog::summarise)
summary(varparAE.model)$varcor %>% as.data.frame() %>% summarise(sum(vcov))
var(SRIFdata_extended_scaled$AE)

#check if variance of the random terms drops
summary(varparAE_SR.model)$varcor %>% as.data.frame() %>% summarise(sum(vcov))
summary(varparAE_IF.model)$varcor %>% as.data.frame() %>% summarise(sum(vcov))
summary(varparAE_SRIF.model)$varcor %>% as.data.frame() %>% summarise(sum(vcov))

##VCA####
#Apply VCA to get an estimate of unique, shared and repeatable variance
vca_null_exposure = VCA_exposure(varparAE.model_exposure,ci = F) %>% mutate(model = "varparAE")
vca_null_SR_exposure = VCA_exposure(varparSR.model_exposure,ci = F) %>% mutate(model = "varparSR")

##rcVCA####
#Apply VCA functions to get cVCP (note that the term exposure and interactions are dropped)
vca_null = rcVCA(varparAE.model) %>% mutate(model = "varparAE")
vca_sr = rcVCA(varparAE_SR.model, null = F, nullmodel = varparAE.model) %>% mutate(model = "varparAE_SR")
vca_if = rcVCA(varparAE_IF.model, null = F, nullmodel = varparAE.model) %>% mutate(model = "varparAE_IF")
vca_srif = rcVCA(varparAE_SRIF.model, null = F, nullmodel = varparAE.model)  %>% mutate(model = "varparAE_SR&IF")

#rename for simplicity
conflicts_prefer(tidylog::inner_join)
colnames(vca_null)[2:4] = paste0(colnames(vca_null)[2:4], "_null")
colnames(vca_sr)[2:4] = paste0(colnames(vca_sr)[2:4], "_sr")
colnames(vca_if)[2:4] = paste0(colnames(vca_if)[2:4], "_if")
colnames(vca_srif)[2:4] = paste0(colnames(vca_srif)[2:4], "_srif")
vca_comparison = list(vca_null,vca_sr,vca_if,vca_srif) %>% reduce(inner_join, by='VPC')

#create a df for plotting
conflicts_prefer(tidylog::pivot_longer)
plot_vca_comparison = 
  vca_comparison %>% 
  mutate(null_m_SR = total_null-total_sr, # amount of random effects covarying with the fixed effect (SR)
         null_m_IF = total_null-total_if, # amount of random effects covarying with the fixed effect (IF)
         null_m_SRIF = total_null-total_srif) %>%   # amount of random effects covarying with the fixed effect (SR + IF)
  mutate(SRIF_m_null_m_IF_SRIF = null_m_SRIF -null_m_SR, # amount of random effects specifically covarying with the fixed effect (SR)
         SRIF_m_null_m_SR_SRIF = null_m_SRIF -null_m_IF) %>% # amount of random effects specifically covarying with the fixed effect (IF)
  mutate(shared_SRIF = null_m_SRIF - (SRIF_m_null_m_SR_SRIF + SRIF_m_null_m_IF_SRIF)) %>% ## amount of random effects covarying with the shared fixed effects (SR + IF)
  select(VPC, total_null,total_sr, null_m_SR, total_if, null_m_IF, total_srif, SRIF_m_null_m_SR_SRIF, SRIF_m_null_m_IF_SRIF, shared_SRIF) %>% 
  rename(null = total_null, SR = total_sr, IF =  total_if, SRIF = total_srif) %>% 
  pivot_longer(names_to = "model", values_to = "value", c(null:shared_SRIF)) %>% 
  mutate(VPC = ifelse(model == "null_m_SR", paste0(VPC,"_SR"), 
                      ifelse(model == "null_m_IF", paste0(VPC,"_IF"),
                             ifelse(model == "SRIF_m_null_m_SR_SRIF",paste0(VPC,"_SR"),
                                    ifelse(model == "SRIF_m_null_m_IF_SRIF",paste0(VPC,"_IF"),
                                           ifelse(model == "shared_SRIF",paste0(VPC,"_SRIF"), VPC))))),
         lmm = ifelse(endsWith(model, '_IF'), "IF", ifelse(endsWith(model, 'SRIF'),"SR+IF",ifelse(endsWith(model, '_SR'), "SR", model))))

#prepare for plotting
level_order = c(
"Residual",
"Stimulus",
"Stimulus_SR",
"Stimulus_SRIF",
"Stimulus_IF",
"Individual",
"Individual_SR",
"Stimulus*Individual",
"Stimulus*Individual_SR"
)
level_lmm = c(
  "null",
  "SR",
  "IF",
  "SR+IF"
)
color_VA = c(viridis::viridis(32), "lightGray")
color_VA_explained = wesanderson::wes_palette("GrandBudapest2")
  c(viridis::viridis(32), "lightGray")

#Rename VPC for ease of plotting
plot_vca_comparison = plot_vca_comparison %>% 
  mutate(VPC = ifelse(VPC == "Individual_IF","Individual_SR",
                ifelse(VPC == "Stimulus*Individual_IF","Stimulus*Individual_SR",VPC)))

conflicts_prefer(tidylog::group_by)
plot_vca_comparison_accounted = plot_vca_comparison %>% 
  filter(lmm  == "SR" | lmm  == "IF" |lmm  == "SR+IF") %>% 
  filter(str_detect(VPC, '_')) %>% group_by(lmm ) %>% 
  summarise(value = sum(value, na.rm = T))

##FS1 c####
plot_vca_comparison %>% 
  mutate(value = round(value,2)) %>% 
  filter(value >0) %>% 
  mutate(VPC = factor(VPC, levels = c(level_order))) %>% 
  mutate(lmm = factor(lmm, levels = c(level_lmm))) %>% 
  ggplot(aes(x = lmm, y = value, fill = VPC, label = round(value,2))) + 
  geom_bar(stat = "identity", color = "black",  width = 0.5)  +
  geom_text(size = 3, position = position_stack(vjust = 0.5), color = "white")+
  scale_fill_manual(values = rev(color_VA[c(1,4,10,14, 21,24,27,30,33)])) +
  theme_classic(base_size = 12)+
  ylim(0,1)+
  geom_hline(yintercept = sum(round(vca_null[1:3,]$total_null,2)), linetype = "dashed")+
  geom_hline(yintercept = sum(round(vca_null[2:3,]$total_null,2)))+
  geom_hline(yintercept = 0)+
  labs(x = "Model",
       y = "Prop. of Aesthetic Ratings' Var.",
       fill = "VPC") +
  theme(legend.spacing.y = unit(.1, 'cm'))  +
  ## important additional element
  guides(fill = guide_legend(byrow = TRUE))

##FS1 d####
plot_vca_comparison_accounted %>% 
  filter(value >0) %>% 
  mutate(lmm = factor(lmm, levels = c(level_lmm))) %>% 
  ggplot(aes(x = lmm, y = value, fill = lmm, label = round(value,2))) + 
  geom_bar(stat = "identity", color = "black",  width = 0.4)  +
  geom_text(size = 3, position = position_stack(vjust = 0.5), color = "white")+
  scale_fill_manual(values =color_VA_explained) +
  theme_classic(base_size = 12)+
  ylim(0,1)+
  geom_hline(yintercept = sum(round(vca_null[1:3,]$total_null,2)), linetype = "dashed")+
  geom_hline(yintercept = 0)+
  labs(x = "Model",
       y = "Prop. of Explained Aesthetic Ratings' Var.",
       fill = "Model") +
    theme(legend.spacing.y = unit(.1, 'cm'))  +
    ## important additional element
    guides(fill = guide_legend(byrow = TRUE))


#CHECK####
FE_final_table = final_table %>% group_by(Image) %>% select(-c(Subj,AETime,AETrial,rgbLuminance)) %>% summarise_all(mean)
##Quality checks
#chek if VPC shared explained match with a simple linear model
summary(lm(AE ~ SR, FE_final_table))
#.50
plot_vca_comparison %>% filter(lmm == "SR") %>% filter(VPC == "Stimulus_SR" ) %>% pull(value) / plot_vca_comparison %>% filter(lmm == "null") %>% filter(VPC  == "Stimulus" ) %>% pull(value)
#.46

LMIF1 <- paste0("AE ~ ", paste0(allIF, collapse = " + "))
summary(lm(LMIF1, FE_final_table))
#.52
plot_vca_comparison %>% filter(lmm == "IF") %>% filter(VPC == "Stimulus_IF" ) %>% pull(value) / plot_vca_comparison %>% filter(lmm == "null") %>% filter(VPC  == "Stimulus" ) %>% pull(value)
#.52

LMSRIF1 <- paste0("AE ~ SR +  ", paste0(allIF, collapse = " + "))
summary(lm(LMSRIF1, FE_final_table))
#.65
sum(plot_vca_comparison %>% filter(lmm == "SR+IF") %>% filter(VPC == "Stimulus_SR" | VPC == "Stimulus_SRIF" | VPC == "Stimulus_IF") %>% pull(value)) / plot_vca_comparison %>% filter(lmm == "null") %>% filter(VPC  == "Stimulus" ) %>% pull(value)
#.66


#Dominance analysis####
#Extend to dominance analysis
#NOTE HERE WE FOCUS ONLY ON FIXED EFFECTS AND AVERAGED AE AND SR ACROSS SUBJECTS PER IMAGE
LMIFall1 <- paste0("AE ~ SR + ", paste0(allIF, collapse = " + "))
LMIF1 <- paste0("AE ~ ", paste0(allIF, collapse = " + "))
LMIF1 <- paste0("SR ~ ", paste0(allIF, collapse = " + "))
#get an idea of how this model perform
summary(lm(AE ~ SR, FE_final_table))
summary(lm(AE ~ SR + hsv_h + hsv_s + hsv_v + fourierSigma + fourierSlope + selfSimilarity + complexity + anisotropy + compressability + dimensionality + memorability + predictability + visualSaliency + rmsContrast + spatialFreqCenteroid + spatialFreqCircularVariance + Naturalness + Disorder, FE_final_table))
#calculate relative importance for each predictor
dominALL =  ### takes a long time! ###
      domir::domin(AE ~ SR + hsv_h + hsv_s + hsv_v + fourierSigma + fourierSlope + selfSimilarity + complexity + anisotropy + compressability + dimensionality + memorability + predictability + visualSaliency + rmsContrast + spatialFreqCenteroid + spatialFreqCircularVariance + Naturalness + Disorder, 
      lm, 
      list(summary, "r.squared"), 
      data = FE_final_table)
#save(dominALL, file = "dominance.RData")
#Scatter plot of AE~SR#
ggplot(FE_final_table, aes(x=SR, y=AE)) +
  geom_point(color="lightGray")+
  stat_smooth(aes(group=1), method="lm", color="blue", size=1.5)+
  theme_classic(base_size = 12)+
  #labs(title ="Linear Mixed Effect Model AR~SR")+
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))+
  labs(x = "Avg. Self-relevance Ratings", y = "Avg. Aesthetic Ratings")
##BAR plot with multiple R2 (note that this is equal to the sum of all the dominance per predictor)
dominALL$Fit_Statistic_Overall %>%
  as.data.frame() %>% 
  rownames_to_column()%>%
  rename(R2 = rowname, fit_statistic = '.') %>% 
  mutate(R2 = "") %>% 
  ggplot(aes(x = R2, y = fit_statistic, fill = R2)) + 
  geom_bar(stat = "identity", color = "black", fill = "#3d5a80",width = 0.5)  +
  ylim(0,1)+
  theme_classic(base_size = 12)+
  labs(x = "",
       y = expression("multiple-R"^"2"),
       fill = "Predictors") 
##BAR plot with overall contribution to fir statistics (R2) for AR~SR+ allIF
dominALL$General_Dominance %>% 
  as.data.frame() %>%
  rownames_to_column()%>%
  rename(predictor = rowname, contribution = '.') %>% 
  arrange(contribution) %>% 
  mutate(x = "") %>% 
  mutate(predictor = factor(predictor, levels = c(predictor))) %>% 
  ggplot(aes(x = x, y = contribution, fill = predictor)) + 
  geom_bar(stat = "identity", color = "black",  width = 0.5)  +
  scale_fill_viridis_d(option = "magma") +
  ylim(0,1)+
  theme_classic(base_size = 12)+
  labs(x = "",
       y = "General dominance",
       fill = "Predictors")+
  theme(legend.position = "bottom")


