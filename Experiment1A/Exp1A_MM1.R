# Exp1A_MM1.R
# Authors: Edward Vessel, Laura Pasqualette, R. Muralikrishnan, Sarah Koldehoff
# Last modified: 2023-06-16
# Description: Compute across-observer agreement metrics and variance partitioning for Exp. 1A
# Vessel, Pasqualette, Uran, Koldehoff, Bignardi, Vinck (2023). Self-relevance predicts aesthetic appeal for real and synthetic artworks generated via neural style transfer. Psychological Science.

setwd("C:/Users/86189/Desktop/R/Experiment1A") #set directory


library(tidyverse)
library(magrittr)
library(lme4)
library(psych)

SRdata <- read_delim("Exp1A_Data.txt", delim = "\t", col_names = TRUE)

# filter out Low Reliability Subjects (Subj#3)
SRdata %>%
  filter(!Subj %in% c(3)) -> SRdata

#set fixed variables as categorical
SRdata$Subj = as.factor(SRdata$Subj)
SRdata$Block = as.factor(SRdata$Block)
SRdata$Question = as.factor(SRdata$Question)
SRdata$TestRe = as.factor(SRdata$TestRe)
SRdata$Trial = as.factor(SRdata$Trial)
SRdata$Image = as.factor(SRdata$Image)


###############MM1 FOR AESTHETIC APPEAL#########################

#create table with data spread for subj
SRdata %>% filter(Block == "1") %>% select(Subj, Image, Rating)%>%
 arrange(Image) %>%
  spread(Subj, Rating) %>%
  column_to_rownames(var = "Image")-> b1


#first, create a table to input all the correlations per subject
b1_cor <- tibble(Subj = double(), Correlation_Coefficient = double())

#loop to compute correlation of Subject i with the averaged rating values of the other subjects
for (i in 1:length(b1)) {
  select(b1, -i) -> tmp_table #create a temporary table with all subjects - 1
  tmp_table %>%
    mutate(mean_all = rowMeans(.)) ->tmp_mean # create a temporary table that include a column with the average across rating rows
    cor(b1[,i], tmp_mean$mean_all, use = "complete.obs", method = "pearson") -> x #run correlation
    b1_cor %<>% dplyr::add_row(Subj = i, Correlation_Coefficient = x) #compute in new table
    
}

#plot a histogram
b1_cor%>%
  ggplot(aes(x=Correlation_Coefficient))+
  geom_histogram(binwidth = 0.05, color = "deepskyblue3", fill='lightcyan')+
  labs(title ="Aesthetic Appeal (MM1)", x = "MM1 agreement (r)", y = "# Participants")+
  theme_minimal() +theme(plot.title = element_text(hjust = 0.5, face = "bold")) + coord_cartesian(xlim=c(-1,1))


#Convert r to z

rz<- fisherz(b1_cor$Correlation_Coefficient) #convert r to z

#calculate the CI 95%
Lower_limit <-  mean(rz) - (1.96)*(sd(rz))  # (z(mean) -+ (1.96)*(SD))
Upper_limit <-  mean(rz) + (1.96)*(sd(rz)) 
MM1.ae = fisherz2r(mean(rz)) #convert z to r  
MM1.aeCI=fisherz2r(Lower_limit)
MM1.aeCI[2]=fisherz2r(Upper_limit) 

###############MM1 FOR SELF-RELEVANCE#########################

SRdata %>% filter(Block == "2") %>% select(Subj, Image, Rating)%>%
  arrange(Image) %>%
  spread(Subj, Rating) %>%
  column_to_rownames(var = "Image")-> b2


#first, create a table to input all the correlations per subject
b2_cor <- tibble(Subj = double(), Correlation_Coefficient = double())

#loop to compute correlation of Subject i with the averaged rating values of the other subjects
for (i in 1:length(b2)) {
  select(b2, -i) -> tmp_table #create a temporary table with all subjects - 1
  tmp_table %>%
    mutate(mean_all = rowMeans(.)) ->tmp_mean # create a temporary table that include a column with the average across rating rows
  cor(b2[,i], tmp_mean$mean_all, use = "complete.obs", method = "pearson") -> x #run correlation
  b2_cor %<>% dplyr::add_row(Subj = i, Correlation_Coefficient = x) #compute in new table
  
}

#plot a histogram
b2_cor%>%
  ggplot(aes(x=Correlation_Coefficient))+
  geom_histogram(binwidth = 0.05, color = "deepskyblue3", fill='lightpink2')+
  labs(title ="Self-Relevance (MM1)", x = "MM1 Agreement (r)", y = "# Participants")+
  theme_minimal() +theme(plot.title = element_text(hjust = 0.5, face = "bold")) + coord_cartesian(xlim=c(-1,1))

#Convert r to z
rz2<- fisherz(b2_cor$Correlation_Coefficient) #convert r to z
#calculate the CI 95%
Lower_limit <-  mean(rz2) - (1.96)*sd(rz2) # #(z(mean) -+ (1.96)*(SD))
Upper_limit <-  mean(rz2) + (1.96)*sd(rz2) 
MM1.sr=fisherz2r(mean(rz2)) #convert z to r 
MM1.srCI=fisherz2r(Lower_limit) 
MM1.srCI[2]=fisherz2r(Upper_limit) 


## Variance partitioning

#split data by question
SRdata %>% filter(Question == "1") %>% select(-one_of("Block")) %>% rename(Sub=Subj,Obj=Image,Block=TestRe) -> SRdata.AEonly
SRdata %>% filter(Question == "2") %>% select(-one_of("Block"))%>%rename(Sub=Subj,Obj=Image,Block=TestRe) -> SRdata.SRonly

source("vca_exposure.r")

varparAE.model = lmer(Rating ~ 1 + ((1|Sub) + (1 |Obj) + (1|Sub:Obj) + (1|Block) + (1|Block:Sub) + (1|Block:Obj)),data=SRdata.AEonly)
VCA.AE = VCA_exposure(varparAE.model,ci=F)

Vind.AE = (VCA.AE[2,3] + VCA.AE[4,3] + VCA.AE[5,3])#EV fix
Vshar.AE = 1 - Vind.AE
Vrepeat.AE = 1 - VCA.AE[7,2]

varparSR.model = lmer(Rating ~ 1 + ((1|Sub) + (1 |Obj) + (1|Sub:Obj) + (1|Block) + (1|Block:Sub) + (1|Block:Obj)),data=SRdata.SRonly)
VCA.SR = VCA_exposure(varparSR.model,ci=F)

Vind.SR = (VCA.SR[2,3] + VCA.SR[4,3] + VCA.SR[5,3]) #EV fix
Vshar.SR = 1 - Vind.SR
Vrepeat.SR = 1 - VCA.SR[7,2]

