## Reliability in fMRI Sample - Subscales/Total Score

## Call needed libraries
library(tidyverse)
library(readxl)
library(psych)
library(ltm)
library(lubridate)
library(DescTools)
library(mice)

#######################
## RDT - fMRI Sample ##
#######################

# Read in data from qualtrics survey
qual <- read_csv("~/Library/CloudStorage/Box-Box/Scrupulosity_fMRI/Behavioral_Data/data/221118_survey_data.csv")%>% 
  filter(SUBJID != "MW28ZD" & SUBJID != "SE17RN" & SUBJID != "LT72ET") %>%  # They didn't take the phase 2 surveys
  dplyr::select(SUBJID, RDT_1:RDT_30) 
  
# What to do with the missing RDT1-5 for HB56DN? 

# Read in data from in-Scanner administrations (not ready yet)
fMRI1 <- read_excel("~/Library/CloudStorage/Box-Box/Second_Year_Project/Analysis/Data/inScanner_RDT/fMRI_RDT1.xlsx") %>% 
  dplyr::select(SUBJID, phase3, "RDT_1","RDT_2","RDT_3", "RDT_7","RDT_8", "RDT_9","RDT_13","RDT_14",
                "RDT_15", "RDT_19","RDT_20","RDT_21","RDT_25","RDT_26","RDT_27") 
fMRI2 <- read_excel("~/Library/CloudStorage/Box-Box/Second_Year_Project/Analysis/Data/inScanner_RDT/fMRI_RDT2.xlsx") %>% 
  dplyr::select(SUBJID, phase3, "RDT_1","RDT_2","RDT_3", "RDT_7","RDT_8", "RDT_9","RDT_13","RDT_14",
                "RDT_15", "RDT_19","RDT_20","RDT_21","RDT_25","RDT_26","RDT_27") %>% 
  filter(!SUBJID %in% c("QM43RU", "DF57LF", "EM46EU", "AW84RQ", "GX39NL", "HN40PN", "YK56GZ","YG64UC",
                     "AX44QR", "PU53AJ", "NG55ZP","SU56GA"))

diag <- read_excel("/Users/kianabunnell/Library/CloudStorage/Box-Box/Second_Year_Project/Analysis/Data/diagnosis_key.xlsx")

comb <- fMRI1 %>% 
  left_join(fMRI2, by = "SUBJID") %>%
  left_join(diag, by = "SUBJID") %>% 
  drop_na

table(comb$Group)

# Calculate percentage of missingness in each sample
(sum(is.na(qual))/prod(dim(qual)))*100    # 0.158%
(sum(is.na(fMRI1))/prod(dim(fMRI1)))*100  # 1.96%
(sum(is.na(fMRI2))/prod(dim(fMRI2)))*100  # 2.087% 

md.pattern(fMRI2) # helps you see if there is a pattern

# Impute missing data using proportional odds model
# https://towardsdatascience.com/smart-handling-of-missing-data-in-r-6425f8a559f2
## https://cran.r-project.org/web/packages/finalfit/vignettes/missing.html
# The above is nice, because I think it explains how to impute based on group (scrup vs. control)

help <- fMRI2[,3:17] %>% 
  lapply(as.factor) %>% 
  as.data.frame()

fMRI2_impute <- mice(help, m=10, maxit = 50, method = 'polr', seed = 22112) 
# Need to pool the estimated missing values, and then replace the missing values still


####################
## RDT score Qual ##
####################

# Total Score
RDT_columns <- c("RDT_1","RDT_2","RDT_3", "RDT_7","RDT_8", "RDT_9","RDT_13","RDT_14",
                 "RDT_15", "RDT_19","RDT_20","RDT_21","RDT_25","RDT_26","RDT_27")
qual$RDT_Mean_qual <- rowMeans(qual[,RDT_columns])


# Factor 1
RDT_F1 <- c("RDT_1","RDT_2","RDT_3","RDT_7","RDT_9","RDT_13","RDT_19","RDT_25","RDT_26", "RDT_27")
qual$RDT_f1_qual <- rowMeans(qual[,RDT_F1])

# Factor 2
RDT_F2 <- c("RDT_8","RDT_14","RDT_15","RDT_20","RDT_21")
qual$RDT_f2_qual <- rowMeans(qual[,RDT_F2])


##########################
## RDT score in-Scanner ##
##########################

# Total Score
fMRI1$RDT_Mean_fMRI1 <- rowMeans(fMRI1[,RDT_columns])
fMRI2$RDT_Mean_fMRI <- rowMeans(fMRI2[,RDT_columns])

# Factor 1
fMRI1$RDT_f1_fMRI1 <- rowMeans(fMRI1[,RDT_F1])
fMRI2$RDT_f1_fMRI2 <- rowMeans(fMRI2[,RDT_F1])

# Factor 2
fMRI1$RDT_f2_fMRI1 <- rowMeans(fMRI1[,RDT_F2])
fMRI2$RDT_f2_fMRI2 <- rowMeans(fMRI2[,RDT_F2])

######################
## Combine datasets ##
######################

# Only need RDT mean totals 
comb <- qual %>% 
  left_join(fMRI1, by = "SUBJID") %>% 
  left_join(fMRI2, by = c("SUBJID", "phase3")) %>% 
  dplyr::select(SUBJID, starts_with("RDT_Mean"), starts_with("RDT_f1"), starts_with("RDT_f2"))

##### ##### ##### ##### 
##### Reliability ##### 
##### ##### ##### ##### 

# Figure out N for correlations
count <- comb %>% 
  drop_na(RDT_Mean_qual, RDT_Mean_fMRI1) # 91, 92, 84

## Spearman's Rank-Order Correlation for test-retest 
SpearmanRho(comb$RDT_f1_qual, comb$RDT_f1_fMRI1, use = c("complete.obs"), conf.level = 0.95)
SpearmanRho(comb$RDT_f2_qual, comb$RDT_f2_fMRI1, use = c("complete.obs"), conf.level = 0.95)
SpearmanRho(comb$RDT_Mean_qual, comb$RDT_Mean_fMRI1, use = c("complete.obs"), conf.level = 0.95)

## Cronbach's alpha ##
# in-Scanner and qualtrics? How to approach

## QUALTRICS 

# RDT_f1 qual Sample
qual_f1 <- qual %>% 
  dplyr::select(all_of(RDT_F1))
cronbach.alpha(qual_f1, na.rm = TRUE, CI = TRUE)
alpha(qual_f1) 

# RDT_f2 qual Sample
qual_f2 <- qual %>% 
  dplyr::select(all_of(RDT_F2))
cronbach.alpha(qual_f2, na.rm = TRUE, CI = TRUE)
alpha(qual_f2) 


## in-SCANNER RUN 1

# RDT_f1 
fMRI1_f1 <- fMRI1 %>% 
  dplyr::select(all_of(RDT_F1))
cronbach.alpha(fMRI1_f1, na.rm = TRUE, CI = TRUE)
alpha(fMRI1_f1) 

# RDT_f2 
fMRI1_f2 <- fMRI1 %>% 
  dplyr::select(all_of(RDT_F2))
cronbach.alpha(fMRI1_f2, na.rm = TRUE, CI = TRUE)
alpha(fMRI1_f2) 


## in-SCANNER RUN 2

# RDT_f1 
fMRI2_f1 <- fMRI2 %>% 
  dplyr::select(all_of(RDT_F1))
cronbach.alpha(fMRI2_f1, na.rm = TRUE, CI = TRUE)
alpha(fMRI2_f1) 

# RDT_f2 
fMRI2_f2 <- fMRI2 %>% 
  dplyr::select(all_of(RDT_F2))
cronbach.alpha(fMRI2_f2, na.rm = TRUE, CI = TRUE)
alpha(fMRI2_f2) 



