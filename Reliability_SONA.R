## Reliability in SONA Sample - Subscales/Total Score

## Call needed libraries
library(tidyverse)
library(readxl)
library(psych)
library(ltm)
library(lubridate)
library(DescTools)

################################
## RDT - SONA Sample - Retest ##
################################

# Read in data from original survey
orig <- read_excel("~/Library/CloudStorage/Box-Box/Scrupulosity_SONA_PHI/Data/2ndYearProject/cleanedRDTorig.xlsx") %>%
  dplyr::select(EndDate, id, dup, RDT_01:RDT_30, PIOS_01:PIOS_15, SCF_01:SCF_12) %>% 
  dplyr::select(-c(starts_with("Attention"))) %>% # remove attention questions
  arrange(lubridate::ymd_hms(EndDate)) %>% # order by date 
  mutate(Sample = ifelse(row_number() %in% 1:418, "1", "2")) # all survey from 2021 = sample 1, 2022 = sample 2

# Read in data from follow-up survey
rep <- read_excel("~/Library/CloudStorage/Box-Box/Scrupulosity_SONA_PHI/Data/2ndYearProject/cleanedRDTrep.xlsx") %>% 
  dplyr::select(EndDate, id, starts_with("RDT")) %>% 
  slice(-c(1,2)) %>% 
  arrange(lubridate::ymd_hms(EndDate)) %>% # order by date
  mutate(Sample = ifelse(row_number() %in% 1:244, "1", "2")) # all survey from 2021 = sample 1, 2022 = sample 2


####################
## RDT score orig ##
####################

# Total Score
RDT_columns <- c("RDT_01","RDT_02","RDT_03", "RDT_07","RDT_08", "RDT_09","RDT_13","RDT_14",
                 "RDT_15", "RDT_19","RDT_20","RDT_21","RDT_25","RDT_26","RDT_27")
orig[,RDT_columns] <- lapply(orig[,RDT_columns], as.numeric)
orig$RDT_Mean <- rowMeans(orig[,RDT_columns])

# Factor 1
RDT_F1 <- c("RDT_01","RDT_02","RDT_03","RDT_07","RDT_09","RDT_13","RDT_19","RDT_25","RDT_26", "RDT_27")
orig$RDT_f1 <- rowMeans(orig[,RDT_F1])

# Factor 2
RDT_F2 <- c("RDT_08","RDT_14","RDT_15","RDT_20","RDT_21")
orig$RDT_f2 <- rowMeans(orig[,RDT_F2])


####################
## RDT score rep ##
####################

# Total Score
rep[,RDT_columns] <- lapply(rep[,RDT_columns], as.numeric)
rep$RDT_Mean <- rowMeans(rep[,RDT_columns])

# Factor 1
rep$RDT_f1 <- rowMeans(rep[,RDT_F1])

# Factor 2
rep$RDT_f2 <- rowMeans(rep[,RDT_F2])


######################
## Combine datasets ##
######################

## Dropping duplicates also gets rid of the skeleton entries (e.g., no entered data)
# Only need RDT mean totals 
eh <- orig %>% 
  subset(dup == 0) %>% 
  dplyr::select(id, dup, Sample, RDT_Mean, RDT_f1, RDT_f2) %>% 
  left_join(rep, by = c("id" = "id", "Sample" = "Sample"))
  

## separate by sample 
Sample1 <- eh %>% 
  subset(Sample == 1) %>% 
  drop_na()

Sample2 <- eh %>% 
  subset(Sample == 2) %>% 
  drop_na()

##### ##### ##### ##### 
##### Reliability ##### 
##### ##### ##### ##### 

## Spearman's Rank-Order Correlation for test-retest 
SpearmanRho(Sample1$RDT_f1.x, Sample1$RDT_f1.y, use = c("complete.obs"), conf.level = 0.95)
SpearmanRho(Sample1$RDT_f2.x, Sample1$RDT_f2.y, use = c("complete.obs"), conf.level = 0.95)
SpearmanRho(Sample1$RDT_Mean.x, Sample1$RDT_Mean.y, use = c("complete.obs"), conf.level = 0.95)

SpearmanRho(Sample2$RDT_f1.x, Sample2$RDT_f1.y, use = c("complete.obs"), conf.level = 0.95)
SpearmanRho(Sample2$RDT_f2.x, Sample2$RDT_f2.y, use = c("complete.obs"), conf.level = 0.95)
SpearmanRho(Sample2$RDT_Mean.x, Sample2$RDT_Mean.y, use = c("complete.obs"), conf.level = 0.95)


## Cronbach's alpha ##

# RDT_f1 2021 Sample
rdt_2021_f1 <- orig %>% 
  subset(Sample == 1) %>% 
  dplyr::select(all_of(RDT_F1))
cronbach.alpha(rdt_2021_f1, na.rm = TRUE, CI = TRUE)
alpha(rdt_2021_f1) 

# RDT_f2 2021 Sample
rdt_2021_f2 <- orig %>% 
  subset(Sample == 1) %>% 
  dplyr::select(all_of(RDT_F2))
cronbach.alpha(rdt_2021_f2, na.rm = TRUE, CI = TRUE)
alpha(rdt_2021_f2) 

# RDT_f1 2022 Sample
rdt_2022_f1 <- orig %>% 
  subset(Sample == 2) %>% 
  dplyr::select(all_of(RDT_F1))
cronbach.alpha(rdt_2022_f1, na.rm = TRUE, CI = TRUE) 
alpha(rdt_2022_f1) 

# RDT_f2 2021 Sample
rdt_2022_f2 <- orig %>% 
  subset(Sample == 2) %>% 
  dplyr::select(all_of(RDT_F2))
cronbach.alpha(rdt_2022_f2, na.rm = TRUE, CI = TRUE)
alpha(rdt_2022_f2) 



