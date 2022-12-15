## Description ## 
# Using dataframe from RDT_SONAreplication.R
# Analysis: Confirmatory factor analysis
# Reference https://lavaan.ugent.be/tutorial/cfa.html

# Libraries
library(tidyverse)
library(lavaan)
library(semPlot)

##################
## Read in Data ##
##################
# From SONA 2022 Sample
RDT2 <- read_excel("~/Library/CloudStorage/Box-Box/Scrupulosity_SONA_PHI/Data/2ndYearProject/cleanedRDTorig.xlsx") %>%
  dplyr::select(-c(starts_with("Attention"))) %>% # remove attention questions
  arrange(lubridate::ymd_hms(EndDate)) %>% # order by date 
  mutate(Sample = ifelse(row_number() %in% 1:418, "1", "2")) %>% # all survey from 2021 = sample 1, 2022 = sample 2
  subset(Sample == "2") %>% 
  dplyr::select(RDT_01:RDT_03, RDT_07:RDT_09, RDT_13:RDT_15, RDT_19:RDT_21, RDT_25:RDT_27) %>% 
  drop_na() # No missing responses to individual items, only empty responses


###########################
## Define 2 Factor Model ##
###########################
twofac.m <- ' RDT_fact1 =~ RDT_01 + RDT_02 + RDT_03 + RDT_07 + RDT_09 + RDT_13 + RDT_19 + RDT_25 + RDT_26 + RDT_27
              RDT_fact2   =~ RDT_08 + RDT_14 + RDT_15 + RDT_20 + RDT_21
'

#############
## Fit CFA ##
#############
# oblimin = items are allowed to covary
# ordered = all items are being treated as ordinal
# WLSMV = method that uses probit regression to estimate factor loadings
# std.lv = fixing variance of latent variables to 1 so that factor loadings can be freely estimated
cfa.results <- cfa(twofac.m, data = RDT2, rotation = "oblimin", estimator = "WLSMV", ordered = TRUE, std.lv = TRUE)

# display summary output
summary(cfa.results, fit.measures = TRUE, standardized = TRUE) 
cfa <- standardizedSolution(cfa.results, type = "std.lv")


################### 
## Create Figure ##
###################

semPlot::semPaths(cfa.results, "std", rotation = 2) ## nah it's ugly

######################################
## Compare Fit to 1-Factor Model??? ##
######################################

onefac.m <- ' RDT =~ RDT_01 + RDT_02 + RDT_03 + RDT_07 + RDT_08 + RDT_09 + RDT_13 + RDT_14 + RDT_15 + RDT_19 + 
                     RDT_20 + RDT_21 + RDT_25 + RDT_26 + RDT_27
'

alt.cfa.results <- cfa(onefac.m, data = RDT2, rotation = "oblimin", estimator = "WLSMV", ordered = TRUE, std.lv = TRUE)
summary(alt.cfa.results, fit.measures = TRUE, standardized = TRUE) 


