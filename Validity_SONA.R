## Validity in SONA Sample Using CFA
# Examples of CFA for validity evidence:
#https://iaap-journals.onlinelibrary.wiley.com/doi/pdf/10.1111/j.1464-0597.2010.00428.x
#http://eduimed.usm.my/EIMJ20110301/EIMJ20110301_06.pdf


## Call needed libraries
library(tidyverse)
library(readxl)
library(psych)
library(ltm)
library(lubridate)
library(lavaan)

#############################
## RDT - SONA 2021 Sample  ##
#############################

# Read in data from original survey
RDT1 <- read_excel("~/Library/CloudStorage/Box-Box/Scrupulosity_SONA_PHI/Data/2ndYearProject/cleanedRDTorig.xlsx") %>%
  dplyr::select(EndDate, id, dup, RDT_01:RDT_30, PIOS_01:PIOS_15, SCF_01:SCF_12) %>% 
  dplyr::select(-c(starts_with("Attention"))) %>% # remove attention questions
  arrange(lubridate::ymd_hms(EndDate)) %>% # order by date 
  mutate(Sample = ifelse(row_number() %in% 1:418, "1", "2")) %>% 
  subset(Sample == "1")


#######################################################
## Fit CFA for Correlations Between Latent Variables ##
#######################################################

## specify the model
valid.model1 <- ' PIOS  =~ PIOS_01 + PIOS_02 + PIOS_03 + PIOS_04 + PIOS_05 + PIOS_06 + PIOS_07 + PIOS_08 + PIOS_09 + 
                          PIOS_10 + PIOS_11 + PIOS_12 + PIOS_13 + PIOS_14 + PIOS_15     
                 RDT_fact1 =~ RDT_01 + RDT_02 + RDT_03 + RDT_07 + RDT_09 + RDT_13 + RDT_19 + RDT_25 + RDT_26 + RDT_27
                 RDT_fact2   =~ RDT_08 + RDT_14 + RDT_15 + RDT_20 + RDT_21
                 SCS =~ SCF_01 + SCF_02 + SCF_03 + SCF_04 + SCF_05 + SCF_06 + SCF_07 + SCF_08 + SCF_09 + 
                        SCF_10 + SCF_11 + SCF_12
'


## fit the model
# oblimin = items are allowed to covary
# ordered = all items are being treated as ordinal
# WLSMV = method that uses probit regression to estimate factor loadings
# std.lv = fixing variance of latent variables to 1 so that factor loadings can be freely estimated
valid_evid1 <- cfa(valid.model1, data = RDT1, rotation = "oblimin", estimator = "WLSMV", ordered = TRUE, std.lv = TRUE)

# display summary output
summary(valid_evid1, fit.measures = TRUE, standardized = TRUE) 
SONA2021 <- standardizedSolution(valid_evid1, type = "std.lv")


#############################
## RDT - SONA 2022 Sample  ##
#############################

# Read in data from original survey
RDT2 <- read_excel("~/Library/CloudStorage/Box-Box/Scrupulosity_SONA_PHI/Data/2ndYearProject/cleanedRDTorig.xlsx") %>%
  dplyr::select(EndDate, id, dup, RDT_01:RDT_30, PIOS_01:PIOS_15, SCF_01:SCF_12) %>% 
  dplyr::select(-c(starts_with("Attention"))) %>% # remove attention questions
  arrange(lubridate::ymd_hms(EndDate)) %>% # order by date 
  mutate(Sample = ifelse(row_number() %in% 1:418, "1", "2")) %>% 
  subset(Sample == "2")


#######################################################
## Fit CFA for Correlations Between Latent Variables ##
#######################################################

## specify the model
valid.model2 <- ' PIOS  =~ PIOS_01 + PIOS_02 + PIOS_03 + PIOS_04 + PIOS_05 + PIOS_06 + PIOS_07 + PIOS_08 + PIOS_09 + 
                          PIOS_10 + PIOS_11 + PIOS_12 + PIOS_13 + PIOS_14 + PIOS_15     
                 RDT_fact1 =~ RDT_01 + RDT_02 + RDT_03 + RDT_07 + RDT_09 + RDT_13 + RDT_19 + RDT_25 + RDT_26 + RDT_27
                 RDT_fact2   =~ RDT_08 + RDT_14 + RDT_15 + RDT_20 + RDT_21
                 SCS =~ SCF_01 + SCF_02 + SCF_03 + SCF_04 + SCF_05 + SCF_06 + SCF_07 + SCF_08 + SCF_09 + 
                        SCF_10 + SCF_11 + SCF_12
'

## fit the model
# oblimin = items are allowed to covary
# ordered = all items are being treated as ordinal
# WLSMV = method that uses probit regression to estimate factor loadings
# std.lv = fixing variance of latent variables to 1 so that factor loadings can be freely estimated
valid_evid2 <- cfa(valid.model2, data = RDT2, rotation = "oblimin", estimator = "WLSMV", ordered = TRUE, std.lv = TRUE)

# display summary output
summary(valid_evid2, fit.measures = TRUE, standardized = TRUE) 
SONA2022 <- standardizedSolution(valid_evid2, type = "std.lv")




