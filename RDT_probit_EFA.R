# https://solomonkurz.netlify.app/post/2021-05-11-yes-you-can-fit-an-exploratory-factor-analysis-with-lavaan/

# Libraries
library(tidyverse)
library(lavaan)

##################
## Read in data ##
##################

RDT1 <- read_excel("~/Library/CloudStorage/Box-Box/Scrupulosity_SONA_PHI/Data/2ndYearProject/cleanedRDTorig.xlsx") %>%
  dplyr::select(-c(starts_with("Attention"))) %>% # remove attention questions
  arrange(lubridate::ymd_hms(EndDate)) %>% # order by date 
  mutate(Sample = ifelse(row_number() %in% 1:418, "1", "2")) %>% # all survey from 2021 = sample 1, 2022 = sample 2
  subset(Sample == "1") %>% 
  dplyr::select(RDT_01:RDT_03, RDT_07:RDT_09, RDT_13:RDT_15, RDT_19:RDT_21, RDT_25:RDT_27) %>% 
  drop_na() # No missing responses to individual items, only empty responses
  


#################################
## Determine Number of Factors ##
#################################

## Screeplot
ev <- eigen(cor(RDT1), symmetric = TRUE) # eigenvalues greater than one = 2
ev$values

scree(RDT1) # 1 factor, 2 components

fafitfree <- fa(RDT1,nfactors = ncol(RDT1), rotate = "none") # 1 factor
n_factors <- length(fafitfree$e.values)
scree     <- data.frame(
  Factor_n =  as.factor(1:n_factors), 
  Eigenvalue = fafitfree$e.values)
ggplot(scree, aes(x = Factor_n, y = Eigenvalue, group = 1)) + 
  geom_point() + geom_line() +
  theme_bw()+ theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())+
  theme(text = element_text(size=12,  family="Times New Roman")) +
  scale_y_continuous(breaks = c(1, 2, 3, 4, 5, 6, 7), limits = c(0,7)) +
  xlab("Number of Factors") +
  ylab("Eigenvalue") +
  labs( title = "Scree Plot") + 
  geom_hline(yintercept = 1, col = "#FAC898")

ggsave(filename = paste("EFA_scree.png"), width = 6, height = 5,
       path = "/Users/kianabunnell/Library/CloudStorage/Box-Box/Second_Year_Project/Analysis/Figures", 
       units = c("in"), dpi = 300)

## Parallel analysis
fa.parallel(RDT1, fa="fa", n.obs = 392) # 4 factors
fa.parallel(RDT1)

## Chong Ming recommends a 2-factor solution

###################
## Define Models ##
###################

# 1-factor model
f1 <- '
efa("efa")*f1 =~ RDT_01 + RDT_02 + RDT_03 + RDT_07 + RDT_08 + RDT_09 + RDT_13 + RDT_14 + RDT_15 +
                 RDT_19 + RDT_20 + RDT_21 + RDT_25 + RDT_26 + RDT_27
'

# 2-factor model
f2 <- '
efa("efa")*f1 +
efa("efa")*f2 =~ RDT_01 + RDT_02 + RDT_03 + RDT_07 + RDT_08 + RDT_09 + RDT_13 + RDT_14 + RDT_15 +
                 RDT_19 + RDT_20 + RDT_21 + RDT_25 + RDT_26 + RDT_27
'

#################
## Perform EFA ##
#################

efa_f1 <- 
  cfa(model = f1,
      data = RDT1,
      rotation = "oblimin",
      estimator = "WLSMV",
      ordered = TRUE)

efa_f2 <- 
  cfa(model = f2,
      data = RDT1,
      rotation = "oblimin",
      estimator = "WLSMV",
      ordered = TRUE)

summary(efa_f1, fit.measures = TRUE, standardized = TRUE)
summary(efa_f2, fit.measures = TRUE, standardized = TRUE)


#################
## Compare Fit ##
#################

# define the fit measures
fit_measures_robust <- c("chisq.scaled", "df", "pvalue.scaled", 
                         "cfi.scaled", "rmsea.scaled", "srmr")

# collect them for each model
rbind(fitmeasures(efa_f1, fit_measures_robust),
      fitmeasures(efa_f2, fit_measures_robust),
      fitmeasures(efa_f4, fit_measures_robust)) %>% 
  data.frame() %>% 
  mutate(chisq.scaled  = round(chisq.scaled, digits = 2),
         df            = as.integer(df),
         pvalue.scaled = ifelse(pvalue.scaled == 0, "< .001", pvalue.scaled)) %>% 
  mutate_at(vars(cfi.scaled:srmr), ~round(., digits =  3))
  
