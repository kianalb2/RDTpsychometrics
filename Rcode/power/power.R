####################
## Power Analysis ##
####################

## Call libraries
library(pwr)
library(tidyverse)

#####################################################
## Effect Size Calculations from Literature Review ##
#####################################################
## Harrison 2012 vmPFC > OFC activation
# From this paper: https://jamanetwork.com/journals/jamapsychiatry/fullarticle/1211981
# z-score: 3.86, cluster extent: 870, sample size per group = 73
3.86/sqrt(73) # 0.45

############################
## Power/Sensitivity Test ##
############################

## Two sample t-test ##
pwr.t.test(n=40, power=0.8) # specific effect size 
pwr.t.test(n=40, d=.45) # specific effect size 

##########################################
## Create power curve figure for t-test ##
##########################################

library(pwr)

# Set parameters
sample_size <- 40
effect_sizes <- seq(0.1, 1.0, by = 0.1)  # Different effect sizes to consider

# Calculate power for different effect sizes
power_values <- sapply(effect_sizes, function(es) {
  pwr.t.test(d = es, n = sample_size, sig.level = 0.05, type = "two.sample")$power
})

# Create a data frame for plotting
data <- data.frame(effect_sizes, power_values)

# Create the plot
ggplot(data, aes(x = effect_sizes, y = power_values)) +
  geom_line() +
  geom_point() +
  geom_hline(yintercept = .8,lty=2, color='blue') +
  labs(
    title = paste("Power Analysis for Sample Size", sample_size),
    x = "Effect Size",
    y = "Power"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(family = "Times New Roman", size = 16, face = "bold"),
    axis.title = element_text(family = "Times New Roman", size = 12),
    axis.text = element_text(family = "Times New Roman", size = 12)
  )

# Export figure
ggsave(filename = paste("t-test_pwr.png"), width = 6, height = 5,
       path = "~/Library/CloudStorage/Box-Box/KianaDissertation/Prospectus/Analysis", units = c("in"), dpi = 300)


