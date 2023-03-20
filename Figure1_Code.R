library(ggplot2)
library(tidyverse)
library(dplyr)
library(ggpubr)

setwd("C:/Users/Ziming Chen/OneDrive - SickKids/DRIVE/Simulation/")
results1 <- read.csv("OR1_200_results.csv")
results2 <- read.csv("OR1_350_results.csv")
results3 <- read.csv("OR1_500_results.csv")
results4 <- read.csv("OR1_800_results.csv")
results5 <- read.csv("OR13_200_results.csv")
results6 <- read.csv("OR13_350_results.csv")
results7 <- read.csv("OR13_500_results.csv")
results8 <- read.csv("OR13_800_results.csv")

results_OR1 <- rbind(results1, results2, results3, results4)
results_OR1$idx <- seq.int(nrow(results_OR1))
results_OR13 <- rbind(results5, results6, results7, results8)
results_OR13$idx <- seq.int(nrow(results_OR13))
results_final <- rbind(results1, results2, results3, results4, results5, results6, results7)
results_final$id <- seq.int(nrow(results_final))

#filter out the ones with type one error greater than 0.05
results_OR1_LSE <- results_OR1 %>%
  filter(state == "LSE")  

results_OR1_LSE$id <-  seq.int(nrow(results_OR1_LSE))

results_OR1_LSE_sub <- results_OR1_LSE %>%
  filter(p_sup <= 0.05)

results_OR1_HSE <- results_OR1 %>%
  filter(state == "HSE")

results_OR1_HSE$id <-  seq.int(nrow(results_OR1_HSE))

results_OR1_HSE_sub <- results_OR1_HSE %>%
  filter(id %in% results_OR1_LSE_sub$id & p_sup <= 0.05)

results_OR1_final <- rbind(results_OR1_LSE_sub, results_OR1_HSE_sub)

results_OR13_LSE <- results_OR13 %>%
  filter(state == "LSE")  

results_OR13_LSE$id <-  seq.int(nrow(results_OR13_LSE))

results_OR13_LSE_sub <- results_OR13_LSE %>%
  filter(id %in% results_OR1_LSE_sub$id)

results_OR13_HSE <- results_OR13 %>%
  filter(state == "HSE")

results_OR13_HSE$id <-  seq.int(nrow(results_OR13_HSE))

results_OR13_HSE_sub <- results_OR13_HSE %>%
  filter(id %in% results_OR1_HSE_sub$id)

results_OR13_final <- rbind(results_OR13_HSE_sub, results_OR13_LSE_sub)

state.labs <- c("High Elastance", "Low Elastance")
names(state.labs) <- c("HSE", "LSE")

#visualize the results for different designs in terms of mean sample size and power for different states. Note that only designs that have a type one error
#less or equal to 0.05 are included in this plot (resulted from the data manipulation from above)
results_OR13_final %>%
  mutate(sup_thresh = factor(sup_thresh),
         futi_thresh = factor(futi_thresh),
         burn_in = factor(burn_in)) %>%
  ggplot(mapping = aes(x = ESS, 
                       y = p_sup, 
                       col = sup_thresh, 
                       shape = futi_thresh, 
                       size = burn_in)) + 
  theme(panel.grid.minor = element_line(colour="lightgray", size=0.5)) + 
  labs(x = "Expected Sample Size", y = "Power at Odds Ratio of 1.3",
       col = "Superiority Prob",
       shape = "Futility Prob",
       size = "Initial Recruitment") +
  geom_line() + 
  geom_point() + 
  scale_colour_grey() +
  facet_wrap(~ state,
             labeller = labeller(state = state.labs))+
  theme_bw()


results_OR13_final_design1 <- results_OR13_final %>%
  filter(burn_in == 500 & sup_thresh == 0.9925 & futi_thresh == 0.95)
results_OR13_final_design2 <- results_OR13_final %>%
  filter(burn_in == 200 & sup_thresh == 0.995 & futi_thresh == 0.95)
results_OR1_final_design1 <- results_OR1_final %>%
  filter(burn_in == 500 & sup_thresh == 0.9925 & futi_thresh == 0.95)
results_OR1_final_design2 <- results_OR1_final %>%
  filter(burn_in == 200 & sup_thresh == 0.995 & futi_thresh == 0.95)


#You can use this to generate results used in Table 3
rbind(results_OR13_final_design1, results_OR1_final_design1)

rbind(results_OR13_final_design2, results_OR1_final_design2)
