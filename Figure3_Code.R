setwd("C:/Users/Ziming Chen/OneDrive - SickKids/DRIVE/Simulation/Cross Scenarios")
library(patternplot)
library(tidyverse)
library(dplyr)
library(colorspace)

L08H08<- read.csv("L08H08.csv")
################ LSE ######################
L08H08_LSE <- L08H08[, 1:2]

#simulation index
id <- NULL
#sample size
SS <- NULL
#treatment
TxARM <- NULL
#trial result
Result <- NULL
for(i in 1: 1000){
  id[i] <- i 
  SS[i] <- L08H08_LSE[3 * (i - 1) + 1, 2]
  TxARM[i] <- L08H08_LSE[3 * (i - 1) + 2,2]
  Result[i] <- L08H08_LSE[3 * (i - 1) + 3,2]
}
L08H08_LSE_df <- data.frame(
  id = id,
  SS = as.numeric(SS),
  TxARM = TxARM,
  Result = Result
)

L08H08_LSE_df <- L08H08_LSE_df %>%
  filter(!is.na(Result))
idx <- L08H08_LSE_df$TxARM == "UC" & L08H08_LSE_df$Result == "superiority"
L08H08_LSE_df$TxARM[idx] <- "DPL"
L08H08_LSE_df$Result[idx] <- "futility"


thresh <- ifelse(length(L08H08_LSE_df$SS[L08H08_LSE_df$SS > 2000]) != 0, min((L08H08_LSE_df$SS[L08H08_LSE_df$SS > 2000])), 2000)
idx2 <- as.numeric(L08H08_LSE_df$SS) > thresh
L08H08_LSE_df$Result[idx2]  <- "Nothing"; L08H08_LSE_df$SS[idx2] <- thresh


L08H08_LSE_df <- L08H08_LSE_df %>%
  mutate(OR = 0.8,
         state = "Low Elastance") %>%
  arrange(as.numeric(SS)) %>%
  mutate(Futility = cumsum(Result == "futility") / n() + runif(n(), 0.000,0.0001),
         Superiority = cumsum(Result == "superiority") / n() + runif(n(), 0.000,0.0001),
         Nothing = 1 - Futility - Superiority)
################ LSE ######################
L08H08_HSE <- L08H08[, c(1,3)]

#simulation index
id <- NULL
#sample size
SS <- NULL
#treatment
TxARM <- NULL
#trial result
Result <- NULL
for(i in 1: 1000){
  id[i] <- i 
  SS[i] <- L08H08_HSE[3 * (i - 1) + 1, 2]
  TxARM[i] <- L08H08_HSE[3 * (i - 1) + 2,2]
  Result[i] <- L08H08_HSE[3 * (i - 1) + 3,2]
}
L08H08_HSE_df <- data.frame(
  id = id,
  SS = as.numeric(SS),
  TxARM = TxARM,
  Result = Result
)
L08H08_HSE_df <- L08H08_HSE_df %>%
  filter(!is.na(Result))
idx <- L08H08_HSE_df$TxARM == "UC" & L08H08_HSE_df$Result == "superiority"
L08H08_HSE_df$TxARM[idx] <- "DPL"
L08H08_HSE_df$Result[idx] <- "futility"

thresh <- ifelse(length(L08H08_HSE_df$SS[L08H08_HSE_df$SS > 2000]) != 0, min((L08H08_HSE_df$SS[L08H08_HSE_df$SS > 2000])), 2000)
idx2 <- as.numeric(L08H08_HSE_df$SS) > thresh
L08H08_HSE_df$Result[idx2]  <- "Nothing"; L08H08_HSE_df$SS[idx2] <- thresh


L08H08_HSE_df <- L08H08_HSE_df %>%
  mutate(OR = 0.8,
         state = "High Elastance") %>%
  arrange(as.numeric(SS)) %>%
  mutate(Futility = cumsum(Result == "futility") / n() + runif(n(), 0.000,0.0001),
         Superiority = cumsum(Result == "superiority") / n() + runif(n(), 0.000,0.0001),
         Nothing = 1 - Futility - Superiority)
L1H1 <- read.csv("C:/Users/Ziming Chen/OneDrive - SickKids/DRIVE/Simulation/OR1/n500/sup0.9925/OR1_n500_sup9925_fut95.csv")
################ LSE ######################
L1H1_LSE <- L1H1[, 1:2]

#simulation index
id <- NULL
#sample size
SS <- NULL
#treatment
TxARM <- NULL
#trial result
Result <- NULL
for(i in 1: 1000){
  id[i] <- i 
  SS[i] <- L1H1_LSE[3 * (i - 1) + 1, 2]
  TxARM[i] <- L1H1_LSE[3 * (i - 1) + 2,2]
  Result[i] <- L1H1_LSE[3 * (i - 1) + 3,2]
}
L1H1_LSE_df <- data.frame(
  id = id,
  SS = as.numeric(SS),
  TxARM = TxARM,
  Result = Result
)
L1H1_LSE_df <- L1H1_LSE_df %>%
  filter(!is.na(Result))
idx <- L1H1_LSE_df$TxARM == "UC" & L1H1_LSE_df$Result == "superiority"
L1H1_LSE_df$TxARM[idx] <- "DPL"
L1H1_LSE_df$Result[idx] <- "futility"

thresh <- ifelse(length(L1H1_LSE_df$SS[L1H1_LSE_df$SS > 2000]) != 0, min((L1H1_LSE_df$SS[L1H1_LSE_df$SS > 2000])), 2000)
idx2 <- as.numeric(L1H1_LSE_df$SS) > thresh
L1H1_LSE_df$Result[idx2]  <- "Nothing"; L1H1_LSE_df$SS[idx2] <- thresh


L1H1_LSE_df <- L1H1_LSE_df %>%
  mutate(OR = 1,
         state = "Low Elastance") %>%
  arrange(as.numeric(SS)) %>%
  mutate(Futility = cumsum(Result == "futility") / n() + runif(n(), 0.000,0.0001),
         Superiority = cumsum(Result == "superiority") / n() + runif(n(), 0.000,0.0001),
         Nothing = 1 - Futility - Superiority)
################ HSE ######################
L1H1_HSE <- L1H1[, c(1,3)]

#simulation index
id <- NULL
#sample size
SS <- NULL
#treatment
TxARM <- NULL
#trial result
Result <- NULL
for(i in 1: 1000){
  id[i] <- i 
  SS[i] <- L1H1_HSE[3 * (i - 1) + 1, 2]
  TxARM[i] <- L1H1_HSE[3 * (i - 1) + 2,2]
  Result[i] <- L1H1_HSE[3 * (i - 1) + 3,2]
}
L1H1_HSE_df <- data.frame(
  id = id,
  SS = as.numeric(SS),
  TxARM = TxARM,
  Result = Result
)
L1H1_HSE_df <- L1H1_HSE_df %>%
  filter(!is.na(Result))
idx <- L1H1_HSE_df$TxARM == "UC" & L1H1_HSE_df$Result == "superiority"
L1H1_HSE_df$TxARM[idx] <- "DPL"
L1H1_HSE_df$Result[idx] <- "futility"

thresh <- ifelse(length(L1H1_HSE_df$SS[L1H1_HSE_df$SS > 2000]) != 0, min((L1H1_HSE_df$SS[L1H1_HSE_df$SS > 2000])), 2000)
idx2 <- as.numeric(L1H1_HSE_df$SS) > thresh
L1H1_HSE_df$Result[idx2]  <- "Nothing"; L1H1_HSE_df$SS[idx2] <- thresh


L1H1_HSE_df <- L1H1_HSE_df %>%
  mutate(OR = 1,
         state = "High Elastance") %>%
  arrange(as.numeric(SS)) %>%
  mutate(Futility = cumsum(Result == "futility") / n() + runif(n(), 0.000,0.0001),
         Superiority = cumsum(Result == "superiority") / n() + runif(n(), 0.000,0.0001),
         Nothing = 1 - Futility - Superiority)

#####################################################################################
L11H11 <- read.csv("L11H11.csv")
################ LSE ######################
L11H11_LSE <- L11H11[, 1:2]

#simulation index
id <- NULL
#sample size
SS <- NULL
#treatment
TxARM <- NULL
#trial result
Result <- NULL
for(i in 1: 1000){
  id[i] <- i 
  SS[i] <- L11H11_LSE[3 * (i - 1) + 1, 2]
  TxARM[i] <- L11H11_LSE[3 * (i - 1) + 2,2]
  Result[i] <- L11H11_LSE[3 * (i - 1) + 3,2]
}
L11H11_LSE_df <- data.frame(
  id = id,
  SS = as.numeric(SS),
  TxARM = TxARM,
  Result = Result
)
L11H11_LSE_df <- L11H11_LSE_df %>%
  filter(!is.na(Result))
idx <- L11H11_LSE_df$TxARM == "UC" & L11H11_LSE_df$Result == "superiority"
L11H11_LSE_df$TxARM[idx] <- "DPL"
L11H11_LSE_df$Result[idx] <- "futility"

thresh <- ifelse(length(L11H11_LSE_df$SS[L11H11_LSE_df$SS > 2000]) != 0, min((L11H11_LSE_df$SS[L11H11_LSE_df$SS > 2000])), 2000)
idx2 <- as.numeric(L11H11_LSE_df$SS) > thresh
L11H11_LSE_df$Result[idx2]  <- "Nothing"; L11H11_LSE_df$SS[idx2] <- thresh


L11H11_LSE_df <- L11H11_LSE_df %>%
  mutate(OR = 1.1,
         state = "Low Elastance") %>%
  arrange(as.numeric(SS)) %>%
  mutate(Futility = cumsum(Result == "futility") / n() + runif(n(), 0.000,0.0001),
         Superiority = cumsum(Result == "superiority") / n() + runif(n(), 0.000,0.0001),
         Nothing = 1 - Futility - Superiority)
################ HSE ######################
L11H11_HSE <- L11H11[, c(1,3)]

#simulation index
id <- NULL
#sample size
SS <- NULL
#treatment
TxARM <- NULL
#trial result
Result <- NULL
for(i in 1: 1000){
  id[i] <- i 
  SS[i] <- L11H11_HSE[3 * (i - 1) + 1, 2]
  TxARM[i] <- L11H11_HSE[3 * (i - 1) + 2,2]
  Result[i] <- L11H11_HSE[3 * (i - 1) + 3,2]
}
L11H11_HSE_df <- data.frame(
  id = id,
  SS = as.numeric(SS),
  TxARM = TxARM,
  Result = Result
)
L11H11_HSE_df <- L11H11_HSE_df %>%
  filter(!is.na(Result))
idx <- L11H11_HSE_df$TxARM == "UC" & L11H11_HSE_df$Result == "superiority"
L11H11_HSE_df$TxARM[idx] <- "DPL"
L11H11_HSE_df$Result[idx] <- "futility"

thresh <- ifelse(length(L11H11_HSE_df$SS[L11H11_HSE_df$SS > 2000]) != 0, min((L11H11_HSE_df$SS[L11H11_HSE_df$SS > 2000])), 2000)
idx2 <- as.numeric(L11H11_HSE_df$SS) > thresh
L11H11_HSE_df$Result[idx2]  <- "Nothing"; L11H11_HSE_df$SS[idx2] <- thresh


L11H11_HSE_df <- L11H11_HSE_df %>%
  mutate(OR = 1.1,
         state = "High Elastance") %>%
  arrange(as.numeric(SS)) %>%
  mutate(Futility = cumsum(Result == "futility") / n() + runif(n(), 0.000,0.0001),
         Superiority = cumsum(Result == "superiority") / n() + runif(n(), 0.000,0.0001),
         Nothing = 1 - Futility - Superiority)
#####################################################################################
L12H12 <- read.csv("L12H12.csv")
################ LSE ######################
L12H12_LSE <- L12H12[, 1:2]

#simulation index
id <- NULL
#sample size
SS <- NULL
#treatment
TxARM <- NULL
#trial result
Result <- NULL
for(i in 1: 1000){
  id[i] <- i 
  SS[i] <- L12H12_LSE[3 * (i - 1) + 1, 2]
  TxARM[i] <- L12H12_LSE[3 * (i - 1) + 2,2]
  Result[i] <- L12H12_LSE[3 * (i - 1) + 3,2]
}
L12H12_LSE_df <- data.frame(
  id = id,
  SS = as.numeric(SS),
  TxARM = TxARM,
  Result = Result
)
L12H12_LSE_df <- L12H12_LSE_df %>%
  filter(!is.na(Result))
idx <- L12H12_LSE_df$TxARM == "UC" & L12H12_LSE_df$Result == "superiority"
L12H12_LSE_df$TxARM[idx] <- "DPL"
L12H12_LSE_df$Result[idx] <- "futility"


thresh <- ifelse(length(L12H12_LSE_df$SS[L12H12_LSE_df$SS > 2000]) != 0, min((L12H12_LSE_df$SS[L12H12_LSE_df$SS > 2000])), 2000)
idx2 <- as.numeric(L12H12_LSE_df$SS) > thresh
L12H12_LSE_df$Result[idx2]  <- "Nothing"; L12H12_LSE_df$SS[idx2] <- thresh


L12H12_LSE_df <- L12H12_LSE_df %>%
  mutate(OR = 1.2,
         state = "Low Elastance") %>%
  arrange(as.numeric(SS)) %>%
  mutate(Futility = cumsum(Result == "futility") / n() + runif(n(), 0.000,0.0001),
         Superiority = cumsum(Result == "superiority") / n() + runif(n(), 0.000,0.0001),
         Nothing = 1 - Futility - Superiority)
################ HSE ######################
L12H12_HSE <- L12H12[, c(1,3)]

#simulation index
id <- NULL
#sample size
SS <- NULL
#treatment
TxARM <- NULL
#trial result
Result <- NULL
for(i in 1: 1000){
  id[i] <- i 
  SS[i] <- L12H12_HSE[3 * (i - 1) + 1, 2]
  TxARM[i] <- L12H12_HSE[3 * (i - 1) + 2,2]
  Result[i] <- L12H12_HSE[3 * (i - 1) + 3,2]
}
L12H12_HSE_df <- data.frame(
  id = id,
  SS = as.numeric(SS),
  TxARM = TxARM,
  Result = Result
)
L12H12_HSE_df <- L12H12_HSE_df %>%
  filter(!is.na(Result))
idx <- L12H12_HSE_df$TxARM == "UC" & L12H12_HSE_df$Result == "superiority"
L12H12_HSE_df$TxARM[idx] <- "DPL"
L12H12_HSE_df$Result[idx] <- "futility"

thresh <- ifelse(length(L12H12_HSE_df$SS[L12H12_HSE_df$SS > 2000]) != 0, min((L12H12_HSE_df$SS[L12H12_HSE_df$SS > 2000])), 2000)
idx2 <- as.numeric(L12H12_HSE_df$SS) > thresh
L12H12_HSE_df$Result[idx2]  <- "Nothing"; L12H12_HSE_df$SS[idx2] <- thresh


L12H12_HSE_df <- L12H12_HSE_df %>%
  mutate(OR = 1.2,
         state = "High Elastance") %>%
  arrange(as.numeric(SS)) %>%
  mutate(Futility = cumsum(Result == "futility") / n() + runif(n(), 0.000,0.0001),
         Superiority = cumsum(Result == "superiority") / n() + runif(n(), 0.000,0.0001),
         Nothing = 1 - Futility - Superiority)

#####################################################################################
L125H125 <- read.csv("L125H125.csv")
################ LSE ######################
L125H125_LSE <- L125H125[, 1:2]

#simulation index
id <- NULL
#sample size
SS <- NULL
#treatment
TxARM <- NULL
#trial result
Result <- NULL
for(i in 1: 1000){
  id[i] <- i 
  SS[i] <- L125H125_LSE[3 * (i - 1) + 1, 2]
  TxARM[i] <- L125H125_LSE[3 * (i - 1) + 2,2]
  Result[i] <- L125H125_LSE[3 * (i - 1) + 3,2]
}
L125H125_LSE_df <- data.frame(
  id = id,
  SS = as.numeric(SS),
  TxARM = TxARM,
  Result = Result
)
L125H125_LSE_df <- L125H125_LSE_df %>%
  filter(!is.na(Result))
idx <- L125H125_LSE_df$TxARM == "UC" & L125H125_LSE_df$Result == "superiority"
L125H125_LSE_df$TxARM[idx] <- "DPL"
L125H125_LSE_df$Result[idx] <- "futility"

thresh <- ifelse(length(L125H125_LSE_df$SS[L125H125_LSE_df$SS > 2000]) != 0, min((L125H125_LSE_df$SS[L125H125_LSE_df$SS > 2000])), 2000)
idx2 <- as.numeric(L125H125_LSE_df$SS) > thresh
L125H125_LSE_df$Result[idx2]  <- "Nothing"; L125H125_LSE_df$SS[idx2] <- thresh


L125H125_LSE_df <- L125H125_LSE_df %>%
  mutate(OR = 1.25,
         state = "Low Elastance") %>%
  arrange(as.numeric(SS)) %>%
  mutate(Futility = cumsum(Result == "futility") / n() + runif(n(), 0.000,0.0001),
         Superiority = cumsum(Result == "superiority") / n() + runif(n(), 0.000,0.0001),
         Nothing = 1 - Futility - Superiority)
################ HSE ######################
L125H125_HSE <- L125H125[, c(1,3)]

#simulation index
id <- NULL
#sample size
SS <- NULL
#treatment
TxARM <- NULL
#trial result
Result <- NULL
for(i in 1: 1000){
  id[i] <- i 
  SS[i] <- L125H125_HSE[3 * (i - 1) + 1, 2]
  TxARM[i] <- L125H125_HSE[3 * (i - 1) + 2,2]
  Result[i] <- L125H125_HSE[3 * (i - 1) + 3,2]
}
L125H125_HSE_df <- data.frame(
  id = id,
  SS = as.numeric(SS),
  TxARM = TxARM,
  Result = Result
)
L125H125_HSE_df <- L125H125_HSE_df %>%
  filter(!is.na(Result))
idx <- L125H125_HSE_df$TxARM == "UC" & L125H125_HSE_df$Result == "superiority"
L125H125_HSE_df$TxARM[idx] <- "DPL"
L125H125_HSE_df$Result[idx] <- "futility"

thresh <- ifelse(length(L125H125_HSE_df$SS[L125H125_HSE_df$SS > 2000]) != 0, min((L125H125_HSE_df$SS[L125H125_HSE_df$SS > 2000])), 2000)
idx2 <- as.numeric(L125H125_HSE_df$SS) > thresh
L125H125_HSE_df$Result[idx2]  <- "Nothing"; L125H125_HSE_df$SS[idx2] <- thresh


L125H125_HSE_df <- L125H125_HSE_df %>%
  mutate(OR = 1.25,
         state = "High Elastance") %>%
  arrange(as.numeric(SS)) %>%
  mutate(Futility = cumsum(Result == "futility") / n() + runif(n(), 0.000,0.0001),
         Superiority = cumsum(Result == "superiority") / n() + runif(n(), 0.000,0.0001),
         Nothing = 1 - Futility - Superiority)

#####################################################################################
L13H13 <- read.csv("L13H13.csv")
################ LSE ######################
L13H13_LSE <- L13H13[, 1:2]

#simulation index
id <- NULL
#sample size
SS <- NULL
#treatment
TxARM <- NULL
#trial result
Result <- NULL
for(i in 1: 1000){
  id[i] <- i 
  SS[i] <- L13H13_LSE[3 * (i - 1) + 1, 2]
  TxARM[i] <- L13H13_LSE[3 * (i - 1) + 2,2]
  Result[i] <- L13H13_LSE[3 * (i - 1) + 3,2]
}
L13H13_LSE_df <- data.frame(
  id = id,
  SS = as.numeric(SS),
  TxARM = TxARM,
  Result = Result
)
L13H13_LSE_df <- L13H13_LSE_df %>%
  filter(!is.na(Result))
idx <- L13H13_LSE_df$TxARM == "UC" & L13H13_LSE_df$Result == "superiority"
L13H13_LSE_df$TxARM[idx] <- "DPL"
L13H13_LSE_df$Result[idx] <- "futility"

thresh <- ifelse(length(L13H13_LSE_df$SS[L13H13_LSE_df$SS > 2000]) != 0, min((L13H13_LSE_df$SS[L13H13_LSE_df$SS > 2000])), 2000)
idx2 <- as.numeric(L13H13_LSE_df$SS) > thresh
L13H13_LSE_df$Result[idx2]  <- "Nothing"; L13H13_LSE_df$SS[idx2] <- thresh


L13H13_LSE_df <- L13H13_LSE_df %>%
  mutate(OR = 1.3,
         state = "Low Elastance") %>%
  arrange(as.numeric(SS)) %>%
  mutate(Futility = cumsum(Result == "futility") / n() + runif(n(), 0.000,0.0001),
         Superiority = cumsum(Result == "superiority") / n() + runif(n(), 0.000,0.0001),
         Nothing = 1 - Futility - Superiority)
################ HSE ######################
L13H13_HSE <- L13H13[, c(1,3)]

#simulation index
id <- NULL
#sample size
SS <- NULL
#treatment
TxARM <- NULL
#trial result
Result <- NULL
for(i in 1: 1000){
  id[i] <- i 
  SS[i] <- L13H13_HSE[3 * (i - 1) + 1, 2]
  TxARM[i] <- L13H13_HSE[3 * (i - 1) + 2,2]
  Result[i] <- L13H13_HSE[3 * (i - 1) + 3,2]
}
L13H13_HSE_df <- data.frame(
  id = id,
  SS = as.numeric(SS),
  TxARM = TxARM,
  Result = Result
)
L13H13_HSE_df <- L13H13_HSE_df %>%
  filter(!is.na(Result))
idx <- L13H13_HSE_df$TxARM == "UC" & L13H13_HSE_df$Result == "superiority"
L13H13_HSE_df$TxARM[idx] <- "DPL"
L13H13_HSE_df$Result[idx] <- "futility"

thresh <- ifelse(length(L13H13_HSE_df$SS[L13H13_HSE_df$SS > 2000]) != 0, min((L13H13_HSE_df$SS[L13H13_HSE_df$SS > 2000])), 2000)
idx2 <- as.numeric(L13H13_HSE_df$SS) > thresh
L13H13_HSE_df$Result[idx2]  <- "Nothing"; L13H13_HSE_df$SS[idx2] <- thresh

L13H13_HSE_df <- L13H13_HSE_df %>%
  mutate(OR = 1.3,
         state = "High Elastance") %>%
  arrange(as.numeric(SS)) %>%
  mutate(Futility = cumsum(Result == "futility") / n() + runif(n(), 0.000,0.0001),
         Superiority = cumsum(Result == "superiority") / n() + runif(n(), 0.000,0.0001),
         Nothing = 1 - Futility - Superiority)

#####################################################################################
L15H15 <- read.csv("L15H15.csv")
################ LSE ######################
L15H15_LSE <- L15H15[, 1:2]

#simulation index
id <- NULL
#sample size
SS <- NULL
#treatment
TxARM <- NULL
#trial result
Result <- NULL
for(i in 1: 1000){
  id[i] <- i 
  SS[i] <- L15H15_LSE[3 * (i - 1) + 1, 2]
  TxARM[i] <- L15H15_LSE[3 * (i - 1) + 2,2]
  Result[i] <- L15H15_LSE[3 * (i - 1) + 3,2]
}
L15H15_LSE_df <- data.frame(
  id = id,
  SS = as.numeric(SS),
  TxARM = TxARM,
  Result = Result
)
L15H15_LSE_df <- L15H15_LSE_df %>%
  filter(!is.na(Result))
idx <- L15H15_LSE_df$TxARM == "UC" & L15H15_LSE_df$Result == "superiority"
L15H15_LSE_df$TxARM[idx] <- "DPL"
L15H15_LSE_df$Result[idx] <- "futility"

thresh <- ifelse(length(L15H15_LSE_df$SS[L15H15_LSE_df$SS > 2000]) != 0, min((L15H15_LSE_df$SS[L15H15_LSE_df$SS > 2000])), 2000)
idx2 <- as.numeric(L15H15_LSE_df$SS) > thresh
L15H15_LSE_df$Result[idx2]  <- "Nothing"; L15H15_LSE_df$SS[idx2] <- thresh

L15H15_LSE_df <- L15H15_LSE_df %>%
  mutate(OR = 1.5,
         state = "Low Elastance") %>%
  arrange(as.numeric(SS)) %>%
  mutate(Futility = cumsum(Result == "futility") / n() + runif(n(), 0.000,0.0001),
         Superiority = cumsum(Result == "superiority") / n() + runif(n(), 0.000,0.0001),
         Nothing = 1 - Futility - Superiority)
################ HSE ######################
L15H15_HSE <- L15H15[, c(1,3)]

#simulation index
id <- NULL
#sample size
SS <- NULL
#treatment
TxARM <- NULL
#trial result
Result <- NULL
for(i in 1: 1000){
  id[i] <- i 
  SS[i] <- L15H15_HSE[3 * (i - 1) + 1, 2]
  TxARM[i] <- L15H15_HSE[3 * (i - 1) + 2,2]
  Result[i] <- L15H15_HSE[3 * (i - 1) + 3,2]
}
L15H15_HSE_df <- data.frame(
  id = id,
  SS = as.numeric(SS),
  TxARM = TxARM,
  Result = Result
)
L15H15_HSE_df <- L15H15_HSE_df %>%
  filter(!is.na(Result))
idx <- L15H15_HSE_df$TxARM == "UC" & L15H15_HSE_df$Result == "superiority"
L15H15_HSE_df$TxARM[idx] <- "DPL"
L15H15_HSE_df$Result[idx] <- "futility"

thresh <- ifelse(length(L15H15_HSE_df$SS[L15H15_HSE_df$SS > 2000]) != 0, min((L15H15_HSE_df$SS[L15H15_HSE_df$SS > 2000])), 2000)
idx2 <- as.numeric(L15H15_HSE_df$SS) > thresh
L15H15_HSE_df$Result[idx2]  <- "Nothing"; L15H15_HSE_df$SS[idx2] <- thresh

L15H15_HSE_df <- L15H15_HSE_df %>%
  mutate(OR = 1.5,
         state = "High Elastance") %>%
  arrange(as.numeric(SS)) %>%
  mutate(Futility = cumsum(Result == "futility") / n() + runif(n(), 0.000,0.0001),
         Superiority = cumsum(Result == "superiority") / n() + runif(n(), 0.000,0.0001),
         Nothing = 1 - Futility - Superiority)

final_data <- rbind(L08H08_LSE_df, L08H08_HSE_df,
                    L1H1_LSE_df, L1H1_HSE_df,
                    L11H11_LSE_df, L11H11_HSE_df,
                    L12H12_LSE_df, L12H12_HSE_df,
                    L125H125_LSE_df, L125H125_HSE_df,
                    L13H13_LSE_df, L13H13_HSE_df,
                    L15H15_LSE_df, L15H15_HSE_df)

final_data <- gather(final_data, fact, p, Futility:Nothing, factor_key=TRUE)
final_data <- final_data %>%
  filter(fact != "Nothing")
ggplot(final_data, aes(SS, p, col = as.factor(OR), linetype = as.factor(OR))) + 
  facet_grid(fact~state) +
  geom_line(lwd = 1.02) + 
  xlim(200, 2200) + 
  labs(col = "Odds Ratio", linetype = "Odds Ratio") +
  theme_bw() + 
  xlab("Trial Sample Size") + 
  ylab("Probability") + 
  scale_colour_grey() +
  theme(panel.grid.minor = element_line(colour="lightgray", size=0.5)) +
  scale_y_continuous(breaks = seq(0, 1, length.out = 11))





