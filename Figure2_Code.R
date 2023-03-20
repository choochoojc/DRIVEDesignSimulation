library(ggplot2)
library(tidyverse)
library(dplyr)
library(ggpubr)

##############  L08H1  #####################

setwd("C:/Users/Ziming Chen/OneDrive - SickKids/DRIVE/Simulation/Cross Scenarios")

L08H1 <- as.data.frame(read.csv("L08H1.csv"))

################ LSE ######################
L08H1_LSE <- L08H1[, 1:2]

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
  SS[i] <- L08H1_LSE[3 * (i - 1) + 1, 2]
  TxARM[i] <- L08H1_LSE[3 * (i - 1) + 2,2]
  Result[i] <- L08H1_LSE[3 * (i - 1) + 3,2]
}
L08H1_LSE_df <- data.frame(
  id = id,
  SS = as.numeric(SS),
  TxARM = TxARM,
  Result = Result
)

idx <- L08H1_LSE_df$TxARM == "UC" & L08H1_LSE_df$Result == "superiority"
L08H1_LSE_df$TxARM[idx] <- "DPL"
L08H1_LSE_df$Result[idx] <- "futility"

thresh <- ifelse(length(L08H1_LSE_df$SS[L08H1_LSE_df$SS > 2000]) != 0, min((L08H1_LSE_df$SS[L08H1_LSE_df$SS > 2000])), 2000)
idx2 <- as.numeric(L08H1_LSE_df$SS) > thresh
L08H1_LSE_df$Result[idx2]  <- "Nothing"
L08H1_LSE_df$SS[idx2] <- thresh

SS50_L08H1_LSE <- ceiling(quantile(as.numeric(L08H1_LSE_df$SS), probs = c(0.5)))
SS80_L08H1_LSE <- ceiling(quantile(as.numeric(L08H1_LSE_df$SS), probs = c(0.8)))
p_sup_L08H1_LSE <- nrow(L08H1_LSE_df[L08H1_LSE_df$Result == "superiority",])/nrow(L08H1_LSE_df)
p_fut_L08H1_LSE <- nrow(L08H1_LSE_df[L08H1_LSE_df$Result == "futility",])/nrow(L08H1_LSE_df)
p_no_L08H1_LSE <- nrow(L08H1_LSE_df[L08H1_LSE_df$Result == "Nothing",])/nrow(L08H1_LSE_df)

################ HSE ######################
L08H1_HSE <- L08H1[, c(1,3)]

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
  SS[i] <- L08H1_HSE[3 * (i - 1) + 1, 2]
  TxARM[i] <- L08H1_HSE[3 * (i - 1) + 2,2]
  Result[i] <- L08H1_HSE[3 * (i - 1) + 3,2]
}
L08H1_HSE_df <- data.frame(
  id = id,
  SS = as.numeric(SS),
  TxARM = TxARM,
  Result = Result
)

idx <- L08H1_HSE_df$TxARM == "UC" & L08H1_HSE_df$Result == "superiority"
L08H1_HSE_df$TxARM[idx] <- "DPL"
L08H1_HSE_df$Result[idx] <- "futility"


thresh <- ifelse(length(L08H1_HSE_df$SS[L08H1_HSE_df$SS > 2000]) != 0, min((L08H1_HSE_df$SS[L08H1_HSE_df$SS > 2000])), 2000)
idx2 <- as.numeric(L08H1_HSE_df$SS) > thresh
L08H1_HSE_df$Result[idx2]  <- "Nothing"
L08H1_HSE_df$SS[idx2] <- thresh

SS50_L08H1_HSE <- ceiling(quantile(as.numeric(L08H1_HSE_df$SS), probs = c(0.5)))
SS80_L08H1_HSE <- ceiling(quantile(as.numeric(L08H1_HSE_df$SS), probs = c(0.8)))
p_sup_L08H1_HSE <- nrow(L08H1_HSE_df[L08H1_HSE_df$Result == "superiority",])/nrow(L08H1_HSE_df)
p_fut_L08H1_HSE <- nrow(L08H1_HSE_df[L08H1_HSE_df$Result == "futility",])/nrow(L08H1_HSE_df)
p_no_L08H1_HSE <- nrow(L08H1_HSE_df[L08H1_HSE_df$Result == "Nothing",])/nrow(L08H1_HSE_df)

##############  L11H1  #####################

L11H1 <- as.data.frame(read.csv("L11H1.csv"))

################ LSE ######################
L11H1_LSE <- L11H1[, 1:2]

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
  SS[i] <- L11H1_LSE[3 * (i - 1) + 1, 2]
  TxARM[i] <- L11H1_LSE[3 * (i - 1) + 2,2]
  Result[i] <- L11H1_LSE[3 * (i - 1) + 3,2]
}
L11H1_LSE_df <- data.frame(
  id = id,
  SS = as.numeric(SS),
  TxARM = TxARM,
  Result = Result
)

idx <- L11H1_LSE_df$TxARM == "UC" & L11H1_LSE_df$Result == "superiority"
L11H1_LSE_df$TxARM[idx] <- "DPL"
L11H1_LSE_df$Result[idx] <- "futility"


thresh <- ifelse(length(L11H1_LSE_df$SS[L11H1_LSE_df$SS > 2000]) != 0, min((L11H1_LSE_df$SS[L11H1_LSE_df$SS > 2000])), 2000)
idx2 <- as.numeric(L11H1_LSE_df$SS) > thresh
L11H1_LSE_df$Result[idx2]  <- "Nothing"; L11H1_LSE_df$SS[idx2] <- thresh


SS50_L11H1_LSE <- ceiling(quantile(as.numeric(L11H1_LSE_df$SS), probs = c(0.5)))
SS80_L11H1_LSE <- ceiling(quantile(as.numeric(L11H1_LSE_df$SS), probs = c(0.8)))
p_sup_L11H1_LSE <- nrow(L11H1_LSE_df[L11H1_LSE_df$Result == "superiority",])/nrow(L11H1_LSE_df)
p_fut_L11H1_LSE <- nrow(L11H1_LSE_df[L11H1_LSE_df$Result == "futility",])/nrow(L11H1_LSE_df)
p_no_L11H1_LSE <- nrow(L11H1_LSE_df[L11H1_LSE_df$Result == "Nothing",])/nrow(L11H1_LSE_df)

################ HSE ######################
L11H1_HSE <- L11H1[, c(1,3)]

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
  SS[i] <- L11H1_HSE[3 * (i - 1) + 1, 2]
  TxARM[i] <- L11H1_HSE[3 * (i - 1) + 2,2]
  Result[i] <- L11H1_HSE[3 * (i - 1) + 3,2]
}
L11H1_HSE_df <- data.frame(
  id = id,
  SS = as.numeric(SS),
  TxARM = TxARM,
  Result = Result
)

idx <- L11H1_HSE_df$TxARM == "UC" & L11H1_HSE_df$Result == "superiority"
L11H1_HSE_df$TxARM[idx] <- "DPL"
L11H1_HSE_df$Result[idx] <- "futility"

thresh <- ifelse(length(L11H1_HSE_df$SS[L11H1_HSE_df$SS > 2000]) != 0, min((L11H1_HSE_df$SS[L11H1_HSE_df$SS > 2000])), 2000)
idx2 <- as.numeric(L11H1_HSE_df$SS) > thresh
L11H1_HSE_df$Result[idx2]  <- "Nothing"; L11H1_HSE_df$SS[idx2] <- thresh

SS50_L11H1_HSE <- ceiling(quantile(as.numeric(L11H1_HSE_df$SS), probs = c(0.5)))
SS80_L11H1_HSE <- ceiling(quantile(as.numeric(L11H1_HSE_df$SS), probs = c(0.8)))
p_sup_L11H1_HSE <- nrow(L11H1_HSE_df[L11H1_HSE_df$Result == "superiority",])/nrow(L11H1_HSE_df)
p_fut_L11H1_HSE <- nrow(L11H1_HSE_df[L11H1_HSE_df$Result == "futility",])/nrow(L11H1_HSE_df)
p_no_L11H1_HSE <- nrow(L11H1_HSE_df[L11H1_HSE_df$Result == "Nothing",])/nrow(L11H1_HSE_df)


##############  L12H1  #####################

L12H1 <- as.data.frame(read.csv("L12H1.csv"))

################ LSE ######################
L12H1_LSE <- L12H1[, 1:2]

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
  SS[i] <- L12H1_LSE[3 * (i - 1) + 1, 2]
  TxARM[i] <- L12H1_LSE[3 * (i - 1) + 2,2]
  Result[i] <- L12H1_LSE[3 * (i - 1) + 3,2]
}
L12H1_LSE_df <- data.frame(
  id = id,
  SS = as.numeric(SS),
  TxARM = TxARM,
  Result = Result
)

idx <- L12H1_LSE_df$TxARM == "UC" & L12H1_LSE_df$Result == "superiority"
L12H1_LSE_df$TxARM[idx] <- "DPL"
L12H1_LSE_df$Result[idx] <- "futility"

thresh <- ifelse(length(L12H1_LSE_df$SS[L12H1_LSE_df$SS > 2000]) != 0, min((L12H1_LSE_df$SS[L12H1_LSE_df$SS > 2000])), 2000)
idx2 <- as.numeric(L12H1_LSE_df$SS) > thresh
L12H1_LSE_df$Result[idx2]  <- "Nothing"; L12H1_LSE_df$SS[idx2] <- thresh


SS50_L12H1_LSE <- ceiling(quantile(as.numeric(L12H1_LSE_df$SS), probs = c(0.5)))
SS80_L12H1_LSE <- ceiling(quantile(as.numeric(L12H1_LSE_df$SS), probs = c(0.8)))
p_sup_L12H1_LSE <- nrow(L12H1_LSE_df[L12H1_LSE_df$Result == "superiority",])/nrow(L12H1_LSE_df)
p_fut_L12H1_LSE <- nrow(L12H1_LSE_df[L12H1_LSE_df$Result == "futility",])/nrow(L12H1_LSE_df)
p_no_L12H1_LSE <- nrow(L12H1_LSE_df[L12H1_LSE_df$Result == "Nothing",])/nrow(L12H1_LSE_df)

################ HSE ######################
L12H1_HSE <- L12H1[, c(1,3)]

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
  SS[i] <- L12H1_HSE[3 * (i - 1) + 1, 2]
  TxARM[i] <- L12H1_HSE[3 * (i - 1) + 2,2]
  Result[i] <- L12H1_HSE[3 * (i - 1) + 3,2]
}
L12H1_HSE_df <- data.frame(
  id = id,
  SS = as.numeric(SS),
  TxARM = TxARM,
  Result = Result
)

idx <- L12H1_HSE_df$TxARM == "UC" & L12H1_HSE_df$Result == "superiority"
L12H1_HSE_df$TxARM[idx] <- "DPL"
L12H1_HSE_df$Result[idx] <- "futility"

thresh <- ifelse(length(L12H1_HSE_df$SS[L12H1_HSE_df$SS > 2000]) != 0, min((L12H1_HSE_df$SS[L12H1_HSE_df$SS > 2000])), 2000)
idx2 <- as.numeric(L12H1_HSE_df$SS) > thresh
L12H1_HSE_df$Result[idx2]  <- "Nothing"; L12H1_HSE_df$SS[idx2] <- thresh


SS50_L12H1_HSE <- ceiling(quantile(as.numeric(L12H1_HSE_df$SS), probs = c(0.5)))
SS80_L12H1_HSE <- ceiling(quantile(as.numeric(L12H1_HSE_df$SS), probs = c(0.8)))
p_sup_L12H1_HSE <- nrow(L12H1_HSE_df[L12H1_HSE_df$Result == "superiority",])/nrow(L12H1_HSE_df)
p_fut_L12H1_HSE <- nrow(L12H1_HSE_df[L12H1_HSE_df$Result == "futility",])/nrow(L12H1_HSE_df)
p_no_L12H1_HSE <- nrow(L12H1_HSE_df[L12H1_HSE_df$Result == "Nothing",])/nrow(L12H1_HSE_df)

##############  L125H1  #####################

L125H1 <- as.data.frame(read.csv("L125H1.csv"))

################ LSE ######################
L125H1_LSE <- L125H1[, 1:2]

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
  SS[i] <- L125H1_LSE[3 * (i - 1) + 1, 2]
  TxARM[i] <- L125H1_LSE[3 * (i - 1) + 2,2]
  Result[i] <- L125H1_LSE[3 * (i - 1) + 3,2]
}
L125H1_LSE_df <- data.frame(
  id = id,
  SS = as.numeric(SS),
  TxARM = TxARM,
  Result = Result
)

idx <- L125H1_LSE_df$TxARM == "UC" & L125H1_LSE_df$Result == "superiority"
L125H1_LSE_df$TxARM[idx] <- "DPL"
L125H1_LSE_df$Result[idx] <- "futility"

thresh <- ifelse(length(L125H1_LSE_df$SS[L125H1_LSE_df$SS > 2000]) != 0, min((L125H1_LSE_df$SS[L125H1_LSE_df$SS > 2000])), 2000)
idx2 <- as.numeric(L125H1_LSE_df$SS) > thresh
L125H1_LSE_df$Result[idx2]  <- "Nothing"; L125H1_LSE_df$SS[idx2] <- thresh


SS50_L125H1_LSE <- ceiling(quantile(as.numeric(L125H1_LSE_df$SS), probs = c(0.5)))
SS80_L125H1_LSE <- ceiling(quantile(as.numeric(L125H1_LSE_df$SS), probs = c(0.8)))
p_sup_L125H1_LSE <- nrow(L125H1_LSE_df[L125H1_LSE_df$Result == "superiority",])/nrow(L125H1_LSE_df)
p_fut_L125H1_LSE <- nrow(L125H1_LSE_df[L125H1_LSE_df$Result == "futility",])/nrow(L125H1_LSE_df)
p_no_L125H1_LSE <- nrow(L125H1_LSE_df[L125H1_LSE_df$Result == "Nothing",])/nrow(L125H1_LSE_df)

################ HSE ######################
L125H1_HSE <- L125H1[, c(1,3)]

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
  SS[i] <- L125H1_HSE[3 * (i - 1) + 1, 2]
  TxARM[i] <- L125H1_HSE[3 * (i - 1) + 2,2]
  Result[i] <- L125H1_HSE[3 * (i - 1) + 3,2]
}
L125H1_HSE_df <- data.frame(
  id = id,
  SS = as.numeric(SS),
  TxARM = TxARM,
  Result = Result
)



idx <- L125H1_HSE_df$TxARM == "UC" & L125H1_HSE_df$Result == "superiority"
L125H1_HSE_df$TxARM[idx] <- "DPL"
L125H1_HSE_df$Result[idx] <- "futility"

thresh <- ifelse(length(L125H1_HSE_df$SS[L125H1_HSE_df$SS > 2000]) != 0, min((L125H1_HSE_df$SS[L125H1_HSE_df$SS > 2000])), 2000)
idx2 <- as.numeric(L125H1_HSE_df$SS) > thresh
L125H1_HSE_df$Result[idx2]  <- "Nothing"; L125H1_HSE_df$SS[idx2] <- thresh

SS50_L125H1_HSE <- ceiling(quantile(as.numeric(L125H1_HSE_df$SS), probs = c(0.5)))
SS80_L125H1_HSE <- ceiling(quantile(as.numeric(L125H1_HSE_df$SS), probs = c(0.8)))
p_sup_L125H1_HSE <- nrow(L125H1_HSE_df[L125H1_HSE_df$Result == "superiority",])/nrow(L125H1_HSE_df)
p_fut_L125H1_HSE <- nrow(L125H1_HSE_df[L125H1_HSE_df$Result == "futility",])/nrow(L125H1_HSE_df)
p_no_L125H1_HSE <- nrow(L125H1_HSE_df[L125H1_HSE_df$Result == "Nothing",])/nrow(L125H1_HSE_df)

##############  L13H1  #####################

L13H1 <- as.data.frame(read.csv("L13H1.csv"))

################ LSE ######################
L13H1_LSE <- L13H1[, 1:2]

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
  SS[i] <- L13H1_LSE[3 * (i - 1) + 1, 2]
  TxARM[i] <- L13H1_LSE[3 * (i - 1) + 2,2]
  Result[i] <- L13H1_LSE[3 * (i - 1) + 3,2]
}
L13H1_LSE_df <- data.frame(
  id = id,
  SS = as.numeric(SS),
  TxARM = TxARM,
  Result = Result
)

idx <- L13H1_LSE_df$TxARM == "UC" & L13H1_LSE_df$Result == "superiority"
L13H1_LSE_df$TxARM[idx] <- "DPL"
L13H1_LSE_df$Result[idx] <- "futility"

thresh <- ifelse(length(L13H1_LSE_df$SS[L13H1_LSE_df$SS > 2000]) != 0, min((L13H1_LSE_df$SS[L13H1_LSE_df$SS > 2000])), 2000)
idx2 <- as.numeric(L13H1_LSE_df$SS) > thresh
L13H1_LSE_df$Result[idx2]  <- "Nothing"; L13H1_LSE_df$SS[idx2] <- thresh


SS50_L13H1_LSE <- ceiling(quantile(as.numeric(L13H1_LSE_df$SS), probs = c(0.5)))
SS80_L13H1_LSE <- ceiling(quantile(as.numeric(L13H1_LSE_df$SS), probs = c(0.8)))
p_sup_L13H1_LSE <- nrow(L13H1_LSE_df[L13H1_LSE_df$Result == "superiority",])/nrow(L13H1_LSE_df)
p_fut_L13H1_LSE <- nrow(L13H1_LSE_df[L13H1_LSE_df$Result == "futility",])/nrow(L13H1_LSE_df)
p_no_L13H1_LSE <- nrow(L13H1_LSE_df[L13H1_LSE_df$Result == "Nothing",])/nrow(L13H1_LSE_df)

################ HSE ######################
L13H1_HSE <- L13H1[, c(1,3)]

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
  SS[i] <- L13H1_HSE[3 * (i - 1) + 1, 2]
  TxARM[i] <- L13H1_HSE[3 * (i - 1) + 2,2]
  Result[i] <- L13H1_HSE[3 * (i - 1) + 3,2]
}
L13H1_HSE_df <- data.frame(
  id = id,
  SS = as.numeric(SS),
  TxARM = TxARM,
  Result = Result
)

idx <- L13H1_HSE_df$TxARM == "UC" & L13H1_HSE_df$Result == "superiority"
L13H1_HSE_df$TxARM[idx] <- "DPL"
L13H1_HSE_df$Result[idx] <- "futility"

thresh <- ifelse(length(L13H1_HSE_df$SS[L13H1_HSE_df$SS > 2000]) != 0, min((L13H1_HSE_df$SS[L13H1_HSE_df$SS > 2000])), 2000)
idx2 <- as.numeric(L13H1_HSE_df$SS) > thresh
L13H1_HSE_df$Result[idx2]  <- "Nothing"; L13H1_HSE_df$SS[idx2] <- thresh


SS50_L13H1_HSE <- ceiling(quantile(as.numeric(L13H1_HSE_df$SS), probs = c(0.5)))
SS80_L13H1_HSE <- ceiling(quantile(as.numeric(L13H1_HSE_df$SS), probs = c(0.8)))
p_sup_L13H1_HSE <- nrow(L13H1_HSE_df[L13H1_HSE_df$Result == "superiority",])/nrow(L13H1_HSE_df)
p_fut_L13H1_HSE <- nrow(L13H1_HSE_df[L13H1_HSE_df$Result == "futility",])/nrow(L13H1_HSE_df)
p_no_L13H1_HSE <- nrow(L13H1_HSE_df[L13H1_HSE_df$Result == "Nothing",])/nrow(L13H1_HSE_df)


##############  L15H1  #####################

L15H1 <- as.data.frame(read.csv("L15H1.csv"))

################ LSE ######################
L15H1_LSE <- L15H1[, 1:2]

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
  SS[i] <- L15H1_LSE[3 * (i - 1) + 1, 2]
  TxARM[i] <- L15H1_LSE[3 * (i - 1) + 2,2]
  Result[i] <- L15H1_LSE[3 * (i - 1) + 3,2]
}
L15H1_LSE_df <- data.frame(
  id = id,
  SS = as.numeric(SS),
  TxARM = TxARM,
  Result = Result
)

idx <- L15H1_LSE_df$TxARM == "UC" & L15H1_LSE_df$Result == "superiority"
L15H1_LSE_df$TxARM[idx] <- "DPL"
L15H1_LSE_df$Result[idx] <- "futility"

thresh <- ifelse(length(L15H1_LSE_df$SS[L15H1_LSE_df$SS > 2000]) != 0, min((L15H1_LSE_df$SS[L15H1_LSE_df$SS > 2000])), 2000)
idx2 <- as.numeric(L15H1_LSE_df$SS) > thresh
L15H1_LSE_df$Result[idx2]  <- "Nothing"; L15H1_LSE_df$SS[idx2] <- thresh


SS50_L15H1_LSE <- ceiling(quantile(as.numeric(L15H1_LSE_df$SS), probs = c(0.5)))
SS80_L15H1_LSE <- ceiling(quantile(as.numeric(L15H1_LSE_df$SS), probs = c(0.8)))
p_sup_L15H1_LSE <- nrow(L15H1_LSE_df[L15H1_LSE_df$Result == "superiority",])/nrow(L15H1_LSE_df)
p_fut_L15H1_LSE <- nrow(L15H1_LSE_df[L15H1_LSE_df$Result == "futility",])/nrow(L15H1_LSE_df)
p_no_L15H1_LSE <- nrow(L15H1_LSE_df[L15H1_LSE_df$Result == "Nothing",])/nrow(L15H1_LSE_df)

################ HSE ######################
L15H1_HSE <- L15H1[, c(1,3)]

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
  SS[i] <- L15H1_HSE[3 * (i - 1) + 1, 2]
  TxARM[i] <- L15H1_HSE[3 * (i - 1) + 2,2]
  Result[i] <- L15H1_HSE[3 * (i - 1) + 3,2]
}
L15H1_HSE_df <- data.frame(
  id = id,
  SS = as.numeric(SS),
  TxARM = TxARM,
  Result = Result
)

idx <- L15H1_HSE_df$TxARM == "UC" & L15H1_HSE_df$Result == "superiority"
L15H1_HSE_df$TxARM[idx] <- "DPL"
L15H1_HSE_df$Result[idx] <- "futility"

thresh <- ifelse(length(L15H1_HSE_df$SS[L15H1_HSE_df$SS > 2000]) != 0, min((L15H1_HSE_df$SS[L15H1_HSE_df$SS > 2000])), 2000)
idx2 <- as.numeric(L15H1_HSE_df$SS) > thresh
L15H1_HSE_df$Result[idx2]  <- "Nothing"; L15H1_HSE_df$SS[idx2] <- thresh


SS50_L15H1_HSE <- ceiling(quantile(as.numeric(L15H1_HSE_df$SS), probs = c(0.5)))
SS80_L15H1_HSE <- ceiling(quantile(as.numeric(L15H1_HSE_df$SS), probs = c(0.8)))
p_sup_L15H1_HSE <- nrow(L15H1_HSE_df[L15H1_HSE_df$Result == "superiority",])/nrow(L15H1_HSE_df)
p_fut_L15H1_HSE <- nrow(L15H1_HSE_df[L15H1_HSE_df$Result == "futility",])/nrow(L15H1_HSE_df)
p_no_L15H1_HSE <- nrow(L15H1_HSE_df[L15H1_HSE_df$Result == "Nothing",])/nrow(L15H1_HSE_df)


##############  L1H08  #####################

L1H08 <- as.data.frame(read.csv("L1H08.csv"))

################ LSE ######################
L1H08_LSE <- L1H08[, 1:2]

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
  SS[i] <- L1H08_LSE[3 * (i - 1) + 1, 2]
  TxARM[i] <- L1H08_LSE[3 * (i - 1) + 2,2]
  Result[i] <- L1H08_LSE[3 * (i - 1) + 3,2]
}
L1H08_LSE_df <- data.frame(
  id = id,
  SS = as.numeric(SS),
  TxARM = TxARM,
  Result = Result
)

idx <- L1H08_LSE_df$TxARM == "UC" & L1H08_LSE_df$Result == "superiority"
L1H08_LSE_df$TxARM[idx] <- "DPL"
L1H08_LSE_df$Result[idx] <- "futility"

thresh <- ifelse(length(L1H08_LSE_df$SS[L1H08_LSE_df$SS > 2000]) != 0, min((L1H08_LSE_df$SS[L1H08_LSE_df$SS > 2000])), 2000)
idx2 <- as.numeric(L1H08_LSE_df$SS) > thresh
L1H08_LSE_df$Result[idx2]  <- "Nothing"; L1H08_LSE_df$SS[idx2] <- thresh

SS50_L1H08_LSE <- ceiling(quantile(as.numeric(L1H08_LSE_df$SS), probs = c(0.5)))
SS80_L1H08_LSE <- ceiling(quantile(as.numeric(L1H08_LSE_df$SS), probs = c(0.8)))
p_sup_L1H08_LSE <- nrow(L1H08_LSE_df[L1H08_LSE_df$Result == "superiority",])/nrow(L1H08_LSE_df)
p_fut_L1H08_LSE <- nrow(L1H08_LSE_df[L1H08_LSE_df$Result == "futility",])/nrow(L1H08_LSE_df)
p_no_L1H08_LSE <- nrow(L1H08_LSE_df[L1H08_LSE_df$Result == "Nothing",])/nrow(L1H08_LSE_df)

################ HSE ######################
L1H08_HSE <- L1H08[, c(1,3)]

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
  SS[i] <- L1H08_HSE[3 * (i - 1) + 1, 2]
  TxARM[i] <- L1H08_HSE[3 * (i - 1) + 2,2]
  Result[i] <- L1H08_HSE[3 * (i - 1) + 3,2]
}
L1H08_HSE_df <- data.frame(
  id = id,
  SS = as.numeric(SS),
  TxARM = TxARM,
  Result = Result
)

idx <- L1H08_HSE_df$TxARM == "UC" & L1H08_HSE_df$Result == "superiority"
L1H08_HSE_df$TxARM[idx] <- "DPL"
L1H08_HSE_df$Result[idx] <- "futility"

thresh <- ifelse(length(L1H08_HSE_df$SS[L1H08_HSE_df$SS > 2000]) != 0, min((L1H08_HSE_df$SS[L1H08_HSE_df$SS > 2000])), 2000)
idx2 <- as.numeric(L1H08_HSE_df$SS) > thresh
L1H08_HSE_df$Result[idx2]  <- "Nothing"; L1H08_HSE_df$SS[idx2] <- thresh

SS50_L1H08_HSE <- ceiling(quantile(as.numeric(L1H08_HSE_df$SS), probs = c(0.5)))
SS80_L1H08_HSE <- ceiling(quantile(as.numeric(L1H08_HSE_df$SS), probs = c(0.8)))
p_sup_L1H08_HSE <- nrow(L1H08_HSE_df[L1H08_HSE_df$Result == "superiority",])/nrow(L1H08_HSE_df)
p_fut_L1H08_HSE <- nrow(L1H08_HSE_df[L1H08_HSE_df$Result == "futility",])/nrow(L1H08_HSE_df)
p_no_L1H08_HSE <- nrow(L1H08_HSE_df[L1H08_HSE_df$Result == "Nothing",])/nrow(L1H08_HSE_df)

##############  L1H11  #####################

L1H11 <- as.data.frame(read.csv("L1H11.csv"))

################ LSE ######################
L1H11_LSE <- L1H11[, 1:2]

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
  SS[i] <- L1H11_LSE[3 * (i - 1) + 1, 2]
  TxARM[i] <- L1H11_LSE[3 * (i - 1) + 2,2]
  Result[i] <- L1H11_LSE[3 * (i - 1) + 3,2]
}
L1H11_LSE_df <- data.frame(
  id = id,
  SS = as.numeric(SS),
  TxARM = TxARM,
  Result = Result
)

idx <- L1H11_LSE_df$TxARM == "UC" & L1H11_LSE_df$Result == "superiority"
L1H11_LSE_df$TxARM[idx] <- "DPL"
L1H11_LSE_df$Result[idx] <- "futility"

thresh <- ifelse(length(L1H11_LSE_df$SS[L1H11_LSE_df$SS > 2000]) != 0, min((L1H11_LSE_df$SS[L1H11_LSE_df$SS > 2000])), 2000)
idx2 <- as.numeric(L1H11_LSE_df$SS) > thresh
L1H11_LSE_df$Result[idx2]  <- "Nothing"; L1H11_LSE_df$SS[idx2] <- thresh


SS50_L1H11_LSE <- ceiling(quantile(as.numeric(L1H11_LSE_df$SS), probs = c(0.5)))
SS80_L1H11_LSE <- ceiling(quantile(as.numeric(L1H11_LSE_df$SS), probs = c(0.8)))
p_sup_L1H11_LSE <- nrow(L1H11_LSE_df[L1H11_LSE_df$Result == "superiority",])/nrow(L1H11_LSE_df)
p_fut_L1H11_LSE <- nrow(L1H11_LSE_df[L1H11_LSE_df$Result == "futility",])/nrow(L1H11_LSE_df)
p_no_L1H11_LSE <- nrow(L1H11_LSE_df[L1H11_LSE_df$Result == "Nothing",])/nrow(L1H11_LSE_df)

################ HSE ######################
L1H11_HSE <- L1H11[, c(1,3)]

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
  SS[i] <- L1H11_HSE[3 * (i - 1) + 1, 2]
  TxARM[i] <- L1H11_HSE[3 * (i - 1) + 2,2]
  Result[i] <- L1H11_HSE[3 * (i - 1) + 3,2]
}
L1H11_HSE_df <- data.frame(
  id = id,
  SS = as.numeric(SS),
  TxARM = TxARM,
  Result = Result
)

idx <- L1H11_HSE_df$TxARM == "UC" & L1H11_HSE_df$Result == "superiority"
L1H11_HSE_df$TxARM[idx] <- "DPL"
L1H11_HSE_df$Result[idx] <- "futility"

thresh <- ifelse(length(L1H11_HSE_df$SS[L1H11_HSE_df$SS > 2000]) != 0, min((L1H11_HSE_df$SS[L1H11_HSE_df$SS > 2000])), 2000)
idx2 <- as.numeric(L1H11_HSE_df$SS) > thresh
L1H11_HSE_df$Result[idx2]  <- "Nothing"; L1H11_HSE_df$SS[idx2] <- thresh


SS50_L1H11_HSE <- ceiling(quantile(as.numeric(L1H11_HSE_df$SS), probs = c(0.5)))
SS80_L1H11_HSE <- ceiling(quantile(as.numeric(L1H11_HSE_df$SS), probs = c(0.8)))
p_sup_L1H11_HSE <- nrow(L1H11_HSE_df[L1H11_HSE_df$Result == "superiority",])/nrow(L1H11_HSE_df)
p_fut_L1H11_HSE <- nrow(L1H11_HSE_df[L1H11_HSE_df$Result == "futility",])/nrow(L1H11_HSE_df)
p_no_L1H11_HSE <- nrow(L1H11_HSE_df[L1H11_HSE_df$Result == "Nothing",])/nrow(L1H11_HSE_df)


##############  L1H12  #####################

L1H12 <- as.data.frame(read.csv("L1H12.csv"))

################ LSE ######################
L1H12_LSE <- L1H12[, 1:2]

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
  SS[i] <- L1H12_LSE[3 * (i - 1) + 1, 2]
  TxARM[i] <- L1H12_LSE[3 * (i - 1) + 2,2]
  Result[i] <- L1H12_LSE[3 * (i - 1) + 3,2]
}
L1H12_LSE_df <- data.frame(
  id = id,
  SS = as.numeric(SS),
  TxARM = TxARM,
  Result = Result
)

idx <- L1H12_LSE_df$TxARM == "UC" & L1H12_LSE_df$Result == "superiority"
L1H12_LSE_df$TxARM[idx] <- "DPL"
L1H12_LSE_df$Result[idx] <- "futility"

thresh <- ifelse(length(L1H12_LSE_df$SS[L1H12_LSE_df$SS > 2000]) != 0, min((L1H12_LSE_df$SS[L1H12_LSE_df$SS > 2000])), 2000)
idx2 <- as.numeric(L1H12_LSE_df$SS) > thresh
L1H12_LSE_df$Result[idx2]  <- "Nothing"; L1H12_LSE_df$SS[idx2] <- thresh


SS50_L1H12_LSE <- ceiling(quantile(as.numeric(L1H12_LSE_df$SS), probs = c(0.5)))
SS80_L1H12_LSE <- ceiling(quantile(as.numeric(L1H12_LSE_df$SS), probs = c(0.8)))
p_sup_L1H12_LSE <- nrow(L1H12_LSE_df[L1H12_LSE_df$Result == "superiority",])/nrow(L1H12_LSE_df)
p_fut_L1H12_LSE <- nrow(L1H12_LSE_df[L1H12_LSE_df$Result == "futility",])/nrow(L1H12_LSE_df)
p_no_L1H12_LSE <- nrow(L1H12_LSE_df[L1H12_LSE_df$Result == "Nothing",])/nrow(L1H12_LSE_df)

################ HSE ######################
L1H12_HSE <- L1H12[, c(1,3)]

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
  SS[i] <- L1H12_HSE[3 * (i - 1) + 1, 2]
  TxARM[i] <- L1H12_HSE[3 * (i - 1) + 2,2]
  Result[i] <- L1H12_HSE[3 * (i - 1) + 3,2]
}
L1H12_HSE_df <- data.frame(
  id = id,
  SS = as.numeric(SS),
  TxARM = TxARM,
  Result = Result
)

idx <- L1H12_HSE_df$TxARM == "UC" & L1H12_HSE_df$Result == "superiority"
L1H12_HSE_df$TxARM[idx] <- "DPL"
L1H12_HSE_df$Result[idx] <- "futility"

thresh <- ifelse(length(L1H12_HSE_df$SS[L1H12_HSE_df$SS > 2000]) != 0, min((L1H12_HSE_df$SS[L1H12_HSE_df$SS > 2000])), 2000)
idx2 <- as.numeric(L1H12_HSE_df$SS) > thresh
L1H12_HSE_df$Result[idx2]  <- "Nothing"; L1H12_HSE_df$SS[idx2] <- thresh


SS50_L1H12_HSE <- ceiling(quantile(as.numeric(L1H12_HSE_df$SS), probs = c(0.5)))
SS80_L1H12_HSE <- ceiling(quantile(as.numeric(L1H12_HSE_df$SS), probs = c(0.8)))
p_sup_L1H12_HSE <- nrow(L1H12_HSE_df[L1H12_HSE_df$Result == "superiority",])/nrow(L1H12_HSE_df)
p_fut_L1H12_HSE <- nrow(L1H12_HSE_df[L1H12_HSE_df$Result == "futility",])/nrow(L1H12_HSE_df)
p_no_L1H12_HSE <- nrow(L1H12_HSE_df[L1H12_HSE_df$Result == "Nothing",])/nrow(L1H12_HSE_df)

##############  L1H125  #####################

L1H125 <- as.data.frame(read.csv("L1H125.csv"))

################ LSE ######################
L1H125_LSE <- L1H125[, 1:2]

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
  SS[i] <- L1H125_LSE[3 * (i - 1) + 1, 2]
  TxARM[i] <- L1H125_LSE[3 * (i - 1) + 2,2]
  Result[i] <- L1H125_LSE[3 * (i - 1) + 3,2]
}
L1H125_LSE_df <- data.frame(
  id = id,
  SS = as.numeric(SS),
  TxARM = TxARM,
  Result = Result
)

idx <- L1H125_LSE_df$TxARM == "UC" & L1H125_LSE_df$Result == "superiority"
L1H125_LSE_df$TxARM[idx] <- "DPL"
L1H125_LSE_df$Result[idx] <- "futility"

thresh <- ifelse(length(L1H125_LSE_df$SS[L1H125_LSE_df$SS > 2000]) != 0, min((L1H125_LSE_df$SS[L1H125_LSE_df$SS > 2000])), 2000)
idx2 <- as.numeric(L1H125_LSE_df$SS) > thresh
L1H125_LSE_df$Result[idx2]  <- "Nothing"; L1H125_LSE_df$SS[idx2] <- thresh


SS50_L1H125_LSE <- ceiling(quantile(as.numeric(L1H125_LSE_df$SS), probs = c(0.5)))
SS80_L1H125_LSE <- ceiling(quantile(as.numeric(L1H125_LSE_df$SS), probs = c(0.8)))
p_sup_L1H125_LSE <- nrow(L1H125_LSE_df[L1H125_LSE_df$Result == "superiority",])/nrow(L1H125_LSE_df)
p_fut_L1H125_LSE <- nrow(L1H125_LSE_df[L1H125_LSE_df$Result == "futility",])/nrow(L1H125_LSE_df)
p_no_L1H125_LSE <- nrow(L1H125_LSE_df[L1H125_LSE_df$Result == "Nothing",])/nrow(L1H125_LSE_df)

################ HSE ######################
L1H125_HSE <- L1H125[, c(1,3)]

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
  SS[i] <- L1H125_HSE[3 * (i - 1) + 1, 2]
  TxARM[i] <- L1H125_HSE[3 * (i - 1) + 2,2]
  Result[i] <- L1H125_HSE[3 * (i - 1) + 3,2]
}
L1H125_HSE_df <- data.frame(
  id = id,
  SS = as.numeric(SS),
  TxARM = TxARM,
  Result = Result
)

idx <- L1H125_HSE_df$TxARM == "UC" & L1H125_HSE_df$Result == "superiority"
L1H125_HSE_df$TxARM[idx] <- "DPL"
L1H125_HSE_df$Result[idx] <- "futility"

thresh <- ifelse(length(L1H125_HSE_df$SS[L1H125_HSE_df$SS > 2000]) != 0, min((L1H125_HSE_df$SS[L1H125_HSE_df$SS > 2000])), 2000)
idx2 <- as.numeric(L1H125_HSE_df$SS) > thresh
L1H125_HSE_df$Result[idx2]  <- "Nothing"; L1H125_HSE_df$SS[idx2] <- thresh


SS50_L1H125_HSE <- ceiling(quantile(as.numeric(L1H125_HSE_df$SS), probs = c(0.5)))
SS80_L1H125_HSE <- ceiling(quantile(as.numeric(L1H125_HSE_df$SS), probs = c(0.8)))
p_sup_L1H125_HSE <- nrow(L1H125_HSE_df[L1H125_HSE_df$Result == "superiority",])/nrow(L1H125_HSE_df)
p_fut_L1H125_HSE <- nrow(L1H125_HSE_df[L1H125_HSE_df$Result == "futility",])/nrow(L1H125_HSE_df)
p_no_L1H125_HSE <- nrow(L1H125_HSE_df[L1H125_HSE_df$Result == "Nothing",])/nrow(L1H125_HSE_df)

##############  L1H13  #####################

L1H13 <- as.data.frame(read.csv("L1H13.csv"))

################ LSE ######################
L1H13_LSE <- L1H13[, 1:2]

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
  SS[i] <- L1H13_LSE[3 * (i - 1) + 1, 2]
  TxARM[i] <- L1H13_LSE[3 * (i - 1) + 2,2]
  Result[i] <- L1H13_LSE[3 * (i - 1) + 3,2]
}
L1H13_LSE_df <- data.frame(
  id = id,
  SS = as.numeric(SS),
  TxARM = TxARM,
  Result = Result
)

idx <- L1H13_LSE_df$TxARM == "UC" & L1H13_LSE_df$Result == "superiority"
L1H13_LSE_df$TxARM[idx] <- "DPL"
L1H13_LSE_df$Result[idx] <- "futility"

thresh <- ifelse(length(L1H13_LSE_df$SS[L1H13_LSE_df$SS > 2000]) != 0, min((L1H13_LSE_df$SS[L1H13_LSE_df$SS > 2000])), 2000)
idx2 <- as.numeric(L1H13_LSE_df$SS) > thresh
L1H13_LSE_df$Result[idx2]  <- "Nothing"; L1H13_LSE_df$SS[idx2] <- thresh



SS50_L1H13_LSE <- ceiling(quantile(as.numeric(L1H13_LSE_df$SS), probs = c(0.5)))
SS80_L1H13_LSE <- ceiling(quantile(as.numeric(L1H13_LSE_df$SS), probs = c(0.8)))
p_sup_L1H13_LSE <- nrow(L1H13_LSE_df[L1H13_LSE_df$Result == "superiority",])/nrow(L1H13_LSE_df)
p_fut_L1H13_LSE <- nrow(L1H13_LSE_df[L1H13_LSE_df$Result == "futility",])/nrow(L1H13_LSE_df)
p_no_L1H13_LSE <- nrow(L1H13_LSE_df[L1H13_LSE_df$Result == "Nothing",])/nrow(L1H13_LSE_df)

################ HSE ######################
L1H13_HSE <- L1H13[, c(1,3)]

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
  SS[i] <- L1H13_HSE[3 * (i - 1) + 1, 2]
  TxARM[i] <- L1H13_HSE[3 * (i - 1) + 2,2]
  Result[i] <- L1H13_HSE[3 * (i - 1) + 3,2]
}
L1H13_HSE_df <- data.frame(
  id = id,
  SS = as.numeric(SS),
  TxARM = TxARM,
  Result = Result
)

idx <- L1H13_HSE_df$TxARM == "UC" & L1H13_HSE_df$Result == "superiority"
L1H13_HSE_df$TxARM[idx] <- "DPL"
L1H13_HSE_df$Result[idx] <- "futility"

thresh <- ifelse(length(L1H13_HSE_df$SS[L1H13_HSE_df$SS > 2000]) != 0, min((L1H13_HSE_df$SS[L1H13_HSE_df$SS > 2000])), 2000)
idx2 <- as.numeric(L1H13_HSE_df$SS) > thresh
L1H13_HSE_df$Result[idx2]  <- "Nothing"; L1H13_HSE_df$SS[idx2] <- thresh

SS50_L1H13_HSE <- ceiling(quantile(as.numeric(L1H13_HSE_df$SS), probs = c(0.5)))
SS80_L1H13_HSE <- ceiling(quantile(as.numeric(L1H13_HSE_df$SS), probs = c(0.8)))
p_sup_L1H13_HSE <- nrow(L1H13_HSE_df[L1H13_HSE_df$Result == "superiority",])/nrow(L1H13_HSE_df)
p_fut_L1H13_HSE <- nrow(L1H13_HSE_df[L1H13_HSE_df$Result == "futility",])/nrow(L1H13_HSE_df)
p_no_L1H13_HSE <- nrow(L1H13_HSE_df[L1H13_HSE_df$Result == "Nothing",])/nrow(L1H13_HSE_df)

##############  L1H15  #####################

L1H15 <- as.data.frame(read.csv("L1H15.csv"))

################ LSE ######################
L1H15_LSE <- L1H15[, 1:2]

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
  SS[i] <- L1H15_LSE[3 * (i - 1) + 1, 2]
  TxARM[i] <- L1H15_LSE[3 * (i - 1) + 2,2]
  Result[i] <- L1H15_LSE[3 * (i - 1) + 3,2]
}
L1H15_LSE_df <- data.frame(
  id = id,
  SS = as.numeric(SS),
  TxARM = TxARM,
  Result = Result
)

idx <- L1H15_LSE_df$TxARM == "UC" & L1H15_LSE_df$Result == "superiority"
L1H15_LSE_df$TxARM[idx] <- "DPL"
L1H15_LSE_df$Result[idx] <- "futility"

thresh <- ifelse(length(L1H15_LSE_df$SS[L1H15_LSE_df$SS > 2000]) != 0, min((L1H15_LSE_df$SS[L1H15_LSE_df$SS > 2000])), 2000)
idx2 <- as.numeric(L1H15_LSE_df$SS) > thresh
L1H15_LSE_df$Result[idx2]  <- "Nothing"; L1H15_LSE_df$SS[idx2] <- thresh


SS50_L1H15_LSE <- ceiling(quantile(as.numeric(L1H15_LSE_df$SS), probs = c(0.5)))
SS80_L1H15_LSE <- ceiling(quantile(as.numeric(L1H15_LSE_df$SS), probs = c(0.8)))
p_sup_L1H15_LSE <- nrow(L1H15_LSE_df[L1H15_LSE_df$Result == "superiority",])/nrow(L1H15_LSE_df)
p_fut_L1H15_LSE <- nrow(L1H15_LSE_df[L1H15_LSE_df$Result == "futility",])/nrow(L1H15_LSE_df)
p_no_L1H15_LSE <- nrow(L1H15_LSE_df[L1H15_LSE_df$Result == "Nothing",])/nrow(L1H15_LSE_df)

################ HSE ######################
L1H15_HSE <- L1H15[, c(1,3)]

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
  SS[i] <- L1H15_HSE[3 * (i - 1) + 1, 2]
  TxARM[i] <- L1H15_HSE[3 * (i - 1) + 2,2]
  Result[i] <- L1H15_HSE[3 * (i - 1) + 3,2]
}
L1H15_HSE_df <- data.frame(
  id = id,
  SS = as.numeric(SS),
  TxARM = TxARM,
  Result = Result
)

idx <- L1H15_HSE_df$TxARM == "UC" & L1H15_HSE_df$Result == "superiority"
L1H15_HSE_df$TxARM[idx] <- "DPL"
L1H15_HSE_df$Result[idx] <- "futility"

thresh <- ifelse(length(L1H15_HSE_df$SS[L1H15_HSE_df$SS > 2000]) != 0, min((L1H15_HSE_df$SS[L1H15_HSE_df$SS > 2000])), 2000)
idx2 <- as.numeric(L1H15_HSE_df$SS) > thresh
L1H15_HSE_df$Result[idx2]  <- "Nothing"; L1H15_HSE_df$SS[idx2] <- thresh


SS50_L1H15_HSE <- ceiling(quantile(as.numeric(L1H15_HSE_df$SS), probs = c(0.5)))
SS80_L1H15_HSE <- ceiling(quantile(as.numeric(L1H15_HSE_df$SS), probs = c(0.8)))
p_sup_L1H15_HSE <- nrow(L1H15_HSE_df[L1H15_HSE_df$Result == "superiority",])/nrow(L1H15_HSE_df)
p_fut_L1H15_HSE <- nrow(L1H15_HSE_df[L1H15_HSE_df$Result == "futility",])/nrow(L1H15_HSE_df)
p_no_L1H15_HSE <- nrow(L1H15_HSE_df[L1H15_HSE_df$Result == "Nothing",])/nrow(L1H15_HSE_df)

##############  L08H08  #####################

L08H08 <- as.data.frame(read.csv("L08H08.csv"))

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

idx <- L08H08_LSE_df$TxARM == "UC" & L08H08_LSE_df$Result == "superiority"
L08H08_LSE_df$TxARM[idx] <- "DPL"
L08H08_LSE_df$Result[idx] <- "futility"

thresh <- ifelse(length(L08H08_LSE_df$SS[L08H08_LSE_df$SS > 2000]) != 0, min((L08H08_LSE_df$SS[L08H08_LSE_df$SS > 2000])), 2000)
idx2 <- as.numeric(L08H08_LSE_df$SS) > thresh
L08H08_LSE_df$Result[idx2]  <- "Nothing"; L08H08_LSE_df$SS[idx2] <- thresh

SS50_L08H08_LSE <- ceiling(quantile(as.numeric(L08H08_LSE_df$SS), probs = c(0.5)))
SS80_L08H08_LSE <- ceiling(quantile(as.numeric(L08H08_LSE_df$SS), probs = c(0.8)))
p_sup_L08H08_LSE <- nrow(L08H08_LSE_df[L08H08_LSE_df$Result == "superiority",])/nrow(L08H08_LSE_df)
p_fut_L08H08_LSE <- nrow(L08H08_LSE_df[L08H08_LSE_df$Result == "futility",])/nrow(L08H08_LSE_df)
p_no_L08H08_LSE <- nrow(L08H08_LSE_df[L08H08_LSE_df$Result == "Nothing",])/nrow(L08H08_LSE_df)

################ HSE ######################
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

idx <- L08H08_HSE_df$TxARM == "UC" & L08H08_HSE_df$Result == "superiority"
L08H08_HSE_df$TxARM[idx] <- "DPL"
L08H08_HSE_df$Result[idx] <- "futility"

thresh <- ifelse(length(L08H08_HSE_df$SS[L08H08_HSE_df$SS > 2000]) != 0, min((L08H08_HSE_df$SS[L08H08_HSE_df$SS > 2000])), 2000)
idx2 <- as.numeric(L08H08_HSE_df$SS) > thresh
L08H08_HSE_df$Result[idx2]  <- "Nothing"; L08H08_HSE_df$SS[idx2] <- thresh


SS50_L08H08_HSE <- ceiling(quantile(as.numeric(L08H08_HSE_df$SS), probs = c(0.5)))
SS80_L08H08_HSE <- ceiling(quantile(as.numeric(L08H08_HSE_df$SS), probs = c(0.8)))
p_sup_L08H08_HSE <- nrow(L08H08_HSE_df[L08H08_HSE_df$Result == "superiority",])/nrow(L08H08_HSE_df)
p_fut_L08H08_HSE <- nrow(L08H08_HSE_df[L08H08_HSE_df$Result == "futility",])/nrow(L08H08_HSE_df)
p_no_L08H08_HSE <- nrow(L08H08_HSE_df[L08H08_HSE_df$Result == "Nothing",])/nrow(L08H08_HSE_df)

##############  L11H11  #####################

L11H11 <- as.data.frame(read.csv("L11H11.csv"))

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

idx <- L11H11_LSE_df$TxARM == "UC" & L11H11_LSE_df$Result == "superiority"
L11H11_LSE_df$TxARM[idx] <- "DPL"
L11H11_LSE_df$Result[idx] <- "futility"

thresh <- ifelse(length(L11H11_LSE_df$SS[L11H11_LSE_df$SS > 2000]) != 0, min((L11H11_LSE_df$SS[L11H11_LSE_df$SS > 2000])), 2000)
idx2 <- as.numeric(L11H11_LSE_df$SS) > thresh
L11H11_LSE_df$Result[idx2]  <- "Nothing"; L11H11_LSE_df$SS[idx2] <- thresh


SS50_L11H11_LSE <- ceiling(quantile(as.numeric(L11H11_LSE_df$SS), probs = c(0.5)))
SS80_L11H11_LSE <- ceiling(quantile(as.numeric(L11H11_LSE_df$SS), probs = c(0.8)))
p_sup_L11H11_LSE <- nrow(L11H11_LSE_df[L11H11_LSE_df$Result == "superiority",])/nrow(L11H11_LSE_df)
p_fut_L11H11_LSE <- nrow(L11H11_LSE_df[L11H11_LSE_df$Result == "futility",])/nrow(L11H11_LSE_df)
p_no_L11H11_LSE <- nrow(L11H11_LSE_df[L11H11_LSE_df$Result == "Nothing",])/nrow(L11H11_LSE_df)

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

idx <- L11H11_HSE_df$TxARM == "UC" & L11H11_HSE_df$Result == "superiority"
L11H11_HSE_df$TxARM[idx] <- "DPL"
L11H11_HSE_df$Result[idx] <- "futility"

thresh <- ifelse(length(L11H11_HSE_df$SS[L11H11_HSE_df$SS > 2000]) != 0, min((L11H11_HSE_df$SS[L11H11_HSE_df$SS > 2000])), 2000)
idx2 <- as.numeric(L11H11_HSE_df$SS) > thresh
L11H11_HSE_df$Result[idx2]  <- "Nothing"; L11H11_HSE_df$SS[idx2] <- thresh


SS50_L11H11_HSE <- ceiling(quantile(as.numeric(L11H11_HSE_df$SS), probs = c(0.5)))
SS80_L11H11_HSE <- ceiling(quantile(as.numeric(L11H11_HSE_df$SS), probs = c(0.8)))
p_sup_L11H11_HSE <- nrow(L11H11_HSE_df[L11H11_HSE_df$Result == "superiority",])/nrow(L11H11_HSE_df)
p_fut_L11H11_HSE <- nrow(L11H11_HSE_df[L11H11_HSE_df$Result == "futility",])/nrow(L11H11_HSE_df)
p_no_L11H11_HSE <- nrow(L11H11_HSE_df[L11H11_HSE_df$Result == "Nothing",])/nrow(L11H11_HSE_df)

##############  L12H12  #####################

L12H12 <- as.data.frame(read.csv("L12H12.csv"))

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

idx <- L12H12_LSE_df$TxARM == "UC" & L12H12_LSE_df$Result == "superiority"
L12H12_LSE_df$TxARM[idx] <- "DPL"
L12H12_LSE_df$Result[idx] <- "futility"

thresh <- ifelse(length(L12H12_LSE_df$SS[L12H12_LSE_df$SS > 2000]) != 0, min((L12H12_LSE_df$SS[L12H12_LSE_df$SS > 2000])), 2000)
idx2 <- as.numeric(L12H12_LSE_df$SS) > thresh
L12H12_LSE_df$Result[idx2]  <- "Nothing"; L12H12_LSE_df$SS[idx2] <- thresh

SS50_L12H12_LSE <- ceiling(quantile(as.numeric(L12H12_LSE_df$SS), probs = c(0.5)))
SS80_L12H12_LSE <- ceiling(quantile(as.numeric(L12H12_LSE_df$SS), probs = c(0.8)))
p_sup_L12H12_LSE <- nrow(L12H12_LSE_df[L12H12_LSE_df$Result == "superiority",])/nrow(L12H12_LSE_df)
p_fut_L12H12_LSE <- nrow(L12H12_LSE_df[L12H12_LSE_df$Result == "futility",])/nrow(L12H12_LSE_df)
p_no_L12H12_LSE <- nrow(L12H12_LSE_df[L12H12_LSE_df$Result == "Nothing",])/nrow(L12H12_LSE_df)

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

idx <- L12H12_HSE_df$TxARM == "UC" & L12H12_HSE_df$Result == "superiority"
L12H12_HSE_df$TxARM[idx] <- "DPL"
L12H12_HSE_df$Result[idx] <- "futility"

thresh <- ifelse(length(L12H12_HSE_df$SS[L12H12_HSE_df$SS > 2000]) != 0, min((L12H12_HSE_df$SS[L12H12_HSE_df$SS > 2000])), 2000)
idx2 <- as.numeric(L12H12_HSE_df$SS) > thresh
L12H12_HSE_df$Result[idx2]  <- "Nothing"; L12H12_HSE_df$SS[idx2] <- thresh


SS50_L12H12_HSE <- ceiling(quantile(as.numeric(L12H12_HSE_df$SS), probs = c(0.5)))
SS80_L12H12_HSE <- ceiling(quantile(as.numeric(L12H12_HSE_df$SS), probs = c(0.8)))
p_sup_L12H12_HSE <- nrow(L12H12_HSE_df[L12H12_HSE_df$Result == "superiority",])/nrow(L12H12_HSE_df)
p_fut_L12H12_HSE <- nrow(L12H12_HSE_df[L12H12_HSE_df$Result == "futility",])/nrow(L12H12_HSE_df)
p_no_L12H12_HSE <- nrow(L12H12_HSE_df[L12H12_HSE_df$Result == "Nothing",])/nrow(L12H12_HSE_df)

##############  L125H125  #####################

L125H125 <- as.data.frame(read.csv("L125H125.csv"))

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

idx <- L125H125_LSE_df$TxARM == "UC" & L125H125_LSE_df$Result == "superiority"
L125H125_LSE_df$TxARM[idx] <- "DPL"
L125H125_LSE_df$Result[idx] <- "futility"

thresh <- ifelse(length(L125H125_LSE_df$SS[L125H125_LSE_df$SS > 2000]) != 0, min((L125H125_LSE_df$SS[L125H125_LSE_df$SS > 2000])), 2000)
idx2 <- as.numeric(L125H125_LSE_df$SS) > thresh
L125H125_LSE_df$Result[idx2]  <- "Nothing"; L125H125_LSE_df$SS[idx2] <- thresh


SS50_L125H125_LSE <- ceiling(quantile(as.numeric(L125H125_LSE_df$SS), probs = c(0.5)))
SS80_L125H125_LSE <- ceiling(quantile(as.numeric(L125H125_LSE_df$SS), probs = c(0.8)))
p_sup_L125H125_LSE <- nrow(L125H125_LSE_df[L125H125_LSE_df$Result == "superiority",])/nrow(L125H125_LSE_df)
p_fut_L125H125_LSE <- nrow(L125H125_LSE_df[L125H125_LSE_df$Result == "futility",])/nrow(L125H125_LSE_df)
p_no_L125H125_LSE <- nrow(L125H125_LSE_df[L125H125_LSE_df$Result == "Nothing",])/nrow(L125H125_LSE_df)

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

idx <- L125H125_HSE_df$TxARM == "UC" & L125H125_HSE_df$Result == "superiority"
L125H125_HSE_df$TxARM[idx] <- "DPL"
L125H125_HSE_df$Result[idx] <- "futility"

thresh <- ifelse(length(L125H125_HSE_df$SS[L125H125_HSE_df$SS > 2000]) != 0, min((L125H125_HSE_df$SS[L125H125_HSE_df$SS > 2000])), 2000)
idx2 <- as.numeric(L125H125_HSE_df$SS) > thresh
L125H125_HSE_df$Result[idx2]  <- "Nothing"; L125H125_HSE_df$SS[idx2] <- thresh


SS50_L125H125_HSE <- ceiling(quantile(as.numeric(L125H125_HSE_df$SS), probs = c(0.5)))
SS80_L125H125_HSE <- ceiling(quantile(as.numeric(L125H125_HSE_df$SS), probs = c(0.8)))
p_sup_L125H125_HSE <- nrow(L125H125_HSE_df[L125H125_HSE_df$Result == "superiority",])/nrow(L125H125_HSE_df)
p_fut_L125H125_HSE <- nrow(L125H125_HSE_df[L125H125_HSE_df$Result == "futility",])/nrow(L125H125_HSE_df)
p_no_L125H125_HSE <- nrow(L125H125_HSE_df[L125H125_HSE_df$Result == "Nothing",])/nrow(L125H125_HSE_df)

##############  L13H13  #####################

L13H13 <- as.data.frame(read.csv("L13H13.csv"))

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
  filter(!is.na(SS))
L13H13_LSE_df$TxARM[L13H13_LSE_df$TxARM == "UC" & L13H13_LSE_df$Result == "superiority"] <- "DPL"
L13H13_LSE_df$Result[L13H13_LSE_df$TxARM == "UC" & L13H13_LSE_df$Result == "superiority"] <- "futility"

idx <- L13H13_LSE_df$TxARM == "UC" & L13H13_LSE_df$Result == "superiority"
L13H13_LSE_df$TxARM[idx] <- "DPL"
L13H13_LSE_df$Result[idx] <- "futility"

thresh <- ifelse(length(L13H13_LSE_df$SS[L13H13_LSE_df$SS > 2000]) != 0, min((L13H13_LSE_df$SS[L13H13_LSE_df$SS > 2000])), 2000)
idx2 <- as.numeric(L13H13_LSE_df$SS) > thresh
L13H13_LSE_df$Result[idx2]  <- "Nothing"; L13H13_LSE_df$SS[idx2] <- thresh

SS50_L13H13_LSE <- ceiling(quantile(as.numeric(L13H13_LSE_df$SS), probs = c(0.5)))
SS80_L13H13_LSE <- ceiling(quantile(as.numeric(L13H13_LSE_df$SS), probs = c(0.8)))
p_sup_L13H13_LSE <- nrow(L13H13_LSE_df[L13H13_LSE_df$Result == "superiority",])/nrow(L13H13_LSE_df)
p_fut_L13H13_LSE <- nrow(L13H13_LSE_df[L13H13_LSE_df$Result == "futility",])/nrow(L13H13_LSE_df)
p_no_L13H13_LSE <- nrow(L13H13_LSE_df[L13H13_LSE_df$Result == "Nothing",])/nrow(L13H13_LSE_df)

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
  filter(!is.na(SS))
idx <- L13H13_HSE_df$TxARM == "UC" & L13H13_HSE_df$Result == "superiority"
L13H13_HSE_df$TxARM[idx] <- "DPL"
L13H13_HSE_df$Result[idx] <- "futility"

thresh <- ifelse(length(L13H13_HSE_df$SS[L13H13_HSE_df$SS > 2000]) != 0, min((L13H13_HSE_df$SS[L13H13_HSE_df$SS > 2000])), 2000)
idx2 <- as.numeric(L13H13_HSE_df$SS) > thresh
L13H13_HSE_df$Result[idx2]  <- "Nothing"; L13H13_HSE_df$SS[idx2] <- thresh

SS50_L13H13_HSE <- ceiling(quantile(as.numeric(L13H13_HSE_df$SS), probs = c(0.5)))
SS80_L13H13_HSE <- ceiling(quantile(as.numeric(L13H13_HSE_df$SS), probs = c(0.8)))
p_sup_L13H13_HSE <- nrow(L13H13_HSE_df[L13H13_HSE_df$Result == "superiority",])/nrow(L13H13_HSE_df)
p_fut_L13H13_HSE <- nrow(L13H13_HSE_df[L13H13_HSE_df$Result == "futility",])/nrow(L13H13_HSE_df)
p_no_L13H13_HSE <- nrow(L13H13_HSE_df[L13H13_HSE_df$Result == "Nothing",])/nrow(L13H13_HSE_df)

##############  L15H15  #####################

L15H15 <- as.data.frame(read.csv("L15H15.csv"))

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

idx <- L15H15_LSE_df$TxARM == "UC" & L15H15_LSE_df$Result == "superiority"
L15H15_LSE_df$TxARM[idx] <- "DPL"
L15H15_LSE_df$Result[idx] <- "futility"

thresh <- ifelse(length(L15H15_LSE_df$SS[L15H15_LSE_df$SS > 2000]) != 0, min((L15H15_LSE_df$SS[L15H15_LSE_df$SS > 2000])), 2000)
idx2 <- as.numeric(L15H15_LSE_df$SS) > thresh
L15H15_LSE_df$Result[idx2]  <- "Nothing"; L15H15_LSE_df$SS[idx2] <- thresh


SS50_L15H15_LSE <- ceiling(quantile(as.numeric(L15H15_LSE_df$SS), probs = c(0.5)))
SS80_L15H15_LSE <- ceiling(quantile(as.numeric(L15H15_LSE_df$SS), probs = c(0.8)))
p_sup_L15H15_LSE <- nrow(L15H15_LSE_df[L15H15_LSE_df$Result == "superiority",])/nrow(L15H15_LSE_df)
p_fut_L15H15_LSE <- nrow(L15H15_LSE_df[L15H15_LSE_df$Result == "futility",])/nrow(L15H15_LSE_df)
p_no_L15H15_LSE <- nrow(L15H15_LSE_df[L15H15_LSE_df$Result == "Nothing",])/nrow(L15H15_LSE_df)

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

idx <- L15H15_HSE_df$TxARM == "UC" & L15H15_HSE_df$Result == "superiority"
L15H15_HSE_df$TxARM[idx] <- "DPL"
L15H15_HSE_df$Result[idx] <- "futility"

thresh <- ifelse(length(L15H15_HSE_df$SS[L15H15_HSE_df$SS > 2000]) != 0, min((L15H15_HSE_df$SS[L15H15_HSE_df$SS > 2000])), 2000)
idx2 <- as.numeric(L15H15_HSE_df$SS) > thresh
L15H15_HSE_df$Result[idx2]  <- "Nothing"; L15H15_HSE_df$SS[idx2] <- thresh


SS50_L15H15_HSE <- ceiling(quantile(as.numeric(L15H15_HSE_df$SS), probs = c(0.5)))
SS80_L15H15_HSE <- ceiling(quantile(as.numeric(L15H15_HSE_df$SS), probs = c(0.8)))
p_sup_L15H15_HSE <- nrow(L15H15_HSE_df[L15H15_HSE_df$Result == "superiority",])/nrow(L15H15_HSE_df)
p_fut_L15H15_HSE <- nrow(L15H15_HSE_df[L15H15_HSE_df$Result == "futility",])/nrow(L15H15_HSE_df)
p_no_L15H15_HSE <- nrow(L15H15_HSE_df[L15H15_HSE_df$Result == "Nothing",])/nrow(L15H15_HSE_df)

##############  L11H13  #####################

L11H13 <- as.data.frame(read.csv("L11H13.csv"))

################ LSE ######################
L11H13_LSE <- L11H13[, 1:2]

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
  SS[i] <- L11H13_LSE[3 * (i - 1) + 1, 2]
  TxARM[i] <- L11H13_LSE[3 * (i - 1) + 2,2]
  Result[i] <- L11H13_LSE[3 * (i - 1) + 3,2]
}
L11H13_LSE_df <- data.frame(
  id = id,
  SS = as.numeric(SS),
  TxARM = TxARM,
  Result = Result
)

idx <- L11H13_LSE_df$TxARM == "UC" & L11H13_LSE_df$Result == "superiority"
L11H13_LSE_df$TxARM[idx] <- "DPL"
L11H13_LSE_df$Result[idx] <- "futility"

thresh <- ifelse(length(L11H13_LSE_df$SS[L11H13_LSE_df$SS > 2000]) != 0, min((L11H13_LSE_df$SS[L11H13_LSE_df$SS > 2000])), 2000)
idx2 <- as.numeric(L11H13_LSE_df$SS) > thresh
L11H13_LSE_df$Result[idx2]  <- "Nothing"; L11H13_LSE_df$SS[idx2] <- thresh


SS50_L11H13_LSE <- ceiling(quantile(as.numeric(L11H13_LSE_df$SS), probs = c(0.5)))
SS80_L11H13_LSE <- ceiling(quantile(as.numeric(L11H13_LSE_df$SS), probs = c(0.8)))
p_sup_L11H13_LSE <- nrow(L11H13_LSE_df[L11H13_LSE_df$Result == "superiority",])/nrow(L11H13_LSE_df)
p_fut_L11H13_LSE <- nrow(L11H13_LSE_df[L11H13_LSE_df$Result == "futility",])/nrow(L11H13_LSE_df)
p_no_L11H13_LSE <- nrow(L11H13_LSE_df[L11H13_LSE_df$Result == "Nothing",])/nrow(L11H13_LSE_df)

################ HSE ######################
L11H13_HSE <- L11H13[, c(1,3)]

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
  SS[i] <- L11H13_HSE[3 * (i - 1) + 1, 2]
  TxARM[i] <- L11H13_HSE[3 * (i - 1) + 2,2]
  Result[i] <- L11H13_HSE[3 * (i - 1) + 3,2]
}
L11H13_HSE_df <- data.frame(
  id = id,
  SS = as.numeric(SS),
  TxARM = TxARM,
  Result = Result
)


idx <- L11H13_HSE_df$TxARM == "UC" & L11H13_HSE_df$Result == "superiority"
L11H13_HSE_df$TxARM[idx] <- "DPL"
L11H13_HSE_df$Result[idx] <- "futility"

thresh <- ifelse(length(L11H13_HSE_df$SS[L11H13_HSE_df$SS > 2000]) != 0, min((L11H13_HSE_df$SS[L11H13_HSE_df$SS > 2000])), 2000)
idx2 <- as.numeric(L11H13_HSE_df$SS) > thresh
L11H13_HSE_df$Result[idx2]  <- "Nothing"; L11H13_HSE_df$SS[idx2] <- thresh


SS50_L11H13_HSE <- ceiling(quantile(as.numeric(L11H13_HSE_df$SS), probs = c(0.5)))
SS80_L11H13_HSE <- ceiling(quantile(as.numeric(L11H13_HSE_df$SS), probs = c(0.8)))
p_sup_L11H13_HSE <- nrow(L11H13_HSE_df[L11H13_HSE_df$Result == "superiority",])/nrow(L11H13_HSE_df)
p_fut_L11H13_HSE <- nrow(L11H13_HSE_df[L11H13_HSE_df$Result == "futility",])/nrow(L11H13_HSE_df)
p_no_L11H13_HSE <- nrow(L11H13_HSE_df[L11H13_HSE_df$Result == "Nothing",])/nrow(L11H13_HSE_df)

##############  L12H13  #####################

L12H13 <- as.data.frame(read.csv("L12H13.csv"))

################ LSE ######################
L12H13_LSE <- L12H13[, 1:2]

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
  SS[i] <- L12H13_LSE[3 * (i - 1) + 1, 2]
  TxARM[i] <- L12H13_LSE[3 * (i - 1) + 2,2]
  Result[i] <- L12H13_LSE[3 * (i - 1) + 3,2]
}
L12H13_LSE_df <- data.frame(
  id = id,
  SS = as.numeric(SS),
  TxARM = TxARM,
  Result = Result
)

idx <- L12H13_LSE_df$TxARM == "UC" & L12H13_LSE_df$Result == "superiority"
L12H13_LSE_df$TxARM[idx] <- "DPL"
L12H13_LSE_df$Result[idx] <- "futility"

thresh <- ifelse(length(L12H13_LSE_df$SS[L12H13_LSE_df$SS > 2000]) != 0, min((L12H13_LSE_df$SS[L12H13_LSE_df$SS > 2000])), 2000)
idx2 <- as.numeric(L12H13_LSE_df$SS) > thresh
L12H13_LSE_df$Result[idx2]  <- "Nothing"; L12H13_LSE_df$SS[idx2] <- thresh

SS50_L12H13_LSE <- ceiling(quantile(as.numeric(L12H13_LSE_df$SS), probs = c(0.5)))
SS80_L12H13_LSE <- ceiling(quantile(as.numeric(L12H13_LSE_df$SS), probs = c(0.8)))
p_sup_L12H13_LSE <- nrow(L12H13_LSE_df[L12H13_LSE_df$Result == "superiority",])/nrow(L12H13_LSE_df)
p_fut_L12H13_LSE <- nrow(L12H13_LSE_df[L12H13_LSE_df$Result == "futility",])/nrow(L12H13_LSE_df)
p_no_L12H13_LSE <- nrow(L12H13_LSE_df[L12H13_LSE_df$Result == "Nothing",])/nrow(L12H13_LSE_df)

################ HSE ######################
L12H13_HSE <- L12H13[, c(1,3)]

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
  SS[i] <- L12H13_HSE[3 * (i - 1) + 1, 2]
  TxARM[i] <- L12H13_HSE[3 * (i - 1) + 2,2]
  Result[i] <- L12H13_HSE[3 * (i - 1) + 3,2]
}
L12H13_HSE_df <- data.frame(
  id = id,
  SS = as.numeric(SS),
  TxARM = TxARM,
  Result = Result
)

idx <- L12H13_HSE_df$TxARM == "UC" & L12H13_HSE_df$Result == "superiority"
L12H13_HSE_df$TxARM[idx] <- "DPL"
L12H13_HSE_df$Result[idx] <- "futility"

thresh <- ifelse(length(L12H13_HSE_df$SS[L12H13_HSE_df$SS > 2000]) != 0, min((L12H13_HSE_df$SS[L12H13_HSE_df$SS > 2000])), 2000)
idx2 <- as.numeric(L12H13_HSE_df$SS) > thresh
L12H13_HSE_df$Result[idx2]  <- "Nothing"; L12H13_HSE_df$SS[idx2] <- thresh


SS50_L12H13_HSE <- ceiling(quantile(as.numeric(L12H13_HSE_df$SS), probs = c(0.5)))
SS80_L12H13_HSE <- ceiling(quantile(as.numeric(L12H13_HSE_df$SS), probs = c(0.8)))
p_sup_L12H13_HSE <- nrow(L12H13_HSE_df[L12H13_HSE_df$Result == "superiority",])/nrow(L12H13_HSE_df)
p_fut_L12H13_HSE <- nrow(L12H13_HSE_df[L12H13_HSE_df$Result == "futility",])/nrow(L12H13_HSE_df)
p_no_L12H13_HSE <- nrow(L12H13_HSE_df[L12H13_HSE_df$Result == "Nothing",])/nrow(L12H13_HSE_df)

##############  L13H11  #####################

L13H11 <- as.data.frame(read.csv("L13H11.csv"))

################ LSE ######################
L13H11_LSE <- L13H11[, 1:2]

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
  SS[i] <- L13H11_LSE[3 * (i - 1) + 1, 2]
  TxARM[i] <- L13H11_LSE[3 * (i - 1) + 2,2]
  Result[i] <- L13H11_LSE[3 * (i - 1) + 3,2]
}
L13H11_LSE_df <- data.frame(
  id = id,
  SS = as.numeric(SS),
  TxARM = TxARM,
  Result = Result
)

L13H11_LSE_df <- L13H11_LSE_df %>%
  filter(!is.na(SS))

idx <- L13H11_LSE_df$TxARM == "UC" & L13H11_LSE_df$Result == "superiority"
L13H11_LSE_df$TxARM[idx] <- "DPL"
L13H11_LSE_df$Result[idx] <- "futility"

thresh <- ifelse(length(L13H11_LSE_df$SS[L13H11_LSE_df$SS > 2000]) != 0, min((L13H11_LSE_df$SS[L13H11_LSE_df$SS > 2000])), 2000)
idx2 <- as.numeric(L13H11_LSE_df$SS) > thresh
L13H11_LSE_df$Result[idx2]  <- "Nothing"; L13H11_LSE_df$SS[idx2] <- thresh


SS50_L13H11_LSE <- ceiling(quantile(as.numeric(L13H11_LSE_df$SS), probs = c(0.5)))
SS80_L13H11_LSE <- ceiling(quantile(as.numeric(L13H11_LSE_df$SS), probs = c(0.8)))
p_sup_L13H11_LSE <- nrow(L13H11_LSE_df[L13H11_LSE_df$Result == "superiority",])/nrow(L13H11_LSE_df)
p_fut_L13H11_LSE <- nrow(L13H11_LSE_df[L13H11_LSE_df$Result == "futility",])/nrow(L13H11_LSE_df)
p_no_L13H11_LSE <- nrow(L13H11_LSE_df[L13H11_LSE_df$Result == "Nothing",])/nrow(L13H11_LSE_df)

################ HSE ######################
L13H11_HSE <- L13H11[, c(1,3)]

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
  SS[i] <- L13H11_HSE[3 * (i - 1) + 1, 2]
  TxARM[i] <- L13H11_HSE[3 * (i - 1) + 2,2]
  Result[i] <- L13H11_HSE[3 * (i - 1) + 3,2]
}
L13H11_HSE_df <- data.frame(
  id = id,
  SS = as.numeric(SS),
  TxARM = TxARM,
  Result = Result
)

L13H11_HSE_df <- L13H11_HSE_df %>%
  filter(!is.na(SS))

idx <- L13H11_HSE_df$TxARM == "UC" & L13H11_HSE_df$Result == "superiority"
L13H11_HSE_df$TxARM[idx] <- "DPL"
L13H11_HSE_df$Result[idx] <- "futility"

thresh <- ifelse(length(L13H11_HSE_df$SS[L13H11_HSE_df$SS > 2000]) != 0, min((L13H11_HSE_df$SS[L13H11_HSE_df$SS > 2000])), 2000)
idx2 <- as.numeric(L13H11_HSE_df$SS) > thresh
L13H11_HSE_df$Result[idx2]  <- "Nothing"; L13H11_HSE_df$SS[idx2] <- thresh


SS50_L13H11_HSE <- ceiling(quantile(as.numeric(L13H11_HSE_df$SS), probs = c(0.5)))
SS80_L13H11_HSE <- ceiling(quantile(as.numeric(L13H11_HSE_df$SS), probs = c(0.8)))
p_sup_L13H11_HSE <- nrow(L13H11_HSE_df[L13H11_HSE_df$Result == "superiority",])/nrow(L13H11_HSE_df)
p_fut_L13H11_HSE <- nrow(L13H11_HSE_df[L13H11_HSE_df$Result == "futility",])/nrow(L13H11_HSE_df)
p_no_L13H11_HSE <- nrow(L13H11_HSE_df[L13H11_HSE_df$Result == "Nothing",])/nrow(L13H11_HSE_df)

##############  L13H12  #####################

L13H12 <- as.data.frame(read.csv("L13H12.csv"))

################ LSE ######################
L13H12_LSE <- L13H12[, 1:2]

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
  SS[i] <- L13H12_LSE[3 * (i - 1) + 1, 2]
  TxARM[i] <- L13H12_LSE[3 * (i - 1) + 2,2]
  Result[i] <- L13H12_LSE[3 * (i - 1) + 3,2]
}
L13H12_LSE_df <- data.frame(
  id = id,
  SS = as.numeric(SS),
  TxARM = TxARM,
  Result = Result
)

idx <- L13H12_LSE_df$TxARM == "UC" & L13H12_LSE_df$Result == "superiority"
L13H12_LSE_df$TxARM[idx] <- "DPL"
L13H12_LSE_df$Result[idx] <- "futility"

thresh <- ifelse(length(L13H12_LSE_df$SS[L13H12_LSE_df$SS > 2000]) != 0, min((L13H12_LSE_df$SS[L13H12_LSE_df$SS > 2000])), 2000)
idx2 <- as.numeric(L13H12_LSE_df$SS) > thresh
L13H12_LSE_df$Result[idx2]  <- "Nothing"; L13H12_LSE_df$SS[idx2] <- thresh

SS50_L13H12_LSE <- ceiling(quantile(as.numeric(L13H12_LSE_df$SS), probs = c(0.5)))
SS80_L13H12_LSE <- ceiling(quantile(as.numeric(L13H12_LSE_df$SS), probs = c(0.8)))
p_sup_L13H12_LSE <- nrow(L13H12_LSE_df[L13H12_LSE_df$Result == "superiority",])/nrow(L13H12_LSE_df)
p_fut_L13H12_LSE <- nrow(L13H12_LSE_df[L13H12_LSE_df$Result == "futility",])/nrow(L13H12_LSE_df)
p_no_L13H12_LSE <- nrow(L13H12_LSE_df[L13H12_LSE_df$Result == "Nothing",])/nrow(L13H12_LSE_df)

################ HSE ######################
L13H12_HSE <- L13H12[, c(1,3)]

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
  SS[i] <- L13H12_HSE[3 * (i - 1) + 1, 2]
  TxARM[i] <- L13H12_HSE[3 * (i - 1) + 2,2]
  Result[i] <- L13H12_HSE[3 * (i - 1) + 3,2]
}
L13H12_HSE_df <- data.frame(
  id = id,
  SS = as.numeric(SS),
  TxARM = TxARM,
  Result = Result
)
idx <- L13H12_HSE_df$TxARM == "UC" & L13H12_HSE_df$Result == "superiority"
L13H12_HSE_df$TxARM[idx] <- "DPL"
L13H12_HSE_df$Result[idx] <- "futility"

thresh <- ifelse(length(L13H12_HSE_df$SS[L13H12_HSE_df$SS > 2000]) != 0, min((L13H12_HSE_df$SS[L13H12_HSE_df$SS > 2000])), 2000)
idx2 <- as.numeric(L13H12_HSE_df$SS) > thresh
L13H12_HSE_df$Result[idx2]  <- "Nothing"; L13H12_HSE_df$SS[idx2] <- thresh


SS50_L13H12_HSE <- ceiling(quantile(as.numeric(L13H12_HSE_df$SS), probs = c(0.5)))
SS80_L13H12_HSE <- ceiling(quantile(as.numeric(L13H12_HSE_df$SS), probs = c(0.8)))
p_sup_L13H12_HSE <- nrow(L13H12_HSE_df[L13H12_HSE_df$Result == "superiority",])/nrow(L13H12_HSE_df)
p_fut_L13H12_HSE <- nrow(L13H12_HSE_df[L13H12_HSE_df$Result == "futility",])/nrow(L13H12_HSE_df)
p_no_L13H12_HSE <- nrow(L13H12_HSE_df[L13H12_HSE_df$Result == "Nothing",])/nrow(L13H12_HSE_df)

##############  L1H1  #####################

L1H1 <- as.data.frame(read.csv("C:/Users/Ziming Chen/OneDrive - SickKids/DRIVE/Simulation/OR1/n500/sup0.9925/OR1_n500_sup9925_fut95.csv"))

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
idx <- L1H1_LSE_df$TxARM == "UC" & L1H1_LSE_df$Result == "superiority"
L1H1_LSE_df$TxARM[idx] <- "DPL"
L1H1_LSE_df$Result[idx] <- "futility"

thresh <- ifelse(length(L1H1_LSE_df$SS[L1H1_LSE_df$SS > 2000]) != 0, min((L1H1_LSE_df$SS[L1H1_LSE_df$SS > 2000])), 2000)
idx2 <- as.numeric(L1H1_LSE_df$SS) > thresh
L1H1_LSE_df$Result[idx2]  <- "Nothing"; L1H1_LSE_df$SS[idx2] <- thresh

SS50_L1H1_LSE <- ceiling(quantile(as.numeric(L1H1_LSE_df$SS), probs = c(0.5)))
SS80_L1H1_LSE <- ceiling(quantile(as.numeric(L1H1_LSE_df$SS), probs = c(0.8)))
p_sup_L1H1_LSE <- nrow(L1H1_LSE_df[L1H1_LSE_df$Result == "superiority",])/nrow(L1H1_LSE_df)
p_fut_L1H1_LSE <- nrow(L1H1_LSE_df[L1H1_LSE_df$Result == "futility",])/nrow(L1H1_LSE_df)
p_no_L1H1_LSE <- nrow(L1H1_LSE_df[L1H1_LSE_df$Result == "Nothing",])/nrow(L1H1_LSE_df)

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

idx <- L1H1_HSE_df$TxARM == "UC" & L1H1_HSE_df$Result == "superiority"
L1H1_HSE_df$TxARM[idx] <- "DPL"
L1H1_HSE_df$Result[idx] <- "futility"

thresh <- ifelse(length(L1H1_HSE_df$SS[L1H1_HSE_df$SS > 2000]) != 0, min((L1H1_HSE_df$SS[L1H1_HSE_df$SS > 2000])), 2000)
idx2 <- as.numeric(L1H1_HSE_df$SS) > thresh
L1H1_HSE_df$Result[idx2]  <- "Nothing"; L1H1_HSE_df$SS[idx2] <- thresh

SS50_L1H1_HSE <- ceiling(quantile(as.numeric(L1H1_HSE_df$SS), probs = c(0.5)))
SS80_L1H1_HSE <- ceiling(quantile(as.numeric(L1H1_HSE_df$SS), probs = c(0.8)))
p_sup_L1H1_HSE <- nrow(L1H1_HSE_df[L1H1_HSE_df$Result == "superiority",])/nrow(L1H1_HSE_df)
p_fut_L1H1_HSE <- nrow(L1H1_HSE_df[L1H1_HSE_df$Result == "futility",])/nrow(L1H1_HSE_df)
p_no_L1H1_HSE <- nrow(L1H1_HSE_df[L1H1_HSE_df$Result == "Nothing",])/nrow(L1H1_HSE_df)

OR <- c(1, 0.8,
        1, 1,
        1, 1.1,
        1, 1.2,
        1, 1.25,
        1, 1.3,
        1, 1.5,
        0.8, 1,
        1.1, 1,
        1.2, 1,
        1.25, 1,
        1.3, 1,
        1.5, 1,
        0.8, 0.8,
        1.1, 1.1,
        1.2, 1.2,
        1.25, 1.25,
        1.3, 1.3,
        1.5, 1.5,
        1.1, 1.3,
        1.2, 1.3,
        1.3, 1.1,
        1.3, 1.2)
state <- rep(c("LSE", "HSE"), 23)
p_sup <- c(p_sup_L1H08_LSE, p_sup_L1H08_HSE,
           p_sup_L1H1_LSE, p_sup_L1H1_HSE,
           p_sup_L1H11_LSE, p_sup_L1H11_HSE,
           p_sup_L1H12_LSE, p_sup_L1H12_HSE,
           p_sup_L1H125_LSE, p_sup_L1H125_HSE,
           p_sup_L1H13_LSE, p_sup_L1H13_HSE,
           p_sup_L1H15_LSE, p_sup_L1H15_HSE,
           p_sup_L08H1_LSE, p_sup_L08H1_HSE,
           p_sup_L11H1_LSE, p_sup_L11H1_HSE,
           p_sup_L12H1_LSE, p_sup_L12H1_HSE,
           p_sup_L125H1_LSE, p_sup_L125H1_HSE,
           p_sup_L13H1_LSE, p_sup_L13H1_HSE,
           p_sup_L15H1_LSE, p_sup_L15H1_HSE,
           p_sup_L08H08_LSE, p_sup_L08H08_HSE,
           p_sup_L11H11_LSE, p_sup_L11H11_HSE,
           p_sup_L12H12_LSE, p_sup_L12H12_HSE,
           p_sup_L125H125_LSE, p_sup_L125H125_HSE,
           p_sup_L13H13_LSE, p_sup_L13H13_HSE,
           p_sup_L15H15_LSE, p_sup_L15H15_HSE,
           p_sup_L11H13_LSE, p_sup_L11H13_HSE,
           p_sup_L12H13_LSE, p_sup_L12H13_HSE,
           p_sup_L13H11_LSE, p_sup_L13H11_HSE,
           p_sup_L13H12_LSE, p_sup_L13H12_HSE)

p_fut <- c(p_fut_L1H08_LSE, p_fut_L1H08_HSE,
           p_fut_L1H1_LSE, p_fut_L1H1_HSE,
           p_fut_L1H11_LSE, p_fut_L1H11_HSE,
           p_fut_L1H12_LSE, p_fut_L1H12_HSE,
           p_fut_L1H125_LSE, p_fut_L1H125_HSE,
           p_fut_L1H13_LSE, p_fut_L1H13_HSE,
           p_fut_L1H15_LSE, p_fut_L1H15_HSE,
           p_fut_L08H1_LSE, p_fut_L08H1_HSE,
           p_fut_L11H1_LSE, p_fut_L11H1_HSE,
           p_fut_L12H1_LSE, p_fut_L12H1_HSE,
           p_fut_L125H1_LSE, p_fut_L125H1_HSE,
           p_fut_L13H1_LSE, p_fut_L13H1_HSE,
           p_fut_L15H1_LSE, p_fut_L15H1_HSE,
           p_fut_L08H08_LSE, p_fut_L08H08_HSE,
           p_fut_L11H11_LSE, p_fut_L11H11_HSE,
           p_fut_L12H12_LSE, p_fut_L12H12_HSE,
           p_fut_L125H125_LSE, p_fut_L125H125_HSE,
           p_fut_L13H13_LSE, p_fut_L13H13_HSE,
           p_fut_L15H15_LSE, p_fut_L15H15_HSE,
           p_fut_L11H13_LSE, p_fut_L11H13_HSE,
           p_fut_L12H13_LSE, p_fut_L12H13_HSE,
           p_fut_L13H11_LSE, p_fut_L13H11_HSE,
           p_fut_L13H12_LSE, p_fut_L13H12_HSE)

p_nothing <- c(p_no_L1H08_LSE, p_no_L1H08_HSE,
               p_no_L1H1_LSE, p_no_L1H1_HSE,
               p_no_L1H11_LSE, p_no_L1H11_HSE,
               p_no_L1H12_LSE, p_no_L1H12_HSE,
               p_no_L1H125_LSE, p_no_L1H125_HSE,
               p_no_L1H13_LSE, p_no_L1H13_HSE,
               p_no_L1H15_LSE, p_no_L1H15_HSE,
               p_no_L08H1_LSE, p_no_L08H1_HSE,
               p_no_L11H1_LSE, p_no_L11H1_HSE,
               p_no_L12H1_LSE, p_no_L12H1_HSE,
               p_no_L125H1_LSE, p_no_L125H1_HSE,
               p_no_L13H1_LSE, p_no_L13H1_HSE,
               p_no_L15H1_LSE, p_no_L15H1_HSE,
               p_no_L08H08_LSE, p_no_L08H08_HSE,
               p_no_L11H11_LSE, p_no_L11H11_HSE,
               p_no_L12H12_LSE, p_no_L12H12_HSE,
               p_no_L125H125_LSE, p_no_L125H125_HSE,
               p_no_L13H13_LSE, p_no_L13H13_HSE,
               p_no_L15H15_LSE, p_no_L15H15_HSE,
               p_no_L11H13_LSE, p_no_L11H13_HSE,
               p_no_L12H13_LSE, p_no_L12H13_HSE,
               p_no_L13H11_LSE, p_no_L13H11_HSE,
               p_no_L13H12_LSE, p_no_L13H12_HSE)

SS50 <- c(SS50_L1H08_LSE, SS50_L1H08_HSE,
          SS50_L1H1_LSE, SS50_L1H1_HSE,
          SS50_L1H11_LSE, SS50_L1H11_HSE,
          SS50_L1H12_LSE, SS50_L1H12_HSE,
          SS50_L1H125_LSE, SS50_L1H125_HSE,
          SS50_L1H13_LSE, SS50_L1H13_HSE,
          SS50_L1H15_LSE, SS50_L1H15_HSE,
          SS50_L08H1_LSE, SS50_L08H1_HSE,
          SS50_L11H1_LSE, SS50_L11H1_HSE,
          SS50_L12H1_LSE, SS50_L12H1_HSE,
          SS50_L125H1_LSE, SS50_L125H1_HSE,
          SS50_L13H1_LSE, SS50_L13H1_HSE,
          SS50_L15H1_LSE, SS50_L15H1_HSE,
          SS50_L08H08_LSE, SS50_L08H08_HSE,
          SS50_L11H11_LSE, SS50_L11H11_HSE,
          SS50_L12H12_LSE, SS50_L12H12_HSE,
          SS50_L125H125_LSE, SS50_L125H125_HSE,
          SS50_L13H13_LSE, SS50_L13H13_HSE,
          SS50_L15H15_LSE, SS50_L15H15_HSE,
          SS50_L11H13_LSE, SS50_L11H13_HSE,
          SS50_L12H13_LSE, SS50_L12H13_HSE,
          SS50_L13H11_LSE, SS50_L13H11_HSE,
          SS50_L13H12_LSE, SS50_L13H12_HSE)

SS80 <- c(SS80_L1H08_LSE, SS80_L1H08_HSE,
          SS80_L1H1_LSE, SS80_L1H1_HSE,
          SS80_L1H11_LSE, SS80_L1H11_HSE,
          SS80_L1H12_LSE, SS80_L1H12_HSE,
          SS80_L1H125_LSE, SS80_L1H125_HSE,
          SS80_L1H13_LSE, SS80_L1H13_HSE,
          SS80_L1H15_LSE, SS80_L1H15_HSE,
          SS80_L08H1_LSE, SS80_L08H1_HSE,
          SS80_L11H1_LSE, SS80_L11H1_HSE,
          SS80_L12H1_LSE, SS80_L12H1_HSE,
          SS80_L125H1_LSE, SS80_L125H1_HSE,
          SS80_L13H1_LSE, SS80_L13H1_HSE,
          SS80_L15H1_LSE, SS80_L15H1_HSE,
          SS80_L08H08_LSE, SS80_L08H08_HSE,
          SS80_L11H11_LSE, SS80_L11H11_HSE,
          SS80_L12H12_LSE, SS80_L12H12_HSE,
          SS80_L125H125_LSE, SS80_L125H125_HSE,
          SS80_L13H13_LSE, SS80_L13H13_HSE,
          SS80_L15H15_LSE, SS80_L15H15_HSE,
          SS80_L11H13_LSE, SS80_L11H13_HSE,
          SS80_L12H13_LSE, SS80_L12H13_HSE,
          SS80_L13H11_LSE, SS80_L13H11_HSE,
          SS80_L13H12_LSE, SS80_L13H12_HSE)

results5 <- data.frame(OR,
                       state,
                       p_sup,
                       p_fut,
                       p_nothing,
                       SS50,
                       SS80)

results_sub <- results5[c(3, 4, seq(27, 38, 1)),]
results_sub_long <- gather(results_sub, end_point, magnitude, p_sup:p_nothing, factor_key = TRUE)

results_sub_long <- results_sub_long %>%
  mutate(design = "Design1")


write.csv(results_sub, "C:/Users/Ziming Chen/OneDrive - SickKids/DRIVE/Simulation/Cross Scenarios2/design1.csv")

write.csv(results_sub_long, "C:/Users/Ziming Chen/OneDrive - SickKids/DRIVE/Simulation/Cross Scenarios2/design1_long.csv")

#Note that, this is only for Design 1 that was chosen in the study
#Do the same for the Design 2 and combine the results
#You can plot Figure2 using the code below:
#Here, results are the results_sub_long for the both designs combined
results <- results %>%
  mutate(state = ifelse(state == "HSE", "High Elastance", "Low Elastance"),
         end_point = factor(as.factor(end_point), levels = c("p_sup", "p_nothing", "p_fut")))
percentage_plot_final <- results %>%
  ggplot() + 
  geom_col(aes(x = factor(OR), y = magnitude, fill = end_point), position = position_stack())  + 
  scale_y_continuous(labels=scales::percent) +
  labs(x = "Odds Ratio", y = "Probability", fill = "") +
  scale_fill_grey(labels=c("Superiority", "No Trigger", "Futility")) +
  theme(panel.grid.major = element_blank()) +
  facet_wrap(design ~ state) +
  theme_bw()