library('boot')
library('dplyr')
library('INLA')
library('tidyverse')
library(doParallel)
library(foreach)

inla.setOption(num.threads = 8)

#read the file for all the probabilities, odds ratios used in the simulation for different states
pdf_outcome1 <- readRDS(file = "/home/wteng/Jocelyn/DynamicBorrowing/VFDS_pdf.rds")

OR_DPL = c(1, 1) #alter this for different treatment effect odds ratio settings
TxARM_n <- c("DPL","UC")

N_patient <- 5000 #alter this for different maximum sample size settings
burn.in <- 800 #alter this for different initial enrolment before analysis settings
prob_sup <- 0.995 #alter this for the superiority threshold
prob_futi <- 0.9 #alter this for the futility threshold
treat_futi <- "DPL" 
states <- c("LSE", "HSE")
recruit_rate <- cbind(126, 64); colnames(recruit_rate) <- states
pdf_outcome <- c()
for (i in 1:length(states)){
  M <- as.data.frame(pdf_outcome1[,,i]) %>%
    mutate(state = states[i])
  pdf_outcome <- rbind(pdf_outcome ,M)
}

VFDs <- unique(pdf_outcome["VFDs"])
VFDs <- as.numeric(VFDs[,1])

p_UC_LSE <- as.numeric(pdf_outcome[pdf_outcome$state == 'LSE', "p_prob"])
p_UC_HSE <- as.numeric(pdf_outcome[pdf_outcome$state == 'HSE', "p_prob"])
p_DPL_LSE <- as.numeric(pdf_outcome[pdf_outcome$state == 'LSE', as.character(OR_DPL[1])])
p_DPL_HSE <- as.numeric(pdf_outcome[pdf_outcome$state == 'HSE', as.character(OR_DPL[2])])

DB_simulation <- function(N_patient, 
                          burn.in, 
                          prob_sup, 
                          prob_futi,
                          treat_futi,
                          OR_DPL, 
                          recruit_rate, 
                          p_UC_LSE,
                          p_UC_HSE,
                          p_DPL_LSE,
                          p_DPL_HSE){
  Result = array(dim = c(3, length(states)),
                 dimnames = list(c("Sample.size", "TxARM", "Result"), states))
  there.has.been.an.error <- FALSE
  n_LSE <- recruit_rate[,"LSE"]
  n_HSE <- recruit_rate[,"HSE"]
  n_state <- cbind(n_LSE , n_HSE); colnames(n_state) <- states
  np <- n <- num_patient <- sum(n_state)
  TxARM <- factor(sample(TxARM_n , n, replace = TRUE))
  State <- factor(sample(states , n, replace = TRUE, prob = c(n_LSE/n, n_HSE/n)))
  n_state <- cbind(sum(State == "LSE"),
                   sum(State == "HSE")); colnames(n_state) <- states
  
  outcome <- vector("numeric", length(TxARM))
  outcome[TxARM == "UC" & State == "LSE"] <- sample(VFDs, sum(TxARM == "UC"& State == "LSE"), replace = TRUE, prob = p_UC_LSE)
  outcome[TxARM == "UC" & State == "HSE"] <- sample(VFDs, sum(TxARM == "UC" & State == "HSE"), replace = TRUE, prob = p_UC_HSE)
  outcome[TxARM == "DPL"& State == "LSE"] <- sample(VFDs, sum(TxARM == "DPL"& State == "LSE"), replace = TRUE, prob = p_DPL_LSE)
  outcome[TxARM == "DPL"& State == "HSE"] <- sample(VFDs, sum(TxARM == "DPL"& State == "HSE"), replace = TRUE, prob = p_DPL_HSE)
  sample_data <- data.frame(outcome, TxARM, State)
  ref = "UC"
  sample_data$TxARM = relevel(sample_data$TxARM, ref = ref)
  ind <- matrix(0, 1, length(states)); colnames(ind) <- states
  
  #----half t prior-----------------------------------
  #https://becarioprecario.bitbucket.io/inla-gitbook/ch-priors.html#sec:priors
  HT.prior = "expression:
  sigma = exp(-theta/2);
  nu = 3;
  tau = 7;
  log_dens = 0 - 0.5 * log(nu * pi) - (-0.1207822);
  log_dens = log_dens - 0.5 * (nu + 1) * log(1 + (sigma * sigma) / (nu * (tau * tau)));
  log_dens = log_dens - log(2) - theta / 2;
  return(log_dens);
  "
  h.t = list(theta = list(prior = HT.prior))
  
  #list(theta=list(prior="loggamma", param=c(alpha, beta)))
  
  lc <- inla.make.lincombs(State = matrix(c(1,0, 0, 1), nrow = 2), TxARMDPL = c(1, 1), StateLSE = c(0, 0))
  
  while (TRUE) {
    if(sum((n_state > burn.in) * (ind == 0)) >= 1){
      if(length(unique(sample_data$outcome)) != 9){
        
        sample_data_adj <- sample_data
        sample_data_adj$outcome <- as.numeric(factor(sample_data$outcome))
        tryCatch(
          {
            model <- inla(outcome ~  State + TxARM + f(State, TxARM, model = "iid"
                                                       , hyper = h.t,
                                                       constr = TRUE),
                          data = sample_data_adj,
                          lincomb = lc,
                          family = "pom",  
                          verbose=FALSE, control.compute = list(config=TRUE),
                          control.family = list(hyper = list(theta1 = list(param = 100))))
          },
          error = function(e){
            print("Error in INLA")
            there.has.been.an.error <<- TRUE
          }
        )
      }
      if(length(unique(sample_data$outcome)) == 9){
        
        tryCatch(
          {
            model <- inla(outcome ~  State + TxARM + f(State, TxARM, model = "iid"
                                                       , hyper = h.t,
                                                       constr = TRUE),
                          data = sample_data,
                          lincomb = lc,
                          family = "pom",  
                          verbose=FALSE, control.compute = list(config=TRUE),
                          control.family = list(hyper = list(theta1 = list(param = 100))))
          },
          error = function(e){
            print("Error in INLA")
            there.has.been.an.error <<- TRUE
          }
        )
      }
      
      if(!there.has.been.an.error){
        m0 <- model$marginals.lincomb.derived
        marg_LSE <- m0$lc2
        marg_HSE <- m0$lc1
        rm(model)
        Prob.differ.futi <- cbind("HSE" =inla.pmarginal(log(1.2), marg_HSE), "LSE" = inla.pmarginal(log(1.2), marg_LSE))
        post.p <- cbind("treat.opt" = TxARM_n,
                        "HSE" = c(1 - inla.pmarginal(log(1), marg_HSE), inla.pmarginal(log(1), marg_HSE)),
                        "LSE" = c(1 - inla.pmarginal(log(1), marg_LSE), inla.pmarginal(log(1), marg_LSE)))
        
        for (j in 1:length(states)){
          n.state <- as.vector(n_state[, states[j]]) 
          ind.state <- as.vector(ind[,states[j]])
          
          if (n.state > burn.in){
            if (ind.state == 0) {
              probs <- as.numeric(post.p[, states[j]])
              treatments <- post.p[, "treat.opt"]
              
              if  (sum(probs > prob_sup) > 0) {
                treat <- treatments[which(probs > prob_sup)] 
                probs <- probs[which(probs > prob_sup)]
                Result["Sample.size", j] = n.state
                Result["Result", j] = "superiority" 
                Result["TxARM", j]  = treat
                ind[,states[j]] = 1
              }
              if  (sum(probs > prob_sup) == 0) {
                if  (Prob.differ.futi[, states[j]] > prob_futi) {
                  Result["Sample.size", j] = n.state
                  Result["Result", j] = "futility" 
                  Result["TxARM", j]  =  treat_futi
                  ind[,states[j]] = 1
                }
                else
                {Result["Sample.size", j] = n.state
                Result["Result", j] = "Nothing" 
                Result["TxARM", j]  = "Nothing"
                }  
              }
              if (n.state >= N_patient){
                Result["Sample.size", j] = n.state
                Result["Result", j] = "Nothing" 
                Result["TxARM", j]  = "Nothing"
                ind[,states[j]] = 1
              }  
            }
          } 
          else { 
            Result["Sample.size", j] = n.state
            Result["Result", j] = "NA" 
            Result["TxARM", j]  = "NA"
          }
        }
      }
    }
    n_new <- cbind(0 , 0); colnames(n_new) <- states
    for (j in 1:length(states)){ 
      if (ind[,states[j]] == 1){
        n_new[, states[j]] <- 0
      }
      else {
        n_new[, states[j]] <- recruit_rate[, states[j]]  
      } 
    }
    n_LSE <- as.vector(n_state[, "LSE"])  
    n_HSE <- as.vector(n_state[, "HSE"])
    new_LSE <- as.vector(n_new[, "LSE"])  
    new_HSE <- as.vector(n_new[, "HSE"])
    n <- sum(n_new)
    if (sum(n_state >= N_patient) == 2 | n == 0){break}
    TxARM <- factor(sample(TxARM_n , n, replace = TRUE))
    State <- factor(sample(states , n, replace = TRUE, prob = c(new_LSE/n, new_HSE/n)))
    outcome <- vector("numeric", length(TxARM))
    outcome[TxARM == "UC" & State == "LSE"] <- sample(VFDs, sum(TxARM == "UC"& State == "LSE"), replace = TRUE, prob = p_UC_LSE)
    outcome[TxARM == "UC" & State == "HSE"] <- sample(VFDs, sum(TxARM == "UC" & State == "HSE"), replace = TRUE, prob = p_UC_HSE)
    outcome[TxARM == "DPL"& State == "LSE"] <- sample(VFDs, sum(TxARM == "DPL"& State == "LSE"), replace = TRUE, prob = p_DPL_LSE)
    outcome[TxARM == "DPL"& State == "HSE"] <- sample(VFDs, sum(TxARM == "DPL"& State == "HSE"), replace = TRUE, prob = p_DPL_HSE)
    new_data <- data.frame(outcome, TxARM, State)
    sample_data <- rbind(sample_data, new_data)
    sample_data$TxARM = relevel(sample_data$TxARM, ref = ref)
    n_state <- cbind(sum(sample_data$State == "LSE"),
                     sum(sample_data$State == "HSE")); colnames(n_state) <- states
    np <- num_patient <- nrow(sample_data)
    if(there.has.been.an.error){break}
  }
  return(Result)
}

#parallel computing code
n_sim <- 1000
ncores <- detectCores() - 1
cl <- makeCluster(ncores)
registerDoParallel(cl)
Sim_Res <- foreach(k = 1:n_sim, .combine = rbind,
                   .packages = c("boot", "dplyr", "tidyverse", "INLA")
) %dopar% { 
  set.seed(k + 123)
  DB_simulation(N_patient = N_patient, 
                burn.in = burn.in, 
                prob_sup = prob_sup, 
                prob_futi = prob_futi,
                treat_futi = treat_futi,
                OR_DPL = OR_DPL, 
                recruit_rate = recruit_rate, 
                p_UC_LSE = p_UC_LSE,
                p_UC_HSE = p_UC_HSE,
                p_DPL_LSE = p_DPL_LSE,
                p_DPL_HSE = p_DPL_HSE)     
}
stopCluster(cl)

results <- as.data.frame(Sim_Res)

write.csv(results, file = "OR1_n800_sup995_fut90.csv") 
