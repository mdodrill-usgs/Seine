###############################################################################
#                                                                        Aug 18
#           JAGS Model Fitting - 2 Stage MultSp Removal Model 
#
#  Notes:
#  *
#
#  To Do:
#  * Use fishR to query/import SN data -- see 'SN_Data_Working.R'
#  * Look into broom/tidy methods for output...tidybayes
#
###############################################################################
# setwd('C:/Users/mdodrill/Desktop/Fish_Git/Seine/Data/')
setwd(paste0(getwd(), "/Data"))
# rm(list = ls(all = TRUE))
library(R2jags)


#-----------------------------------------------------------------------------#
# Removal
dr = read.table(file = "CatchData_SN_BKW_40_100_NEW_ALL_SP.txt", header = T)
names(dr) = tolower(names(dr))

dsub_r = dr

o_r=order(dsub_r$species)   
d1_r = dsub_r[o_r,]
d1_r

#-----------------------------------------------------------------------------#
Nsp = length(unique(dr$species))

catch = as.matrix(d1_r[,5:15])
dim(catch)

Npass_r = as.numeric(d1_r$n_pass)
Nsamps_r = dim(catch)[1]

sp = as.numeric(d1_r$species)

tmp = droplevels(d1_r)

#-----------------------------------------------------------------------------#
# cat = read.csv(file = "SN_2016_data.csv", header = T, stringsAsFactors = F)
# 
# 
# # take out the species subset here... done in data step
# u.sp = as.character(unique(tmp$species))
# 
# sub.cat = cat[which(cat$species_code %in% u.sp),]
# 
# cat.2 = sub.cat[order(as.factor(sub.cat$species_code)),]

#--------------
cat.2 = cat[order(as.factor(cat$species_code)),]

sp2 = as.numeric(as.factor(cat.2$species_code))

Nsamps_cat = dim(cat.2)[1]

catch2 = cat.2$tot.catch


#-----------------------------------------------------------------------------#
sink("JAGS_working_test.jags")
cat("
model{
  
  for(i in 1:Nsamps_r){
    beta[i] ~ dnorm(0, sigma)
  }
  
  sigma <- pow(tau, -2)
  tau ~ dunif(0, 2)
  
  for(i in 1:Nsp){
    mean.p[i] ~ dunif(0, 1)                        # Prior for mean
    mu.p[i] <- log(mean.p[i] / (1 - mean.p[i]))    # Logit transformation 
  }
  
  for(i in 1:Nsp){
    sigma.p[i] ~ dunif(0, 10)                      # Prior for standard deviation
    tau.p[i] <- pow(sigma.p[i], -2)
  }
  
  for(i in 1:Nsamps_r){
    eps.p[i] ~ dnorm(0, tau.p[sp[i]])
  }
  
  for(i in 1:Nsamps_r){
    # p[i] ~ dunif(0,1)
    
    logit(p[i]) <- mu.p[sp[i]] + eps.p[i]
  }
  
  for(i in 1:Nsamps_r){
    catch[i,1] ~ dbin(p[i], N[i])
    z[i,1] <- N[i] - catch[i,1]
    for(j in 2:Npass_r[i]) {
      catch[i,j] ~ dbin(p[i], z[i,j-1])
      z[i,j] <- N[i] - sum(catch[i, 1:j])
    }
    
    N[i] ~ dpois(lambda_r[i])
    log(lambda_r[i]) <-  beta[i]
  }
  
  for(i in 1:Nsamps_cat){
    U[i] ~ dnegbin(mean.p[sp2[i]], catch2[i])
    Ncat[i] <- U[i] + catch2[i]
  }
  
}
    
    ", fill = TRUE)
sink()    
#-----------------------------------------------------------------------------#
# max_pass = 11
# N=read.csv(file="N_GUESS.csv",header=T)
# N_guess = round(N[,1],digits = 0) + 5

N_guess = rep(1000, Nsamps_r)
# N_guess = c(47, 28, 36, 51, 74, 627, 27, 209, 29, 181, 177, 169, 106, 1,
#             41, 26, 83, 10, 31, 47, 51, 47, 75, 66, 40, 10)

#inits<- NULL
#Nsamps = Nsamps_m + Nsamps_r

inits <- function() list(N = N_guess)

catch[is.na(catch)] <- 0 

params = c("N", "p", "beta", "mean.p", "Ncat")
# params = c("Ncat") 

data.in = list(Nsamps_r = Nsamps_r, Npass_r = Npass_r, catch = catch, sp = sp, Nsp = Nsp, 
               sp2 = sp2, Nsamps_cat = Nsamps_cat, catch2 = catch2) 

## MCMC settings
ni <- 5000
nt <- 1
nb <- 2500

fit <- jags(data = data.in, inits = inits, params, "JAGS_working_test.jags",
            n.chains = 3, n.iter = ni, n.thin = nt, n.burnin = nb)


# fit <- jags.parallel(data = data.in, inits = inits, params, "JAGS_working_test.jags",
#             n.chains = 3, n.iter = ni, n.thin = nt, n.burnin = nb,
#             export_obj_names = c('nb', 'N_guess', 'ni', 'nt'))


# rm(list=setdiff(ls(), "fit"))

# f1 = coda::mcmc.list(lapply(1:fit$model$nchain(), function(x) coda::mcmc(fit$BUGSoutput$sims.array[,x,])))

# fit2 = as.mcmc(fit)
#-----------------------------------------------------------------------------#
library(fishR)

out = organize(fit, par.name = "mean.p")


windows(record = T, xpos = 25)

stan_trace(fit, "p", 1:7, same.scale = TRUE)
stan_trace(fit, "mean.p", 1:7, same.scale = TRUE)
stan_trace(fit, "mean.p", 1:3, same.scale = TRUE)

#-----------------------------------------------------------------------------#
# None of this works....:(


# library(broom)
# library(tidyverse)
# 
# load("C:/Users/mdodrill/Desktop/FHM/Model_Runs/test2.RData")
# 
# tidyMCMC(fit2, pars = "p")
# tidy(fit, pars = "N", estimate.method = "mean", conf.int = FALSE,
#      conf.level = 0.95, conf.method = "quantile")
# 
# 
# install.packages("devtools")
# devtools::install_github("mjskay/tidybayes")
# 
# library(tidybayes)
# 
# f = spread_samples(fit2, regex = FALSE, sep = "[, ]")

# fit$model$nchain()
# 
# recompile(fit)

