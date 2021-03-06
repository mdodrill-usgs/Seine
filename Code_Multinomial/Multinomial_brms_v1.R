###############################################################################
#                                                                        Nov 18
#           Seine Data  
#
#  Notes:
#  * Now uses the R package, fishR !
#  * Run Get_SN_Data_v1
#
#  To Do:
#  * Add covariates - temperature, temp.diff, other?
#  * See if LOO will work...?
#  * Think about CAR models for the site effects
#
###############################################################################
library(rstan)
library(brms)
library(ggplot2)
library(reshape2)

rstan_options (auto_write=TRUE)
options (mc.cores=parallel::detectCores ()) # Run on multiple cores

# setwd("C:/Users/mdodrill/Desktop/Seine-master (3)/Seine-master/Code")
setwd(paste0(getwd(), "/Code_Multinomial"))
#-----------------------------------------------------------------------------#
# format for categorical 
dat.18 = cat[cat$year == "2018",]

u.site = unique(dat.18$sample_id)

out = list()

for(i in 1:length(u.site)){
  sub = dat.18[dat.18$sample_id == u.site[i],]
  
  tmp = list()
  
  for(j in 1:length(unique(sub$species_code))){
    tmp[[j]] = rep(unique(sub$species_code)[j], sub$tot.catch[j])
  }
  
  spec = do.call(c,tmp)
  
  out[[i]] = data.frame(sample.id = u.site[i],
                   species = spec)
}

all = do.call(rbind, out)

# subset for the relatively most common, leaving rbt for the contrast
# table(all$species)

all = all[all$species %in% c("RBT", "SPD", "BHS", "FMS", "HBC", "FHM", "PKF"),]

all$site = as.factor(all$sample.id)

# site and river mile
key = data.frame(site = unique(all$site))
key$num = seq(1, nrow(key), 1)
key$rm = cat[match(key$site, cat$sample_id),]$start_rm

all$rm = key[match(all$site, key$site),]$rm

#-----------------------------------------------------------------------------#
# site as a fixed effect (doesn't work well w/o good priors - not default)
# data.in = brms::make_standata(species ~ site,
#                     data = all, family = "categorical")
#--------------------------------------
# add in spline

data.in = brms::make_standata(species ~ site + s(rm),
                    data = all, family = "categorical")

make_stancode(species ~ site + s(rm),
              data = all, family = "categorical",
              save_model = "test_1.stan")

#--------------------------------------
# site as a random effect  
data.in = make_standata(species ~ (1|site), data = all, family = "categorical")

make_stancode(species ~ (1|site),
              data = all, family = "categorical", save_model = "test_2.stan")

ni = 250
nt = 1
nb = 100
nc = 3


fit.1 = brm(species ~ (1|site),
             data = all, family = "categorical",
             chains = nc, iter = ni, warmup = nb, thin = nt)

# loo(fit.1) # warning see kfold


# rm(list=setdiff(ls(), "fit.1"))

#--------------------------------------
# site as a random effect & spline on rm

# , bs = "ps"

data.in = make_standata(species ~ (1|site) + s(rm, bs = "ps"),
                        data = all, family = "categorical")

make_stancode(species ~ (1|site) + s(rm, bs = "ps"),
              data = all, family = "categorical",
              save_model = "test_3.stan")

#--------------------------------------
# 
# north <- 1:10
# Grid <- expand.grid(north)
# K <- nrow(Grid)
# # set up distance and neighbourhood matrices
# distance <- as.matrix(dist(Grid))
# W <- array(0, c(K, K))
# W[distance == 1] <- 1


u.site = key[order(key$rm),]$site
Grid <- expand.grid(u.site)
K <- nrow(Grid)
# set up distance and neighbourhood matrices
distance <- as.matrix(dist(Grid))
W <- array(0, c(K, K))
W[distance == 1] <- 1

rownames(W) = levels(all$site) # thougt I saw this somewhere...



ni = 1000
nt = 1
nb = 500
nc = 3


fit.1 = brm(species ~ site, autocor = cor_car(W),
            data = all, family = "categorical")#,
            # chains = nc, iter = ni, warmup = nb, thin = nt)

#-----------------------------------------------------------------------------#
# run the model with rstan  

# sm.params <- c("b_muPKF", "b_muHBC", "b_muBHS", "b_muFMS")
# sm.params <- c("z_1", "z_2", "z_3", "z_4")
# sm.params <- c("b_muBHS_Intercept", "b_muFMS_Intercept",
#                "b_muHBC_Intercept", "b_muPKF_Intercept")
sm.data = data.in


# MCMC settings
ni = 1000
nt = 1
nb = 500
nc = 3


# Call Stan from R 
# t1 <- proc.time()
# fit <- stan("model.stan",
fit <- stan("test_3.stan",
             # SM <- stan(fit = SM,
             data = sm.data,
             # pars = sm.params,
             # control = list(adapt_delta = .85),
             # control = list(max_treedepth = 14),
             control = list(max_treedepth = 11, adapt_delta = .85),
             chains = nc, iter = ni, warmup = nb, thin = nt, seed = 1)
# t2 <- proc.time()

#-----------------------------------------------------------------------------#
launch_shinystan(fit)

windows(record = TRUE, xpos = 25)

fishR::stan_trace(fit = fit, par.name = "b_muFMS", number = 1:6)
fishR::stan_trace(fit = fit, par.name = "sd_1", number = 1:6)
fishR::stan_trace(fit = fit, par.name = "Z_1", number = 1:6)
fishR::stan_trace(fit = fit, par.name = "bs_muSPD", number = 1:6)


tmp = fishR::bayes_summary(fit = fit, par.name = "Z_1")


#-----------------------------------------------------------------------------#











#-----------------------------------------------------------------------------#