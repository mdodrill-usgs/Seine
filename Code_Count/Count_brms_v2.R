###############################################################################
#                                                                        Jan 19
#           Seine Data  
#
#  Notes:
#  * Now uses the R package, fishR !
#  * Run Get_SN_Data_count_v1
#
#  To Do:
#  * Add covariates - temperature, temp.diff, other?
#  * See if LOO will work...?
#  * Think about CAR models for the site effects
#
###############################################################################
library(rstan)
library(brms)
library(fishR)
library(ggplot2)
library(bayesplot)

options(mc.cores = parallel::detectCores())
windows(record = TRUE, xpos = 25)
#-----------------------------------------------------------------------------#
# lots of zeros
p = ggplot(cat, aes(x = tot.catch)) +
    geom_histogram() +
    facet_wrap(~ species_code, scales = "free")
p

table(cat$species_code, (cat$tot.catch == 0))

#--------------------------------------
# temperature

p2 = ggplot(cat, aes(x = temp_bkw, y = log(tot.catch))) +
     geom_point() +
     geom_smooth() +
     geom_smooth(method = "lm")
p2     

p2 = ggplot(cat, aes(x = log(temp_bkw), y = log(tot.catch))) +
     geom_point() +
     # geom_smooth() +
     geom_smooth(method = "lm") + 
     facet_wrap(~ year)
p2     

#--------------------------------------
u.sp = unique(cat$species_code)

for (i in seq_along(u.sp)) {
    
    p3 = ggplot(cat[which(cat$species_code == u.sp[i]),],
                aes(x = start_rm, y = tot.catch)) +
    geom_point() + 
      labs(title = u.sp[i]) +
    facet_wrap(~ year, scales = "free_y")
  print(p3)
}


#-----------------------------------------------------------------------------#


sub = cat[cat$species_code == "FMS",]

# posterior predictive check looks bad for this model
# fit = brm(tot.catch ~ 1, data = sub, 
#            family = "zero_inflated_poisson",
#           chains = 3, cores = 3)
# pp_check(fit)

fit1 = brm(tot.catch ~ 1, data = sub, 
           family = "negbinomial", chains = 3, cores = 3)
pp_check(fit1)

fit2 = brm(tot.catch ~ 1, data = sub, 
           family = "zero_inflated_negbinomial",
           chains = 3, cores = 3)
pp_check(fit2)

# kfold(fit, fit1, fit2)

#-----------------------------------------------------------------------------#
# posterior predictive check of the # of zeros
# negative binomial looks good vs. zero inflated. 

u.sp = unique(cat$species_code)
prop_zero <- function(x) mean(x == 0)

for(i in seq_along(u.sp)){
  sub = cat[cat$species_code == u.sp[i],]
  
  fit1 = brm(tot.catch ~ 1, data = sub, 
             family = "negbinomial", chains = 3, cores = 3)
  
  yrep = posterior_predict(fit1, draws = 500)
  print(ppc_stat(sub$tot.catch, yrep, stat = "prop_zero", binwidth = 0.005))
  
  fit2 = brm(tot.catch ~ 1, data = sub, 
             family = "zero_inflated_negbinomial",
             chains = 3, cores = 3)
  
  yrep2 = posterior_predict(fit2, draws = 500)  
  print(ppc_stat(sub$tot.catch, yrep2, stat = "prop_zero", binwidth = 0.005))
}


#-----------------------------------------------------------------------------#

specs = c("HBC", "FMS", "BHS", "SPD")
specs = c("HBC")

fit.list = NULL
ic.list = NULL

p1 = proc.time()
for(i in 1:length(specs)){
  sub = N.dat[N.dat$species == specs[i],]
  sub$count = as.integer(sub$my.mean)
  sub$site = as.factor(sub$id)
  
  fit1 = brm(count ~ 1, data = sub, 
             family = "negbinomial", chains = 3, cores = 3)
  
  fit2 = brm(count ~ (1|site), data = sub, 
             family = "negbinomial", chains = 3, cores = 3)
  
  fit3 = brm(count ~ year + (1|site), data = sub, 
             family = "negbinomial", chains = 3, cores = 3)
  
  # loo1 = loo(fit1, fit2, fit3)
  # w.aic = waic(fit1, fit2, fit3)
  # k.fold = kfold(fit1, fit2, fit3)
  fit.list[[i]] = list(fit1, fit2, fit3)
  ic.list[[i]] = kfold(fit1, fit2, fit3)
}

p2 = proc.time()




#-----------------------------------------------------------------------------#
# figure out CAR models here...

# ltl = sub[sub$year == 2018,]

sub = cat[cat$year == 2018,]
sub = sub[sub$species_code == "FMS",]


sub$site = as.factor(round(sub$start_rm))

sub2 = sub[order(sub$start_rm),]

# x = c(1:length(ltl$river_mile))
x = c(1:max(sub$start_rm))
# x = as.numeric(sub2$site)
# x = round(sub2$start_rm)

x = c(1,2,3,4,6)

tmp = dist(x)

tmp2 = tmp

tmp2[tmp != 1] <- NA

tmp3 = as.matrix(tmp2)

tmp3[is.na(tmp3)] <- 0

View(tmp3[1:10,1:10])

W = tmp3




fit1 = brm(count ~ 1, data = ltl, family = "negbinomial")

fit2 = brm(count ~ site, data = sub, family = "negbinomial", autocor = cor_car(W))
fit2 = brm(count ~ 1, data = ltl, family = "negbinomial", autocor = cor_car(W))

fit3 = brm(count ~ river_, data = ltl, family = "Poisson")
fit4 = brm(count ~ 1, data = ltl, family = "Poisson", autocor = cor_car(W))

loo(fit1,fit2)

kfold(fit1, fit2)

#-----------------------------------------------------------------------------#