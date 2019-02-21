###############################################################################
#                                                                        Jan 19
#           Seine Data  
#
#  Notes:
#  * Now uses the R package, fishR !
#  * Run Get_SN_Data_count_v1
#  * loo won't work for comparisions
#
#  To Do:
#  * Add covariates - temperature, temp.diff, other?
#  * Think about CAR models for the site effects - working on this...
#
###############################################################################
library(rstan)
library(brms)
library(fishR)
library(ggplot2)
library(bayesplot)
library(plyr)

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
# Schmidt and Graf 1990
schmidt = data.frame(name = c("Permian Section",
                              "Supai Gorge",
                              "Redwall Gorge",
                              "Lower Marble Canyon",
                              "Furnace Flats",
                              "Upper Granite Gorge",
                              "Aisles",
                              "Middle Granite Gorge",
                              "Muav Gorge",
                              "Lower Canyon",
                              "Lower Granite Gorge"),
                     start = c(0, 11, 22.6, 40, 61.6, 77.5, 117.9, 125.6, 140, 160, 213.9),
                     stop = c(10.9, 22.5, 35.9, 61.5, 77.4, 117.8, 125.5, 139.9, 159.9, 213.8, 225))
#-----------------------------------------------------------------------------#
# figure out CAR models here...

# ltl = sub[sub$year == 2018,]



sub = cat[cat$year %in% c("2009", "2015", "2016", "2017", "2018"),]
# sub = cat[cat$year %in% c("2009"),]
sub = sub[sub$species_code == "HBC",]

sub = cat[cat$species_code == "HBC",]


sub2 = sub[order(sub$start_rm),]

sub2$site = as.factor(round_any(sub2$start_rm, 10))

# sub2$site2 = as.factor(as.numeric(as.factor(sub2$site)))


# x = c(1,2,3,4,6)



# x = sub$start_rm
x = round_any(sub2$start_rm, 10)

x = as.numeric(as.factor(x))

tmp = dist(x)

tmp2 = tmp

tmp2[tmp != 1] <- NA

tmp3 = as.matrix(tmp2)

tmp3[is.na(tmp3)] <- 0

# View(tmp3[1:10,1:10])

W = tmp3



# fit2 = brm(tot.catch ~ 1, data = sub, family = "negbinomial", autocor = cor_car(W), chains = 3, iter = 1000)

# fit.1 = brm(tot.catch ~ year + (1|site2), data = sub2, family = "negbinomial", autocor = cor_car(W),
            # chains = 3, iter = 2000)#, control = list(adapt_delta = .99, max_treedepth = 11))

fit.1 = brm(tot.catch ~ year + (1|site), data = sub2, family = "negbinomial", autocor = cor_car(W),
            chains = 3, iter = 2000)#, control = list(adapt_delta = .99, max_treedepth = 11))
stancode(fit.1)


new.dat = data.frame(expand.grid(year = 2009, site = unique(sub2$site)))
new.dat$year = as.factor(new.dat$year)


test = predict(fit.1, newdata = new.dat, re_formula = ~ 1 + (1|site2))
test = predict(fit.1)


sub2$est = test[,1]


p = ggplot(sub2, aes(x = site, y = tot.catch)) +
    geom_jitter(color = "red") +
    geom_jitter(aes(y = est), color = "black")
p


fit.2 = brm(tot.catch ~ (1|site2), data = sub2, family = "negbinomial", chains = 3, iter = 1000)


loo(fit.1,fit.2)
waic(fit.1)
# kfold(fit1, fit2)

#-----------------------------------------------------------------------------#

rand = as.data.frame(ranef(fit.1))
rand$site = unique(sub2$site)
names(rand)[1:4] = c("Est", "SD", "lower", "upper")


p = ggplot(rand, aes(x = site, y = exp(Est +2))) +
    geom_point() +
    geom_errorbar(aes(ymax = exp(upper+2), ymin = exp(lower+2)), width = 0)
p
#-----------------------------------------------------------------------------#

f.1 = as.data.frame(fixef(fit.1))

fix = as.data.frame(matrix(NA, nrow = nrow(f.1), ncol = 4))

for(i in 1:nrow(fix)){
  if(i == 1){
    fix[i,2] = f.1[i,1]
    fix[i,3] = f.1[i,3]
    fix[i,4] = f.1[i,4]
  } else {
    fix[i,2] = f.1[i,1] + f.1[1,1]
    fix[i,3] = f.1[i,3] + f.1[1,1]
    fix[i,4] = f.1[i,4] + f.1[1,1]
  }
}

fix[,1] = sort(unique(sub2$year))
names(fix) = c("year", "est", "lower", "upper")

p2 = ggplot(fix, aes(x = year, y = exp(est))) +
     geom_point() +
     geom_errorbar(aes(ymax = exp(upper), ymin = exp(lower)), width = 0)

p2

#-----------------------------------------------------------------------------#



library(shinystan)
launch_shinystan(fit.1)

#-----------------------------------------------------------------------------#
