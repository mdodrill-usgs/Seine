###############################################################################
#                                                                        Dec 18
#           Seine Data  
#
#  Notes:
#  * Now uses the R package, fishR !
#  * Run Get_SN_Data_v1
#  * Run JAGS_MulitSp_v1
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


#-----------------------------------------------------------------------------#
sp.key = data.frame(name = unique(d1_r$species),
                    num = 1:Nsp,
                    name2 = c("Black Bullhead", "Bluehead Sucker", "Common Carp",
                              "Fathead Minnow", "Flannelmouth Sucker",
                              "Humpback Chub", "Plains Killifish",
                              "Rainbow Trout", "Red Shiner", "Speckled Dace",
                              "Unknown Sucker"))

sp.key.big = data.frame(name = d1_r$species,
                        num = 1:Nsamps_r)

rm.key = data.frame(river_mile = cat.2$start_rm,
                    num = 1:dim(cat.2)[1],
                    sp = cat.2$species_code,
                    year = cat.2$year)


#-----------------------------------------------------------------------------#
# Abundance

N.dat = bayes_summary(fit = fit, par.name = "Ncat")

N.dat$id = as.numeric(gsub("[^0-9]", "", as.character(N.dat$Parameter)))
N.dat$river_mile = rm.key[match(N.dat$id, rm.key$num),]$river_mile
N.dat$species = rm.key[match(N.dat$id, rm.key$num),]$sp
N.dat$year = rm.key[match(N.dat$id, rm.key$num),]$year
#-----------------------------------------------------------------------------#

specs = c("HBC", "FMS", "BHS", "SPD")

sub = N.dat[N.dat$species == "FMS",]
sub$count = as.integer(sub$my.mean)



f1 = brm(count ~ year, data = sub, family = "Poisson")
f2 = brm(count ~ 1, data = sub, family = "Poisson")


loo(f1)

#-----------------------------------------------------------------------------#
options(mc.cores = parallel::detectCores())
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

ltl = sub

sub$site = as.factor(round(sub$river_mile))

sub2 = sub[order(sub$river_mile),]

# x = c(1:length(ltl$river_mile))
x = as.numeric(sub2$site)

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




