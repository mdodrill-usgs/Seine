###############################################################################
#                                                                        Nov 18
#           JAGS Model Fitting - 2 Stage MultSp Removal Model 
#
#  Notes:
#  *
#
#  To Do:
#  * 
#
###############################################################################
library(ggplot2)
library(ggmcmc)
library(ggthemes)
library(dplyr)

# library(devtools)
# install_github(repo = 'mdodrill-usgs/fishR')

library(fishR)

windows(record = T, xpos = 25, width = 12, height = 7)
#-----------------------------------------------------------------------------#
sp.key = data.frame(name = unique(d1_r$species),
                    num = 1:Nsp)

sp.key.big = data.frame(name = d1_r$species,
                        num = 1:Nsamps_r)

#-----------------------------------------------------------------------------#
# individual p's for all site, species

p.dat = organize(fit = fit, par.name = "p")

p.dat$id = as.numeric(gsub("[^0-9]", "", as.character(p.dat$Parameter)))

p.dat$species = sp.key.big[match(p.dat$id, sp.key.big$num),]$name

sm.p.dat = sample_n(f2.sub, 20000) # cut data down for faster plotting

p = ggplot(p.dat, aes(x = value, group = Parameter)) +
  geom_density(color = "gray70", fill = "dodgerblue2", alpha = .2) +
  theme_base()
p

# facet by species
p = ggplot(sm.p.dat, aes(x = value, group = Parameter)) +
  geom_density(aes(color = species, fill = species), alpha = .2) +
  facet_wrap(~ species, scales = "free_y") +
  theme_base() +
  theme(legend.position = c(.85,.15)) +
  guides(col = guide_legend(ncol = 3))
p


#-----------------------------------------------------------------------------#
# mean p's for each species 

mu.p.dat = organize(fit = fit, par.name = "mean.p")

mu.p.dat$id = as.numeric(gsub("[^0-9]", "", as.character(mu.p.dat$Parameter)))

mu.p.dat$species = sp.key[match(mu.p.dat$id, sp.key$num),]$name


p = ggplot(mu.p.dat, aes(x = value, group = Parameter)) +
  geom_density(aes(color = species, fill = species), alpha = .2) +
  theme_base() +
  theme(legend.position = c(.15,.85)) +
  guides(col = guide_legend(ncol = 3))
p

#-----------------------------------------------------------------------------#
# both the mean.p and individual p's, facet by species 

p = ggplot(sm.p.dat, aes(x = value, group = Parameter)) +
  geom_density(aes(color = species, fill = species), alpha = .2) +
  geom_density(data = mu.p.dat, aes(x = value), color = "black", fill = "black", alpha = .2) +
  facet_wrap(~ species, scales = "free_y") +
  theme_base() +
  theme(legend.position = c(.85,.15)) +
  guides(col = guide_legend(ncol = 3))
p

#-----------------------------------------------------------------------------#
name = "Ncat"

rm.key = data.frame(river_mile = cat.2$start_rm,
                    num = 1:dim(cat.2)[1],
                    sp = cat.2$species_code,
                    year = cat.2$year)

tmp = coda::mcmc.list(lapply(1:fit$model$nchain(), function(x) coda::mcmc(fit$BUGSoutput$sims.array[,x,])))

f1 = ggs(tmp)

f1.sub = f1[which(substr(f1$Parameter,1,nchar(name)) == name),]

all2 = group_by(f1.sub, Parameter) %>%
  summarize(my.mean = mean(value),
            upper = quantile(value, .95),
            lower = quantile(value, .05))

all2$id = as.numeric(gsub("[^0-9]", "", as.character(all2$Parameter)))

all2$river_mile = rm.key[match(all2$id, rm.key$num),]$river_mile
all2$sp = rm.key[match(all2$id, rm.key$num),]$sp



p = ggplot(all2, aes(y = my.mean, x = river_mile)) +
  geom_point(aes(color = sp), shape = 1) +
  geom_errorbar(aes(ymin = lower, ymax = upper, color = sp)) +
  facet_wrap(~ sp, scales = "free_y") +
  theme_base() +
  theme(legend.position = c(.85,.15)) +
  guides(col = guide_legend(ncol = 3))

p 



# geom_point(data = pred, aes(y = my.mean, x = sz), size = 2, color = "black") +
#   geom_errorbar(data = pred, aes(y = my.mean, ymin = lower, ymax = upper),






