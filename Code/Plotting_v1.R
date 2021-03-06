###############################################################################
#                                                                        Nov 18
#           Model Plotting - 2 Stage MultSp Removal Model 
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
library(RColorBrewer)
library(gridExtra)

# library(devtools)
# install_github(repo = 'mdodrill-usgs/fishR')

library(fishR)

windows(record = T, xpos = 25, width = 12, height = 9)
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

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

cols = gg_color_hue(9)
#-----------------------------------------------------------------------------#
# individual p's for all site, species

p.dat = organize(fit = fit, par.name = "p")

p.dat$id = as.numeric(gsub("[^0-9]", "", as.character(p.dat$Parameter)))

p.dat$species = sp.key.big[match(p.dat$id, sp.key.big$num),]$name

sm.p.dat = sample_n(p.dat, 20000) # cut data down for faster plotting

p = ggplot(p.dat, aes(x = value, group = Parameter)) +
  geom_density(color = "gray70", fill = "dodgerblue2", alpha = .2) +
  theme_base()
p

# facet by species
p = ggplot(sm.p.dat, aes(x = value, group = Parameter)) +
  # geom_density(aes(color = "black", fill = species), alpha = .4) +
  geom_density(aes(fill = species), alpha = .4) +
  # scale_fill_brewer(type = "div", palette = 9) +
  facet_wrap(~ species, scales = "free_y") +
  labs(fill = "Species",
       y = "Density", x = "Capture Probability") +
  theme_base() +
  theme(legend.position = c(.875,.15)) +
  guides(fill = guide_legend(ncol = 3))
p


#--------------------------------------
# mean p's for each species 

mu.p.dat = organize(fit = fit, par.name = "mean.p")

mu.p.dat$id = as.numeric(gsub("[^0-9]", "", as.character(mu.p.dat$Parameter)))

mu.p.dat$species = sp.key[match(mu.p.dat$id, sp.key$num),]$name
mu.p.dat$lab = sp.key[match(mu.p.dat$id, sp.key$num),]$name2


p = ggplot(mu.p.dat, aes(x = value, group = Parameter)) +
  geom_density(aes(color = species, fill = species), alpha = .2) +
  theme_base() +
  theme(legend.position = c(.15,.85)) +
  guides(col = guide_legend(ncol = 3))
p

#--------------------------------------


#--------------------------------------
# both the mean.p and individual p's, facet by species 

p = ggplot(sm.p.dat, aes(x = value, group = Parameter)) +
  geom_density(aes(color = species, fill = species), alpha = .4) +
  geom_density(data = mu.p.dat, aes(x = value), color = "black",
               fill = "black", alpha = .2) +
  labs(fill = "Species", color = "Species",
       y = "Density", x = "Capture Probability") +
  facet_wrap(~ species, scales = "free_y") +
  theme_base() +
  theme(legend.position = c(.875,.15)) +
  guides(col = guide_legend(ncol = 3))
p

#--------------------------------------


#-----------------------------------------------------------------------------#
# Abundance

N.dat = bayes_summary(fit = fit, par.name = "Ncat")

N.dat$id = as.numeric(gsub("[^0-9]", "", as.character(N.dat$Parameter)))
N.dat$river_mile = rm.key[match(N.dat$id, rm.key$num),]$river_mile
N.dat$species = rm.key[match(N.dat$id, rm.key$num),]$sp
N.dat$year = rm.key[match(N.dat$id, rm.key$num),]$year


p = ggplot(N.dat, aes(y = my.mean, x = river_mile)) +
  geom_point(aes(color = species), shape = 1) +
  geom_errorbar(aes(ymin = lower, ymax = upper, color = species)) +
  facet_wrap(~ species, scales = "free_y") +
  theme_base() +
  theme(legend.position = c(.85,.15)) +
  guides(col = guide_legend(ncol = 3))

p 



N.hbc = N.dat[N.dat$species == "HBC",]

p = ggplot(N.hbc, aes(y = my.mean, x = river_mile)) +
  geom_point(aes(color = species), shape = 1) +
  geom_errorbar(aes(ymin = lower, ymax = upper, color = species)) +
  facet_wrap(~ year, scales = "free_y") +
  theme_base() +
  theme(legend.position = c(.85,.15)) +
  guides(col = guide_legend(ncol = 3))

p 

# write.table(N.hbc, file = "SN.N.hbc.dat.csv", sep = ",", row.names = F)
#-----------------------------------------------------------------------------#
# End



