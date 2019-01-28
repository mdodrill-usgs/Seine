###############################################################################
#                                                                        Nov 18
#           Seine Data - Examples on how to use brms
#
#  Notes:
#  * Now uses the R package, fishR !
#  * Run Get_SN_Data_v1
#
#  To Do:
#
###############################################################################
library(rstan)
library (brms)
library(ggplot2)
library(reshape2)

rstan_options (auto_write=TRUE)
options (mc.cores=parallel::detectCores ()) # Run on multiple cores

setwd("C:/Users/mdodrill/Desktop/Seine-master (3)/Seine-master/Code")
#-----------------------------------------------------------------------------#
# example from brms...
# set.seed (3875)
# 
# ir <- data.frame (scale (iris[, -5]), Species=iris[, 5])
# 
# b2 <- brm(Species ~ Petal.Length + Petal.Width + Sepal.Length + Sepal.Width,
#           data = ir, family = "categorical",
#           # n.chains = 3, n.iter = 3000, n.warmup = 600,
#           prior = c(set_prior("normal (0, 8)")))
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
u.sub = unique(all$sample.id)[1:20]

# ltl = all[1001:1500,]
# ltl$site = as.factor(c(rep("site_1", 250), rep("site_2", 250)))

ltl = all[which(all$sample.id %in% u.sub),]
ltl$site = as.factor(ltl$sample.id)


# all$site = as.factor(all$sample.id)


# b2 <- brm(species ~ site,
# data = all, family = "categorical")
# n.chains = 3, n.iter = 3000, n.warmup = 600,
# prior = c(set_prior("normal (0, 8)")))



b2 <- brm(species ~ site,
          data = ltl, family = "categorical")

stancode(b2)
test = standata(b2)


pred = predict(b2)

pred.2 = as.data.frame(pred)

pred.2$site = ltl$site

pred.3 = melt(pred.2, id.vars = "site")


p = ggplot(pred.3, aes(x = value)) +
  geom_density(aes(color = variable, fill = variable)) +
  facet_wrap(~ site)
p


summ = group_by(pred.3, site, variable) %>%
  summarise(mean = mean(value)) %>% 
  as.data.frame()

summ$rm

p2 = ggplot(summ, aes(x = variable, y = mean)) +
  geom_point(aes(color = site))
p2


#-----------------------------------------------------------------------------#
