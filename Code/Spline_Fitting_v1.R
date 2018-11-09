###############################################################################
#                                                                        Nov 18
#         Fitting Splines to the predicted abundance
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

library(mgcv)

# library(devtools)
# install_github(repo = 'mdodrill-usgs/fishR')

library(fishR)

windows(record = T, xpos = 25, width = 12, height = 7)
#-----------------------------------------------------------------------------#

rm.key = data.frame(river_mile = cat.2$start_rm,
                    num = 1:dim(cat.2)[1],
                    sp = cat.2$species_code,
                    year = cat.2$year)

#-----------------------------------------------------------------------------#
N.dat = bayes_summary(fit = fit, par.name = "Ncat")

N.dat$id = as.numeric(gsub("[^0-9]", "", as.character(N.dat$Parameter)))
N.dat$river_mile = rm.key[match(N.dat$id, rm.key$num),]$river_mile
N.dat$species = rm.key[match(N.dat$id, rm.key$num),]$sp
N.dat$year = rm.key[match(N.dat$id, rm.key$num),]$year

N.hbc = N.dat[N.dat$species == "HBC",]


#-----------------------------------------------------------------------------#


b0 <- gam(my.mean ~ s(river_mile, bs = "ps"), data = N.hbc, method = "REML")
b0 <- gam(my.mean ~ s(river_mile, bs = "ad"), data = N.hbc, method = "REML")


# plot.gam()
# 
# plot(b0,pages=1,seWithMean=TRUE)
# 
# plot(b0,pages=1,residuals=TRUE,all.terms=TRUE,shade=TRUE,shade.col=2)


new.dat = data.frame(river_mile = seq(min(N.hbc$river_mile),max(N.hbc$river_mile),5))

tmp = predict.gam(b0, newdata = new.dat, se.fit = TRUE)



out = data.frame(y_hat = tmp$fit,
                 upper = tmp$fit + 2*tmp$se.fit,
                 lower = tmp$fit - 2*tmp$se.fit,
                 river_mile = new.dat$river_mile)


p = ggplot(out, aes(y = y_hat, x = river_mile)) +
    geom_line() +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .2) +
    geom_point(data = N.hbc,aes(y = my.mean), shape = 1) +
    # geom_errorbar(data = N.hbc, aes(y = my.mean,ymin = lower, ymax = upper)) +
    # geom_errorbar(aes(ymin = lower, ymax = upper)) +
    coord_cartesian(ylim = c(0,50))
p


big.out = list()
u.yr = unique(N.hbc$year)

u.yr = u.yr[!u.yr %in% c("2002", "2012", "2013")]

for(i in 1:length(u.yr)){
  sub = N.hbc[N.hbc$year == u.yr[i],]
  
  b0 <- gam(my.mean ~ s(river_mile, bs = "ps"), data = sub, method = "REML")
  
  new.dat = data.frame(river_mile = seq(min(sub$river_mile),max(sub$river_mile),1))
  
  tmp = predict.gam(b0, newdata = new.dat, se.fit = TRUE)
  
  out = data.frame(y_hat = tmp$fit,
                   upper = tmp$fit + 2*tmp$se.fit,
                   lower = tmp$fit - 2*tmp$se.fit,
                   river_mile = new.dat$river_mile,
                   year = u.yr[i])
  
  big.out[[i]] = out
  
}

all.out = do.call('rbind', big.out)



p = ggplot(all.out, aes(y = y_hat, x = river_mile)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .2) +
  geom_point(data = N.hbc,aes(y = my.mean), shape = 1) +
  # geom_errorbar(data = N.hbc, aes(y = my.mean,ymin = lower, ymax = upper)) +
  # geom_errorbar(aes(ymin = lower, ymax = upper)) +
  # coord_cartesian(ylim = c(0,50)) 
  facet_wrap(~year, scales = "free")
p


all.out$period = ifelse(all.out$year %in% c("2015", "2016", "2017", "2018"),
                        "new", "old")


p = ggplot(all.out, aes(y = y_hat, x = river_mile)) +
  geom_line(aes(color = year)) +
  # geom_ribbon(aes(ymin = lower, ymax = upper, fill = year), alpha = .2) +
  # geom_point(data = N.hbc,aes(y = my.mean), shape = 1) +
  # geom_errorbar(data = N.hbc, aes(y = my.mean,ymin = lower, ymax = upper)) +
  # geom_errorbar(aes(ymin = lower, ymax = upper)) +
  coord_cartesian(ylim = c(0,50)) +
  facet_wrap(~period)
p





#-----------------------------------------------------------------------------#



