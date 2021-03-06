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


b0 <- gam(log(my.mean) ~ s(river_mile, bs = "ps"), data = N.hbc, method = "REML")
# b0 <- gam(log(my.mean) ~ s(river_mile, bs = "cr"), data = N.hbc, method = "REML")
# b0 <- gam(my.mean ~ s(river_mile, bs = "ps"), data = N.hbc, method = "REML")
# b0 <- gam(my.mean ~ s(river_mile, bs = "ad"), data = N.hbc, method = "REML")

gam.check(b0)

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

out = data.frame(y_hat = exp(tmp$fit),
                 upper = exp(tmp$fit + 2*tmp$se.fit),
                 lower = exp(tmp$fit - 2*tmp$se.fit),
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
  
  b0 <- gam(log(my.mean) ~ s(river_mile, bs = "ps"), data = sub, method = "REML")
  
  new.dat = data.frame(river_mile = seq(min(sub$river_mile),max(sub$river_mile),1))
  
  tmp = predict.gam(b0, newdata = new.dat, se.fit = TRUE)
  
  out = data.frame(y_hat = exp(tmp$fit),
                   upper = exp(tmp$fit + 2*tmp$se.fit),
                   lower = exp(tmp$fit - 2*tmp$se.fit),
                   river_mile = new.dat$river_mile,
                   year = u.yr[i])
  
  big.out[[i]] = out
  
}

all.out = do.call('rbind', big.out)



p = ggplot(all.out, aes(y = y_hat, x = river_mile)) +
  geom_line() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .2) +
  geom_point(data = N.hbc, aes(y = my.mean), shape = 1) +
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


b0 <- gam(log(my.mean) ~ year + s(river_mile, bs = "ps"), data = N.hbc, method = "REML")
b1 <- gam(log(my.mean) ~ s(river_mile, bs = "ps"), data = N.hbc, method = "REML")
b2 <- gam(log(my.mean) ~ s(river_mile, bs = "ps", by = year), data = N.hbc, method = "REML")

b3 <- lm(log(my.mean) ~ year, data = N.hbc)

AIC(b0, b1, b2, b3)


new.dat.2 = data.frame(river_mile = 200,
                       year = unique(N.hbc$year))



tmp = predict.gam(b0, newdata = new.dat.2, se.fit = TRUE)
# tmp = predict.gam(b2, newdata = new.dat.2, se.fit = TRUE)


out.2 = data.frame(y_hat = exp(tmp$fit),
                   upper = exp(tmp$fit + 2*tmp$se.fit),
                   lower = exp(tmp$fit - 2*tmp$se.fit),
                   year = new.dat.2$year)

p = ggplot(out.2, aes(y = y_hat, x = year))+
  geom_point() +
  geom_errorbar(aes(ymin = lower, ymax = upper))
p



new.dat.3 = data.frame(river_mile = seq(min(N.hbc$river_mile),max(N.hbc$river_mile),1),
                       year = 2016)



tmp = predict.gam(b0, newdata = new.dat.3, se.fit = TRUE)


out.3 = data.frame(y_hat = exp(tmp$fit),
                   upper = exp(tmp$fit + 2*tmp$se.fit),
                   lower = exp(tmp$fit - 2*tmp$se.fit),
                   river_mile = seq(min(N.hbc$river_mile),max(N.hbc$river_mile),1))

out.3$river_km = out.3$river_mile * 1.60934

# p = ggplot(out.3, aes(y = y_hat, x = river_km))+
p = ggplot(out.3, aes(y = y_hat, x = river_mile))+
  geom_line(size = 2) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .2) +
  scale_x_continuous(limits = c(0,250)) +
  labs(y = "HBC YOY Index")
  # geom_errorbar(aes(ymin = lower, ymax = upper))
p = p + theme_base()


# P:\BIOLOGICAL\Foodbase\LIGHT_TRAPS\CODE\\lt Jul25 for BQR figure
# FB_Wave is flow curve
# FB_LightTrap_Response = bug response
dat = read.csv("C:\\Users\\mdodrill\\Desktop\\test\\test.csv")



p2 = ggplot(dat, aes(y = y, x = x * 0.621371))+
  geom_line(size = 2) +
  scale_x_continuous(limits = c(0,250)) +
  labs(y = "water @ dusk", x = "river_mile")
p2 = p2 + theme_base()


grid.arrange(p,p2, ncol = 1)


#-----------------------------------------------------------------------------#
N.fms = N.dat[N.dat$species == "FMS",]



b0 <- gam(log(my.mean) ~ year + s(river_mile, bs = "ps"), data = N.fms, method = "REML")
b1 <- gam(log(my.mean) ~ s(river_mile, bs = "ps"), data = N.fms, method = "REML")
b2 <- gam(log(my.mean) ~ s(river_mile, bs = "ps", by = year), data = N.fms, method = "REML")

b3 <- lm(log(my.mean) ~ year, data = N.fms)

AIC(b0, b1, b2, b3)


new.dat.4 = data.frame(expand.grid(river_mile = seq(min(N.fms$river_mile), max(N.fms$river_mile),5),
                       year = unique(N.fms$year)))

new.dat.4 = data.frame(expand.grid(river_mile = seq(min(N.fms$river_mile), max(N.fms$river_mile),5),
                       year = unique(N.fms$year)[9]))



tmp = predict.gam(b0, newdata = new.dat.4, se.fit = TRUE)
# tmp = predict.gam(b2, newdata = new.dat.2, se.fit = TRUE)


out.2 = data.frame(y_hat = exp(tmp$fit),
                   upper = exp(tmp$fit + 2*tmp$se.fit),
                   lower = exp(tmp$fit - 2*tmp$se.fit),
                   year = new.dat.4$year,
                   river_mile = new.dat.4$river_mile)

p = ggplot(out.2, aes(y = y_hat, x = river_mile))+
  geom_point() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .2) +
  facet_wrap(~ year, scales = "free_y")
p


#-----------------------------------------------------------------------------#
N.bhs = N.dat[N.dat$species == "BHS",]



b0 <- gam(log(my.mean) ~ year + s(river_mile, bs = "ps"), data = N.bhs, method = "REML")
b1 <- gam(log(my.mean) ~ s(river_mile, bs = "ps"), data = N.bhs, method = "REML")
b2 <- gam(log(my.mean) ~ s(river_mile, bs = "ps", by = year), data = N.bhs, method = "REML")

b3 <- lm(log(my.mean) ~ year, data = N.bhs)

AIC(b0, b1, b2, b3)


new.dat.4 = data.frame(expand.grid(river_mile = seq(min(N.bhs$river_mile), max(N.bhs$river_mile),5),
                                   year = unique(N.bhs$year)))

new.dat.4 = data.frame(expand.grid(river_mile = seq(min(N.bhs$river_mile), max(N.bhs$river_mile),5),
                                   year = unique(N.bhs$year)[9]))



tmp = predict.gam(b0, newdata = new.dat.4, se.fit = TRUE)
# tmp = predict.gam(b2, newdata = new.dat.2, se.fit = TRUE)


out.2 = data.frame(y_hat = exp(tmp$fit),
                   upper = exp(tmp$fit + 2*tmp$se.fit),
                   lower = exp(tmp$fit - 2*tmp$se.fit),
                   year = new.dat.4$year,
                   river_mile = new.dat.4$river_mile)

p = ggplot(out.2, aes(y = y_hat, x = river_mile))+
  geom_point() +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .2) +
  facet_wrap(~ year, scales = "free_y")
p


