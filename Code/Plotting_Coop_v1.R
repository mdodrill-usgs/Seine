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

windows(record = T, xpos = 25, width = 12, height = 9) # preserved 4:3 ratio
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
p.dat = organize(fit = fit, par.name = "p")

p.dat$id = as.numeric(gsub("[^0-9]", "", as.character(p.dat$Parameter)))

p.dat$species = sp.key.big[match(p.dat$id, sp.key.big$num),]$name


# facet by species, cut down to 9 spp
sub.p.dat = p.dat[!p.dat$species %in% c("BBH", "SUC"),]
sub.p.dat$lab = sp.key[match(sub.p.dat$species, sp.key$name),]$name2

p = ggplot(sub.p.dat, aes(x = value, group = Parameter)) +
  geom_density(aes(fill = species), alpha = .4) +
  facet_wrap(~ lab, scales = "free_y") +
  labs(fill = "Species",
       y = "Relative Density", x = "Capture Probability") +
  scale_x_continuous(limits = c(0,1), breaks = c(0,.2,.4,.6,.8,1),
                     labels = c("0", "0.2", "0.4", "0.6", "0.8", "1")) +
  theme(legend.position = "none", 
        strip.text.x = element_text(size = 20),
        axis.title = element_text(size = 18),
        axis.text.x = element_text(size = 16, colour = "black"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.text = element_text(vjust = 1),
        strip.background = element_blank(),
        panel.spacing = unit(.5, "lines"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.line = element_line(colour = "black"))#,

p

#-----------------------------------------------------------------------------#
# both the mean.p and individual p's, facet by species, cut down to 9 spp 
mu.p.dat.sm = mu.p.dat[!mu.p.dat$species %in% c("BBH", "SUC"),]

p = ggplot(sub.p.dat, aes(x = value, group = Parameter)) +
  geom_density(aes(fill = species), alpha = .4) +
  geom_density(data = mu.p.dat.sm, aes(x = value), color = "black",
               fill = "black", alpha = .4) +
  scale_x_continuous(limits = c(0,1), breaks = c(0,.2,.4,.6,.8,1),
                     labels = c("0", "0.2", "0.4", "0.6", "0.8", "1")) +
  labs(fill = "Species", color = "Species",
       y = "Relative Density", x = "Capture Probability") +
  facet_wrap(~ lab, scales = "free_y") +
  # theme_base() +
  theme(legend.position = "none", 
        strip.text.x = element_text(size = 20),
        axis.title = element_text(size = 18),
        axis.text.x = element_text(size = 16, colour = "black"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        strip.text = element_text(vjust = 1),
        strip.background = element_blank(),
        panel.spacing = unit(.5, "lines"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.line = element_line(colour = "black"))#,

p

#-----------------------------------------------------------------------------#
mu.p.dat = organize(fit = fit, par.name = "mean.p")

mu.p.dat$id = as.numeric(gsub("[^0-9]", "", as.character(mu.p.dat$Parameter)))

mu.p.dat$species = sp.key[match(mu.p.dat$id, sp.key$num),]$name
mu.p.dat$lab = sp.key[match(mu.p.dat$id, sp.key$num),]$name2
# only a couple
specs = c("HBC", "FMS", "BHS", "SPD")
sub.mu.p.dat = mu.p.dat[mu.p.dat$species %in% specs,]

p = ggplot(sub.mu.p.dat, aes(x = value, group = Parameter)) +
  geom_density(aes(color = lab, fill = lab), alpha = .4, size = 1.5) +
  scale_x_continuous(limits = c(0,1), breaks = c(0,.2,.4,.6,.8,1)) +
  labs(x = "", y = "Relative Density", color = "", fill = "") +
  # theme_base() + 
  theme(legend.position = c(.15,.85),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black"),
        axis.title.x = element_text(size = 18, vjust = -.1),
        axis.title.y = element_text(size = 18, vjust = 1),
        axis.text.x = element_text(size = 16, colour = "black"),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.text = element_text(size = 16),
        plot.margin = unit(c(1,1,0,1), "cm"))
p


summ = bayes_summary(fit = fit, par.name = "mean.p")
summ$id = as.numeric(gsub("[^0-9]", "", as.character(summ$Parameter)))

summ$species = sp.key[match(summ$id, sp.key$num),]$name
summ$lab = sp.key[match(summ$id, sp.key$num),]$name2

sub.summ = summ[summ$species %in% specs,]

p2 = ggplot(sub.summ, aes(x = my.mean, y = lab)) +
  geom_point(aes(color = lab), size = 5) +
  geom_errorbarh(aes(xmin = lower, xmax = upper, color = lab),
                 height = 0, size = 2) +
  scale_x_continuous(limits = c(0,1), breaks = c(0,.2,.4,.6,.8,1),
                     labels = c("0", "0.2", "0.4", "0.6", "0.8", "1")) +
  labs(x = "Capture Probability") +
  # theme_base() +
  theme(axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(size = 16, colour = "black"),
        axis.title.x = element_text(size = 18, colour = "black"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank(),
        panel.background = element_blank(),
        axis.line.x = element_line(colour = "black"),
        legend.position = "none",
        plot.margin = unit(c(0,1,1,1), "cm"))

p2


# grid.arrange(p, p2, heights = c(.8, .1))



# https://stackoverflow.com/questions/13294952/left-align-two-graph-edges-ggplot
gA <- ggplotGrob(p)
gB <- ggplotGrob(p2)
maxWidth = grid::unit.pmax(gA$widths[2:5], gB$widths[2:5])
gA$widths[2:5] <- as.list(maxWidth)
gB$widths[2:5] <- as.list(maxWidth)
grid.arrange(gA, gB, ncol=1, heights = c(.8, .2))

#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
# Abundance

N.dat = bayes_summary(fit = fit, par.name = "Ncat")

N.dat$id = as.numeric(gsub("[^0-9]", "", as.character(N.dat$Parameter)))
N.dat$river_mile = rm.key[match(N.dat$id, rm.key$num),]$river_mile
N.dat$species = rm.key[match(N.dat$id, rm.key$num),]$sp
N.dat$year = rm.key[match(N.dat$id, rm.key$num),]$year

# error in the data, hbc above 30 mile - exclude for now
bad = which(N.dat$species == "HBC" & N.dat$river_mile <= 28)
N.dat = N.dat[-bad,]

# facet by species, cut down to 9 spp
sub.N.dat = N.dat[!N.dat$species %in% c("BBH", "SUC"),]
sub.N.dat$lab = sp.key[match(sub.N.dat$species, sp.key$name),]$name2


sub.N.dat.2 = sub.N.dat[sub.N.dat$species %in% specs,]

# p = ggplot(sub.N.dat.2, aes(y = my.mean, x = river_mile)) +
p = ggplot(sub.N.dat, aes(y = my.mean, x = river_mile)) +
  geom_point(aes(fill = species, color = species), alpha = .5, ) +
  geom_errorbar(aes(ymin = lower, ymax = upper, color = species), alpha = .25) +
  geom_point(color = "black", shape = 1, alpha = .25) +
  scale_x_continuous(breaks = seq(0,250,50)) +
  coord_cartesian(ylim = c(0, 100)) +
  labs(x = "River Mile", y = "Abundance +- 95% CRI") +
  facet_wrap(~ lab, scales = "free_y") +
  theme(legend.position = "none", 
        strip.text.x = element_text(size = 20),
        axis.title = element_text(size = 18),
        axis.text.x = element_text(size = 16, colour = "black"),
        axis.text.y = element_text(size = 16, colour = "black"),
        strip.text = element_text(vjust = 1),
        strip.background = element_blank(),
        panel.spacing = unit(.5, "lines"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.line = element_line(colour = "black"))#,
p 


#-----------------------------------------------------------------------------#
# HBC

N.hbc = sub.N.dat[sub.N.dat$species == "HBC",]

N.hbc.sub = N.hbc[!N.hbc$year %in% c("2002", "2012", "2013"),]

p = ggplot(N.hbc.sub, aes(y = my.mean, x = river_mile)) +
  geom_point(aes(fill = species), alpha = .5, color = "#00C19F") +
  geom_errorbar(aes(ymin = lower, ymax = upper), alpha = .25, color = "#00C19F") +
  geom_point(color = "black", shape = 1, alpha = .25) +
  # geom_smooth(se = TRUE, method = "lm", color = "#00C19F") +
  geom_smooth(se = TRUE, color = "#00C19F") +
  labs(x = "River Mile", y = "Abundance +- 95% CRI", title = "Humpback Chub") +
  scale_x_continuous(breaks = seq(0,250,50)) +
  coord_cartesian(ylim = c(0, 100)) +
  facet_wrap(~ year) +
  theme(legend.position = "none",
        title = element_text(size = 18),
        strip.text.x = element_text(size = 18),
        axis.title = element_text(size = 18),
        axis.text.x = element_text(size = 16, colour = "black"),
        axis.text.y = element_text(size = 16, colour = "black"),
        strip.text = element_text(vjust = 1),
        strip.background = element_blank(),
        panel.spacing = unit(.5, "lines"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.line = element_line(colour = "black"))

p

#-----------------------------------------------------------------------------#
# spline fitting
library(mgcv)
library(bbmle)  # AICtab - delta AIC table 

b0 <- gam(log(my.mean) ~ year + s(river_mile, bs = "ps"), data = N.hbc.sub, method = "REML")
b1 <- gam(log(my.mean) ~ s(river_mile, bs = "ps"), data = N.hbc.sub, method = "REML")
b2 <- gam(log(my.mean) ~ s(river_mile, bs = "ps", by = year), data = N.hbc.sub, method = "REML")

b3 <- lm(log(my.mean) ~ year, data = N.hbc.sub)
b4 <- lm(log(my.mean) ~ river_mile, data = N.hbc.sub)
b5 <- lm(log(my.mean) ~ year + river_mile, data = N.hbc.sub)
b6 <- lm(log(my.mean) ~ year * river_mile, data = N.hbc.sub)
 

ICtab(b0, b1, b2, b3, b4, b5, b6, type = "AIC")


#-----------------------------------------------------------------------------#
# year effects
windows(record = T, xpos = 25, width = 12/.9, height = 9/2)
new.dat.2 = data.frame(river_mile = 100,
                       year = unique(N.hbc.sub$year))

tmp = predict.gam(b0, newdata = new.dat.2, se.fit = TRUE)


out.2 = data.frame(y_hat = exp(tmp$fit),
                   upper = exp(tmp$fit + 2*tmp$se.fit),
                   lower = exp(tmp$fit - 2*tmp$se.fit),
                   year = new.dat.2$year)

p = ggplot(out.2, aes(y = y_hat, x = year))+
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = lower, ymax = upper), width = 0) +
  labs(y = "HBC Site Abundance +-95% CI\n (@ reference - 100 RM)",
       x = "") +
  theme(legend.position = "none",
        title = element_text(size = 18),
        strip.text.x = element_text(size = 18),
        axis.title = element_text(size = 18),
        axis.text.x = element_text(size = 16, colour = "black"),
        axis.text.y = element_text(size = 16, colour = "black"),
        strip.text = element_text(vjust = 1),
        strip.background = element_blank(),
        panel.spacing = unit(.5, "lines"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.line = element_line(colour = "black"))
p

#--------------------------------------
# river mile function
windows(record = T, xpos = 25, width = 12, height = 9) # preserved 4:3 ratio

new.dat.3 = data.frame(river_mile = seq(min(N.hbc.sub$river_mile),220,1),
                       year = 2016)

tmp = predict.gam(b0, newdata = new.dat.3, se.fit = TRUE)


out.3 = data.frame(y_hat = exp(tmp$fit),
                   upper = exp(tmp$fit + 2*tmp$se.fit),
                   lower = exp(tmp$fit - 2*tmp$se.fit),
                   river_mile = seq(min(N.hbc.sub$river_mile),220,1))

out.3$river_km = out.3$river_mile * 1.60934

# p = ggplot(out.3, aes(y = y_hat, x = river_km))+
p = ggplot(out.3, aes(y = y_hat, x = river_mile))+
  geom_line(size = 2) +
  geom_ribbon(aes(ymin = lower, ymax = upper), alpha = .2) +
  scale_x_continuous(limits = c(0,220)) +
  labs(y = "YOY HBC N Index +-95% CI\n (reference Year - 2016)",
       x = "River Mile") +
  theme(legend.position = "none",
        title = element_text(size = 18),
        strip.text.x = element_text(size = 18),
        axis.title = element_text(size = 18),
        axis.text.x = element_text(size = 16, colour = "black"),
        axis.text.y = element_text(size = 16, colour = "black"),
        strip.text = element_text(vjust = 1),
        strip.background = element_blank(),
        panel.spacing = unit(.5, "lines"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.line = element_line(colour = "black"))

p


# P:\BIOLOGICAL\Foodbase\LIGHT_TRAPS\CODE\\lt Jul25 for BQR figure
# FB_Wave is flow curve
# FB_LightTrap_Response = bug response
library(arm)

dat = read.csv("FB_Wave.csv")
bug = read.csv("FB_LightTrap_Response.csv")

bug$y2 = bug$y - mean(bug$y) + mean(dat$y, na.rm = T)
bug$y2 = scale(bug$y) * mean(dat$y, na.rm = T)/2 + 60



p2 = ggplot(dat, aes(y = y, x = x * 0.621371))+
  geom_line(size = 2) +
  # geom_line(data = bug, aes(y = y2, x = x * 0.621371), color = "red", size = 2) +
  scale_x_continuous(limits = c(0,220)) +
  # annotate("text", x = 150, y = 150, label = "Average Midge Abundance",
  #          color = "red", size = 8)+
  labs(y = "water @ dusk", x = "River Mile") +
  theme(legend.position = "none",
        title = element_text(size = 18),
        strip.text.x = element_text(size = 18),
        axis.title = element_text(size = 18),
        axis.text.x = element_text(size = 16, colour = "black"),
        # axis.text.y = element_text(size = 16, colour = "black"),
        axis.text.y = element_blank(),
        strip.text = element_text(vjust = 1),
        strip.background = element_blank(),
        panel.spacing = unit(.5, "lines"),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect(colour = "black", fill = NA),
        axis.line = element_line(colour = "black"))
p2









gA <- ggplotGrob(p)
gB <- ggplotGrob(p2)
maxWidth = grid::unit.pmax(gA$widths[2:5], gB$widths[2:5])
gA$widths[2:5] <- as.list(maxWidth)
gB$widths[2:5] <- as.list(maxWidth)
grid.arrange(gA, gB, ncol=1, heights = c(.5, .5))




#-----------------------------------------------------------------------------#
# adding in a little bug picture -- looks pretty shitty...
# 
# library(png)
# library(grid)
# img <- readPNG(system.file("img", "midge_adult.png", package="png"))
# g <- rasterGrob(img, interpolate=TRUE)
# 
# qplot(1:10, 1:10, geom="blank") +
#   annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
#   geom_point()
# 
# img <- readPNG(source = "U:\\Desktop\\Fish_Git\\Seine\\Data\\midge_adult.png")#system.file("img", "midge_adult.png", package="png"))
# g <- rasterGrob(img, interpolate=TRUE)
# 
# 
# p2 = ggplot(dat, aes(y = y, x = x * 0.621371))+
#   geom_line(size = 2) +
#   geom_line(data = bug, aes(y = y2, x = x * 0.621371), color = "red", size = 2) +
#   scale_x_continuous(limits = c(0,220)) +
#   annotate("text", x = 150, y = 150, label = "Average Midge Abundance",
#            color = "red", size = 8) +
#   annotation_custom(g, xmin=200, xmax=220, ymin=135, ymax=155) +
#   labs(y = "water @ dusk", x = "River Mile") +
#   theme(legend.position = "none",
#         title = element_text(size = 18),
#         strip.text.x = element_text(size = 18),
#         axis.title = element_text(size = 18),
#         axis.text.x = element_text(size = 16, colour = "black"),
#         axis.text.y = element_text(size = 16, colour = "black"),
#         strip.text = element_text(vjust = 1),
#         strip.background = element_blank(),
#         panel.spacing = unit(.5, "lines"),
#         panel.grid.major = element_blank(),
#         panel.grid.minor = element_blank(),
#         panel.background = element_rect(fill = "white"),
#         panel.border = element_rect(colour = "black", fill = NA),
#         axis.line = element_line(colour = "black"))
# p2
# 
