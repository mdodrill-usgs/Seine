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
