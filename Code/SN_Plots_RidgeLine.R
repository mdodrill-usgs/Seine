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
library(ggridges)
library(RColorBrewer)
library(fishR)

windows(record = TRUE, xpos = 25, height = 9, width = 12)
#-----------------------------------------------------------------------------#
# connect to db
# my_db = src_sqlite(path = db_path, create = FALSE)
my_db = connect_fish_db(update.db = FALSE)

# get the tables 
db_fish_samp = tbl(my_db, "samp")
db_fish_spec = tbl(my_db, "spec")
#-----------------------------------------------------------------------------#
# set up fish samples

samp.cols = c("sample_id",
              "sample_type",
              "trip_id",
              "start_datetime",
              "end_datetime", 
              "sample_type",
              "river_code",
              "start_rm",
              # "end_rm",
              # "start_rkm", 
              # "end_rkm",
              "gear_code",
              # "ef_total_seconds",
              "effort",
              # "total_catch", 
              "seine_haul_length",
              "seine_total_area",
              "seine_hab_width",
              "seine_hab_length", 
              "seine_depth_1",
              "seine_depth_2",
              "seine_max_depth",
              "depletion_num", 
              "habitat_code",
              "hydraulic_code",
              # "substrate_code",
              # "cover_code", 
              "turbidity",
              # "water_temp",
              # "air_temp",
              "sample_notes") #,
# "station_id")


all.sn = c('BL','BS','BX','SA','SB','SC','SEN','SG','SL','SS','SX','SN', 'EL', 'HB', "HS")
# all.sn = c('BL','BS','BX','SA','SB','SC','SEN','SG','SL','SS','SX','SN')
# all.sn = c('EL')

sa1 = select(db_fish_samp, one_of(samp.cols)) %>% 
  filter(gear_code %in% all.sn,
         !is.na(start_rm),
         start_rm >= 0,
         river_code == "COR")
         # hydraulic_code == "BA")     

sa2 = collect(sa1)


#--------------------------------------
# subset for year, but now, looks like its 2000 - present.
sa2$year = substr(sa2$start_datetime, 1, 4)
# unique(sa2$year)

# sa3 = sa2[which(sa2$year %in% as.character(2000:2018)),]
# unique(sa3$year)

# only years that have SN data
years = c("2000", "2002", "2003", "2004", "2005", "2006", "2007", "2008", 
          "2009", "2012", "2013", "2014", "2016", "2015", "2017", "2018")

sa3 = sa2[which(sa2$year %in% years),]

#-----------------------------------------------------------------------------#
# set up fish specimens

spec.cols = c("sample_id",
              "species_code",
              "total_length",
              "fork_length", 
              "disposition_code")


sp1 = select(db_fish_spec, one_of(spec.cols)) #%>%
# filter(species_code %in% "FMS",
#        !is.na(total_length))

sp2 = collect(sp1)

#-----------------------------------------------------------------------------#
# Join the tables

# We want all the samples, including the samps where no fish were captured, so
# left_join
dat = left_join(sa3, sp2, by = "sample_id")
dat


# There are a lot of total lengths of 0 or 1, need to fix or exclude...

#-----------------------------------------------------------------------------#
dat$gear = ifelse(dat$gear_code == "EL", "E Fishing", 
                  ifelse(dat$gear_code %in% c("HB", "HS"), "Hoop", "Seine"))

dat.2 = dat[which(dat$total_length >= 10),]

# dat.3 = dat.2[dat.2$species_code %in% c("HBC", "FMS"),]
dat.3 = dat.2[dat.2$species_code %in% c("HBC"),]

# dat.4 = dat.3[dat.3$total_length <= 100,]
d.text = select(dat.3, year, gear, species_code, total_length) %>%
  group_by(year, gear) %>%
  summarise(count = n(), median = median(total_length))



# display.brewer.pal(11,"Spectral");brewer.pal(11,"Spectral")

p = ggplot(dat.3, aes(x = total_length, y = year, fill = gear)) +
  geom_density_ridges(alpha = .85)+
  # scale_fill_brewer(palette = "Spectral") +
  scale_fill_manual(values = c("#F46D43", "#FDAE61", "#3288BD")) +
  # scale_color_manual(values = c("#F46D43", "#FDAE61", "#3288BD"), guide = "none") +
  scale_x_continuous(expand = c(0.01, 0), limits = c(0,525)) +
  scale_y_discrete(expand = c(0.01, 0)) +
  # geom_text(data = d.text, aes(y = year, x = 420, color = gear,
  #                              label = paste0("n = ",as.character(count)))
  #           , vjust = -.5, hjust = 1, size = 5, position = position_dodge(20)) +
  
  geom_text(data = d.text[d.text$gear == "E Fishing",],
            aes(y = year, x = 410, label = paste0("n = ", as.character(count))),
            vjust = -.5, hjust = 1, color = "#F46D43",
            size = 5) +
  geom_text(data = d.text[d.text$gear == "Hoop",],
            aes(y = year, x = 470, label = paste0("n = ", as.character(count))),
            vjust = -.5, hjust = 1, color = "#FDAE61",
            size = 5) +
  geom_text(data = d.text[d.text$gear == "Seine",],
            aes(y = year, x = 520, label = paste0("n = ", as.character(count))),
            vjust = -.5, hjust = 1, color = "#3288BD",
            size = 5) +
  
  labs(y = "", x = "Total Length (mm)", title = "Insert Title Here")
p


p2 = p + theme_base()
# p2 = p + ggthemes::theme_solarized_2(light = FALSE) 

g = p2 + theme(panel.grid.major.y = element_line(colour = "gray"),
               panel.grid.minor  = element_line(colour = "white"),
               axis.title.x = element_text(size = 14, vjust = -.1),
               axis.title.y = element_text(size = 14, vjust = 1),
               axis.text.x = element_text(size = 12, colour = "black"),
               axis.text.y = element_text(size = 12, colour = "black"),
               legend.position = c(.85,.975),
               # legend.key.width = unit(1, "cm"),
               panel.border = element_blank(),
               panel.background = element_blank(),
               axis.line = element_line(colour = "black"),
               legend.title = element_blank()) +
               guides(color = guide_legend(nrow = 1), fill = guide_legend(nrow = 1))

g 


#-----------------------------------------------------------------------------#
# Just Seine
dat.2 = dat[which(dat$total_length >=10 ),]

dat.3 = dat.2[dat.2$species_code %in% c("HBC"),]

dat.4 = dat.3[dat.3$gear == "Seine",]

d.text = select(dat.4, year, gear, species_code) %>%
         group_by(year, gear) %>%
         summarise(count = n())


p = ggplot(dat.4, aes(x = total_length, y = year, fill = gear)) +
  geom_density_ridges(alpha = .75)+
  scale_fill_manual(values = c("#3288BD")) +
  # scale_color_manual(values = c("#3288BD"), guide = "none") +
  scale_x_continuous(expand = c(0.01, 0), limits = c(0,525)) +
  scale_y_discrete(expand = c(0.01, 0)) +
  geom_text(data = d.text, aes(y = year, x = 420,
                               label = paste0("n = ",as.character(count))),
            color = "#3288BD", vjust = -.5, hjust = 1, size =5) +
  labs(y = "", x = "Total Length (mm)", title = "Humpback Chub")
p


p2 = p + theme_base()

g = p2 + theme(panel.grid.major.y = element_line(colour = "gray"),
               panel.grid.minor  = element_line(colour = "white"),
               axis.title.x = element_text(size = 14, vjust = -.1),
               axis.title.y = element_text(size = 14, vjust = 1),
               axis.text.x = element_text(size = 12, colour = "black"),
               axis.text.y = element_text(size = 12, colour = "black"),
               panel.border = element_blank(),
               panel.background = element_blank(),
               axis.line = element_line(colour = "black"),
               legend.position = c(.7,.975),
               legend.title = element_blank()) +
  guides(color = guide_legend(nrow = 1), fill = guide_legend(nrow = 1))

g



#-----------------------------------------------------------------------------#
#-----------------------------------------------------------------------------#
# Seine + hoops
dat.2 = dat[which(dat$total_length >=10 ),]

dat.3 = dat.2[dat.2$species_code %in% c("HBC"),]

dat.4 = dat.3[dat.3$gear %in% c("Seine", "Hoop"),]

d.text = select(dat.4, year, gear, species_code) %>%
         group_by(year, gear) %>%
         summarise(count = n())


p = ggplot(dat.4, aes(x = total_length, y = year, fill = gear)) +
  geom_density_ridges(alpha = .75)+
  scale_fill_manual(values = c("#FDAE61", "#3288BD")) +
  # scale_color_manual(values = c("#3288BD"), guide = "none") +
  scale_x_continuous(expand = c(0.01, 0), limits = c(0,525)) +
  scale_y_discrete(expand = c(0.01, 0)) +
  # geom_text(data = d.text, aes(y = year, x = 420,
  #                              label = paste0("n = ",as.character(count))),
  #           color = "#3288BD", vjust = -.5, hjust = 1, size =5) +
  geom_text(data = d.text[d.text$gear == "Seine",],
            aes(y = year, x = 520, label = paste0("n = ", as.character(count))),
            vjust = -.5, hjust = 1, color = "#3288BD",
            size = 5) +
  labs(y = "", x = "Total Length (mm)", title = "Humpback Chub")
p


p2 = p + theme_base()

g = p2 + theme(panel.grid.major.y = element_line(colour = "gray"),
               panel.grid.minor  = element_line(colour = "white"),
               axis.title.x = element_text(size = 14, vjust = -.1),
               axis.title.y = element_text(size = 14, vjust = 1),
               axis.text.x = element_text(size = 12, colour = "black"),
               axis.text.y = element_text(size = 12, colour = "black"),
               panel.border = element_blank(),
               panel.background = element_blank(),
               axis.line = element_line(colour = "black"),
               legend.position = c(.7,.975),
               legend.title = element_blank()) +
  guides(color = guide_legend(nrow = 1), fill = guide_legend(nrow = 1))

g



#-----------------------------------------------------------------------------#
# for coop 

ltl.dat = dat.4[dat.4$year == "2018",]


p = ggplot(ltl.dat, aes(x = total_length)) +
    geom_histogram(fill = c("#3288BD"), alpha = .75, color = "black", binwidth = 4) +
    geom_density(fill = c("#3288BD"), alpha = .5, aes(y = 4 * ..count..)) +
    scale_x_continuous(breaks = seq(20, 100, 10)) +
    annotate("text",label = "Total Count = 253", x = 22, y = 70, size = 6) +
    labs(y = "Count", x = "Total Length (mm)", title = "Humpback Chub - 2018")

p


g = p + theme(panel.grid.major.y = element_line(colour = "white"),
               panel.grid.minor  = element_line(colour = "white"),
               axis.title.x = element_text(size = 18, vjust = -.1),
               axis.title.y = element_text(size = 18, vjust = 1),
               axis.text.x = element_text(size = 16, colour = "black"),
               axis.text.y = element_text(size = 16, colour = "black"),
               title = element_text(size = 20), 
               panel.border = element_blank(),
               panel.background = element_blank(),
               axis.line = element_line(colour = "black"))

g

#-----------------------------------------------------------------------------#