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

windows(record = TRUE, xpos = 25, height = 12, width = 10)
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

dat.2 = dat[which(dat$total_length >=5 ),]

# dat.3 = dat.2[dat.2$species_code %in% c("HBC", "FMS"),]
dat.3 = dat.2[dat.2$species_code %in% c("HBC"),]

# dat.4 = dat.3[dat.3$total_length <= 100,]


# display.brewer.pal(11,"Spectral");brewer.pal(11,"Spectral")

p = ggplot(dat.3, aes(x = total_length, y = year, fill = gear)) +
  geom_density_ridges(alpha = .75)+
  scale_fill_manual(values = c("#F46D43", "#ABDDA4", "#3288BD")) +
  scale_color_manual(values = c("#F46D43", "#ABDDA4", "#3288BD"), guide = "none") +
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_y_discrete(expand = c(0.01, 0)) +
  labs(y = "", x = "Total Length (mm)", title = "Insert Title Here")
p


p2 = p + theme_base()

g = p2 + theme(panel.grid.major.y = element_line(colour = "gray"),
               panel.grid.minor  = element_line(colour = "white"),
               axis.title.x = element_text(size = 14, vjust = -.1),
               axis.title.y = element_text(size = 14, vjust = 1),
               axis.text.x = element_text(size = 12, colour = "black"),
               axis.text.y = element_text(size = 12, colour = "black"),
               legend.position = c(.7,.975),
               legend.title = element_blank()) +
               guides(color = guide_legend(nrow = 1), fill = guide_legend(nrow = 1))

g


#-----------------------------------------------------------------------------#
dat.2 = dat[which(dat$total_length >=5 ),]

dat.3 = dat.2[dat.2$species_code %in% c("HBC"),]

dat.4 = dat.3[dat.3$gear == "Seine",]

tab = table(dat.4$year)

d.text = data.frame(year = attributes(tab)$dimnames[[1]],
                    lab = tab,
                    gear = NA)

p = ggplot(dat.4, aes(x = total_length, y = year, fill = gear)) +
  geom_density_ridges(alpha = .75)+
  scale_fill_manual(values = c("#3288BD")) +
  # scale_color_manual(values = c("#3288BD"), guide = "none") +
  scale_x_continuous(expand = c(0.01, 0)) +
  scale_y_discrete(expand = c(0.01, 0)) +
  geom_text(data = d.text, aes(y = year, x = 400,
                               label = paste0("n = ",as.character(lab.Freq))),
            color = "#3288BD", vjust = -1.5) +
  labs(y = "", x = "Total Length (mm)", title = "Humpback Chub")
p


p2 = p + theme_base()

g = p2 + theme(panel.grid.major.y = element_line(colour = "gray"),
               panel.grid.minor  = element_line(colour = "white"),
               axis.title.x = element_text(size = 14, vjust = -.1),
               axis.title.y = element_text(size = 14, vjust = 1),
               axis.text.x = element_text(size = 12, colour = "black"),
               axis.text.y = element_text(size = 12, colour = "black"),
               legend.position = c(.7,.975),
               legend.title = element_blank()) +
  guides(color = guide_legend(nrow = 1), fill = guide_legend(nrow = 1))

g




#-----------------------------------------------------------------------------#