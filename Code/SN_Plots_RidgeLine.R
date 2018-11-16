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


all.sn = c('BL','BS','BX','SA','SB','SC','SEN','SG','SL','SS','SX','SN')

sa1 = select(db_fish_samp, one_of(samp.cols)) %>% 
  filter(gear_code %in% all.sn,
         !is.na(start_rm),
         start_rm >= 0,
         river_code == "COR",
         hydraulic_code == "BA")     

sa2 = collect(sa1)


#--------------------------------------
# subset for year, but now, looks like its 2000 - present.
sa2$year = substr(sa2$start_datetime, 1, 4)
unique(sa2$year)

sa3 = sa2[which(sa2$year %in% as.character(2000:2018)),]

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
dat.2 = dat[which(dat$total_length >=5 ),]

dat.3 = dat.2[dat.2$species_code %in% c("HBC", "FMS"),]

# dat.4 = dat.3[dat.3$total_length <= 100,]




p = ggplot(dat.3, aes(x = total_length, y = year, fill = species_code)) +
  # geom_density_ridges(fill = "dodgerblue", alpha = .2)+
  geom_density_ridges()+
  scale_fill_manual(values = c("#D55E0050", "#0072B250"), labels = c("female", "male")) +
  scale_color_manual(values = c("#D55E00", "#0072B2"), guide = "none") +
  # geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_x_continuous(expand = c(0.01, 0), limits = c(0,300)) +
  scale_y_discrete(expand = c(0.01, 0)) +
  # scale_fill_viridis(name = "s_river_mile", option = "C") +
  labs(y = "", x = "Total Length (mm)", title = "Flannelmouth Sucker")
# labs(y = "", x = "Total Length (mm)", title = "Humpback Chub")
p


p = ggplot(dat.3, aes(x = total_length, y = year, fill = ..x..)) +
  # geom_density_ridges(aes(fill = site)) 
  geom_density_ridges_gradient(scale = 3, rel_min_height = 0.01) +
  scale_x_continuous(expand = c(0.01, 0), limits = c(0,300)) +
  scale_y_discrete(expand = c(0.01, 0)) +
  scale_fill_viridis(name = "s_river_mile", option = "C") +
  labs(y = "", x = "Total Length (mm)", title = "Flannelmouth Sucker")
# labs(y = "", x = "Total Length (mm)", title = "Humpback Chub")
p

p2 = p + theme_base()

g = p2 + theme(panel.grid.major.y = element_line(colour = "gray"),
               panel.grid.minor  = element_line(colour = "white"),
               axis.title.x = element_text(size = 14, vjust = -.1),
               axis.title.y = element_text(size = 14, vjust = 1),
               axis.text.x = element_text(size = 12, colour = "black"),
               axis.text.y = element_text(size = 12, colour = "black"),
               legend.position = "none")

g






library(DAAG) # for ais dataset
ais$sport <- factor(
  ais$sport,
  levels = c("B_Ball", "Field", "Gym", "Netball", "Row", "Swim", "T_400m", "T_Sprnt", "Tennis", "W_Polo"),
  labels = c("Basketball", "Field", "Gym", "Netball", "Row", "Swim", "Track 400m", "Track Sprint", "Tennis", "Water Polo")
)

ggplot(ais, aes(x=ht, y=sport, color=sex, point_color=sex, fill=sex)) +
  geom_density_ridges(
    jittered_points=TRUE, scale = .95, rel_min_height = .01,
    point_shape = "|", point_size = 3, size = 0.25,
    position = position_points_jitter(height = 0)
  ) +
  scale_y_discrete(expand = c(.01, 0)) +
  scale_x_continuous(expand = c(0, 0), name = "height [cm]") +
  scale_fill_manual(values = c("#D55E0050", "#0072B250"), labels = c("female", "male")) +
  scale_color_manual(values = c("#D55E00", "#0072B2"), guide = "none") +
  scale_discrete_manual("point_color", values = c("#D55E00", "#0072B2"), guide = "none") +
  guides(fill = guide_legend(
    override.aes = list(
      fill = c("#D55E00A0", "#0072B2A0"),
      color = NA, point_color = NA))
  ) +
  ggtitle("Height in Australian athletes") +
  theme_ridges(center = TRUE)







