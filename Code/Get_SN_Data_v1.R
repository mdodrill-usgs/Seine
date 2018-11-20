###############################################################################
#                                                                        Aug 18
#           Extracting Fish Data for 2 Stage MultSp Removal Model 
#
#  Notes:
#  * Now uses the R package, fishR !
#  * Table names are in lower case
#
#  To Do:
#  * Find 2014, 2017 data...
#
###############################################################################
# setwd('C:/Users/mdodrill/Desktop/FHM/DATA/')
rm(list = ls(all = TRUE))

library(fishR)

library(ggplot2)
library(ggthemes)
library(dplyr)
library(tidyr)
# library(RSQLite)

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
# summary table of catch by size bin 

# Only August - October
dat$month = substr(dat$trip_id, 7, 8)
dat2 = dat[which(dat$month %in% c("08", "09", "10")),]

# only species that we have removal data for
u.sp = c("BBH", "BHS", "CRP", "FHM", "FMS", "HBC", "PKF", "RBT", "RSH", "SPD", "SUC")

dat3 = dat2[which(dat2$species_code %in% u.sp),]


# dat3 = dat2[which(dat2$year == "2016"),]

#--------------------------------------

catch = group_by(dat3, year, sample_id, species_code) %>%
  filter(!is.na(species_code)) %>%
  summarise(tot.catch = n()) %>%
  ungroup() #%>%
# complete(trip_id, sz.class, fill = list(tot.catch = 0))
# complete()

# not sure if this is totally correct................................CHECK!
# catch2 = complete(catch, expand(catch, nesting(trip_id, turbidity)), sz.class, fill = list(tot.catch = 0))
# I think this is what we want...
# catch2 = complete(catch, expand(dat2, nesting(trip_id, turbidity)), sz.class, fill = list(tot.catch = 0))

# catch2 = catch[catch$sz.class == "<=150",]

# eff = group_by(dat2, trip_id, turbidity) %>%
#   filter(!duplicated(sample_id)) %>%
#   summarise(tot.eff.min = sum(ef_total_minutes))

#-----------------------------------------------------------------------------#
ltl.dat = select(sa3, sample_id, start_rm)


#-----------------------------------------------------------------------------#
cat = left_join(catch, ltl.dat, by = c("sample_id"))


# write.csv(cat, file = "SN_2016_data.csv", row.names = F)
DBI::dbDisconnect(my_db)
rm(list = setdiff(ls(), "cat"))
#-----------------------------------------------------------------------------#
# End