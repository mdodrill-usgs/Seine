###############################################################################
#                                                                        Aug 18
#           Extracting Fish Data (Counts) for Neg. Binom Models
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
              "substrate_code",
              "cover_code",
              "turbidity",
              "water_temp",
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

# Only August - October
sa3$month = substr(sa3$trip_id, 7, 8)
sa4 = sa3[which(sa3$month %in% c("08", "09", "10")),]

#--------------------------------------
# organize the temps cols.   (figure out how to apply this)


# sa4$test = apply(sa4,1,temp.fixer)
# sa4$test

sa4$temp_bkw = apply(sa4, 1, temp.fixer)

#--------------------------------------
# # river temps...


#-----------------------------------------------------------------------------#
# set up fish specimens

spec.cols = c("sample_id",
              "species_code",
              "total_length",
              "fork_length", 
              "disposition_code")

# only common species for now...
# u.sp = c("BBH", "BHS", "CRP", "FHM", "FMS", "HBC", "PKF", "RBT", "RSH", "SPD", "SUC")
u.sp = c("BHS", "FHM", "FMS", "HBC", "SPD")

sp1 = select(db_fish_spec, one_of(spec.cols)) %>%
  filter(species_code %in% u.sp)

sp2 = collect(sp1)

#-----------------------------------------------------------------------------#
# Join the tables

# We want all the samples, including the samps where no fish were captured, so
# left_join
dat = left_join(sa4, sp2, by = "sample_id")
dat



#-----------------------------------------------------------------------------#
# summary table of catch by size bin 

catch = group_by(dat, sample_id, species_code) %>%
  dplyr::summarise(tot.catch = n()) %>%
  ungroup() %>%
  complete(sample_id, species_code, fill = list(tot.catch = 0)) %>%
  filter(!is.na(species_code))  # order of when you filter matters here

#-----------------------------------------------------------------------------#
# could cut sample data down here, befor joining below...?
ltl.dat = select(sa4, sample_id, start_rm, year, temp_bkw)

cat = left_join(catch, ltl.dat, by = c("sample_id"))

# leave out a couple years with only youth data
cat = cat[which(!cat$year %in% c("2012", "2013")),]

# leave out the samples below diamond
cat = cat[which(cat$start_rm <= 225),]

# write.csv(cat, file = "SN_2016_data.csv", row.names = F)
DBI::dbDisconnect(my_db)
rm(list = setdiff(ls(), c("cat", "dat")))
#-----------------------------------------------------------------------------#
# End