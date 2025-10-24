# Set up a project - see the below link for directions.
#https://happygitwithr.com/rstudio-git-github.html

# But basically:
# 1.	Set up a Git repo on GitHub.
# 2.	Create the project in R - New Project - Version Control - Git
# 3. type "git add -A" in the terminal
# 4.	Create a bunch of directories automatically (see below)
# 5. Copy git -ignore file

#Create a "name_dat.R" file
#put this file in the folder with the project and create the following subfolders
if(!dir.exists("archive"))dir.create("archive")
if(!dir.exists("data"))dir.create("data")
if(!dir.exists("data_derived"))dir.create("data_derived") # derived data sets
if(!dir.exists("figs"))dir.create("figs") #for publication quality only
if(!dir.exists("output"))dir.create("output") # for tables and figures
if(!dir.exists("ms"))dir.create("ms") # manuscript
if(!dir.exists("report"))dir.create("report") #for rmd report
if(!dir.exists("refs"))dir.create("refs") #for rmd report



# import ----

tmp <- read.csv("../data/electro_fish.csv")
str(tmp) # ~37K records
length(tmp$Weight[is.na(tmp$Weight)]) # 13.7K are NA
tmp[is.na(tmp$River_Code),] # row 19904
df_all <- tmp[, c(1:9, 12, 14, 17)]
str(df_all)


# Clean ----
# convert so that numeric codes make sense
library(dplyr)
unique(df_all$Species)

df_all$Species <- as.factor(df_all$Species)
## ll = landlocked or ouananiche; an = anadramous
## need cut offs for YOY from length
levels(df_all$Species) <- c("4", "AS-ll", "AS-an", "BnT", "BT", "700")


# Station - need Clarke for this

## rivers ----
## see electro_fish.xlsx in main data folder for progress and Waldron report for River codes
## see also NL River Codes_updated 2024.xlsx (worksheet: pivot) in main restoration folder

remove <- c(1416821, 2216821, 2217260, 2216270, 707790) # these rivers have no River Code in the above file or in the old Waldron codes.  I can't match them to anything. Most are 1 or 2 years.  The last two are three years; 2416821 has 4 years - likely Waterford; 
# 707790 is Exploits but there are only 4 records (707798 is Pamehac)
# 2215270 is clearly Seal Cove - its a typo (see below)

# But I think that we should delete Seal Cove - there should be ~ 1500 records (see Seal Cove data: sealcove1994bysite.csv) and there are < 30.  So eliminate and bring in that one as required; we have this so why try hard to make it work here???
# this code shows the problem
df_all$Station[df_all$River_Code == "2216270" & df_all$Station == ""]
df_all <- df_all |>
   filter(!(River_Code == "2216270" & Station == ""))

df_all$River_Code[df_all$River_Code == 2215270] <- as.integer(2216270)
# note that there is a River_Code == "2215270" which must be a typo for Seal Cove; there is no value in the River Codes file and its right in the middle of the Seal Cove data in the electrofish.csv file

nrow(df_all |> filter(River_Code == 4402420))
## asked Clarke and Kristin about these - may be able to salvage a few.
nrow(df_all)
df_all <- df_all |>
   filter(! River_Code %in% remove)

# join rivers ----
# made a csv for teh River_Code and River Names in electro_fish.xlsx
## join them to df_all

river_codes <- read.csv("../../NL_river_names_codes/NL_river_codes_updated_2024.csv")
str(river_codes)
river_codes$RIVER.CODE <- as.integer(river_codes$RIVER.CODE)

river_codes[is.na(river_codes$RIVER.CODE),]

river_codes <- river_codes |>
   filter(!is.na(RIVER.CODE))

# join so that rivers have a name
df_all <- left_join(df_all, river_codes, by = c("River_Code" = "RIVER.CODE"))
str(df_all, give.attr = F)

## clean NA ----
# remove NAs for rivers
df_all <- df_all |>
   filter(!is.na(River_Code)) #remove one row with is.na(River_Code)
unique(df_all$RIVER.NAME)
unique(df_all$River_Code)

# This shows the River.names with NA: these are just tributaries of the larger rivers
df_all |>
   group_by(River_Code, RIVER.NAME) |>
   summarise(first(River_Code)) 

df_all$RIVER.NAME[df_all$River_Code == 2416811 & is.na(df_all$RIVER.NAME)]<- "Quidi Vidi River"

df_all$RIVER.NAME[df_all$River_Code == 2416821 & is.na(df_all$RIVER.NAME)]<- "Waterford River"

df_all$RIVER.NAME[df_all$River_Code == 3614071 & is.na(df_all$RIVER.NAME)]<- "Salmon River (Bay D’Est)"
df_all$RIVER.NAME[df_all$River_Code == 3614072 & is.na(df_all$RIVER.NAME)]<- "Salmon River (Bay D’Est)"

# this is almost certinaly a typo; only entry in archive and embedded in Corner Book; note that there are three Corner Brook rivers (0403140 (Sops Arm), 0604990 (no idea but in SFA 4), 4402420 (Humber Arm)) 
df_all$RIVER.NAME[df_all$River_Code == 4403420 & is.na(df_all$RIVER.NAME)]<- "Corner Brook"
df_all$River_Code[df_all$River_Code == 4403420 & df_all$RIVER.NAME == "Corner Brook"] <- 4402420

# OK - so all checks out - proceed
unique(df_all$River_Code)
df_all |>
   group_by(River_Code, RIVER.NAME) |>
   summarise(first(River_Code)) 

## years ----
# Nas for years and fix names
df_all[is.na(df_all$Year),] # Gander River and Salon River(Hare Bay) don't have Years for 41 rows out of 4218
nrow(df_all[is.na(df_all$Year),])
as.character(unique(df_all$Year))

df_all[df_all$Station == "", c(2:6, 13)]

df_all |> 
   filter(RIVER.NAME == "Gander River" | 
          RIVER.NAME == "Salmon River (Hare Bay)") |>
   select(River_Code, RIVER.NAME, Year, Species)
df_all[df_all$River_Code == "908610", ] # Gander River: based on this and a manual search, its seems very likely that all of the missing years were 1992
df_all[df_all$River_Code == "300850", ] # Salmon River (Hare Bay): based on this and a manual search, its seems very likely that all of the missing years were 1993
df_all$Year[df_all$River_Code == "908610" & is.na(df_all$Year)] <- "92"
df_all$Year[df_all$River_Code == "300850" & is.na(df_all$Year)] <- "93"

# change all years "19XX"
df_all$Year <- paste0("19", df_all$Year)

unique(df_all$Year)

## Stations ----
# Stations - fix blanks
unique(df_all$Station)
df_all[df_all$Station == "", c(2:6, 13)]
# for Gander River, seems very likely this is 01
df_all$Station[df_all$River_Code == "908610" & df_all$Station == ""] <- "01"

# see above for Seal Cove - not worth trying to revive this so just delete

# Corner Brook
# one record clearly a typo
df_all[df_all$Station == "" & df_all$River_Code != "4402420", c(2:6, 13)]


# 555 records for Corner Book 1994 but no stations - all in June of 1994
df_all[df_all$Station == "" & df_all$River_Code == "4402420", c(2:6, 13)]
nrow(df_all[df_all$Station == "" & df_all$River_Code == "4402420", c(2:6, 13)])

# notebooks?

## Species ----
unique(df_all$Species)
df_all |>
   filter(Species == "4")
# there's only 10 records with 4 so remove
df_all <- df_all |>
   filter(Species != "4")

# I went through the Akenhead and LeGrow 1981 report with the species code and "700" is striped wolffish!!!  Further, all of 127 records were from 1994-11-08 in Corner Brook on sweep 1.  So assume that this is BT which Clarke said was all that they had caught.
df_all$Species[df_all$Species == "700"] <- "BT"
unique(df_all$Species)

## Sweep ----
unique(df_all$Sweep_Number)

df_all[is.na(df_all$Sweep_Number), c(2:6, 13)]
df_all[is.na(df_all$Sweep_Number & df_all$River_Code != 4402420), c(2:6, 13)]

# all records with missing Sweep_Number are from Corner Brook
str(df_all, give.attr = F)

# need Clarke's data for this



# Summary ----
df_summary <- df_all |>
   group_by(River_Code, RIVER.NAME) |>
   summarize(minYear = min(Year), maxYear = max(Year))

write.csv(df_summary, "df_summary.csv")

# sum catch ----
## first, create a table for abun = T (total catch) and biomass
df_sum_T <- df_all |>
   group_by(River_Code,
            RIVER.NAME,
            Year, 
            Month, 
            Station, 
            Species, 
            Sweep_Number) |>
   summarise(abun = n()) 
str(df_sum_T, give.attr = F)

# calculate Sweep_number by Year and Station

df_sum_T |>
   group_by(River_Code, Year, Species) |> 
   summarise(maxSweep = max(Sweep_Number)) |>
   print(n = Inf)

# filter records with weight and repeat above - this will give 

df_sum_bio <- df_all |>
   group_by(River_Code,
            RIVER.NAME,
            Year, 
            Month, 
            Station, 
            Species, 
            Sweep_Number) |>
   filter(!is.na(Weight)) |>
   summarise(bio.sum = sum(Weight, na.rm = T),
             abun = n()) 
str(df_sum_bio, give.attr = F)

# should joing these back

df_sum <- left_join(df_sum_T, df_sum_bio, 
                    by = c("River_Code",
                           "RIVER.NAME",
                           "Year", 
                           "Month", 
                           "Station", 
                           "Species", 
                           "Sweep_Number")) |>
   rename(abun = abun.x, abu_bio = abun.y)
str(df_sum, give.attr = F)
length(df_sum$bio.sum[is.na(df_sum$bio.sum)]) # 221 bio.sum are NA

unique(df_sum$RIVER.NAME)
unique(df_sum$River_Code)

# I think that the thing to do here is to have a dens
## but I can't get density for most of these because we won't have area
### is this even worth doing?  I think its something but nothing will be comparable

# grid ----
# create dataset with all possible combinations of the following variables
# there NA's or blanks in all of these
river <- unique(df_sum$River_Code)
year <- as.character(unique(df_sum$Year))
station <- unique(df_sum$Station)
species <- c("AS", "ASYOY", "BT", "BTYOY")
sweep <- c(1:max(df_sum$Sweep_Number))

# make grid
df_grid <- expand.grid(Year = year, 
                       Species = species,
                       Station = station,
                       Sweep = sweep) |> 
   arrange(Year, Species, Station, Sweep)
#str(df_grid)

# write.csv(df_grid, "data_derived/df_grid.csv")

# edit grid ----

# get max by Year and Station
df_sweep <- df_sum |> 
   group_by(Year, Station) |>
   summarise(max_sweep = max(Sweep)) |> 
   pivot_wider(id_cols = Year,
               names_from = Station,
               values_from = max_sweep)

# Calculate T ----
## look for any issues such as lots of sweeps etc.



