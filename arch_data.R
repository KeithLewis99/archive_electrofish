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
## see electro_fish.xlsx in main data folder for progress and Waldron report for River codes
## see also NL River Codes_updated 2024.xlsx (worksheet: pivot) in main restoration folder

remove <- c(1416821, 2215270,
            2216821, 2217260) # these rivers have no River Code in the above file or in the old Waldron codes.  I can't match them to anything. Most are 1 or 2 years.  The last two are three years; 2416821 has 4 years - likely Waterford; 
## asked Clarke and Kristin about these - may be able to salvage a few.
 
df_all <- df_all |>
   filter(! River_Code %in% remove)

# rivers ----
# make a csv for teh River_Code and River Names in electro_fish.xlsx
## join them to df_all

river_codes <- read.csv("../../NL_river_names_codes/NL_river_codes_updated_2024.csv")
str(river_codes)
river_codes$RIVER.CODE <- as.integer(river_codes$RIVER.CODE)

row.names(river_codes[!unique(river_codes$RIVER.CODE),])
river_codes <- river_codes |>
   filter(!is.na(RIVER.CODE))
df_all <- left_join(df_all, river_codes, by = c("River_Code" = "RIVER.CODE"))
str(df_all, give.attr = F)

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



