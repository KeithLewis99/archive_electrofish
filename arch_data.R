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
            2216821, 2217260,
            2416811, 2416821,
            3614071, 3614072) # these rivers have no River Code in the above file or in the old Waldron codes.  I can't match them to anything. Most are 1 or 2 years.  The last two are three years; 2416821 has 4 years - likely Waterford; 
## asked Clarke and Kristin about these - may be able to salvage a few.
 
df_all <- df_all |>
   filter(! River_Code %in% remove)

# make a csv for teh River_Code and River Names in electro_fish.xlsx
## join them to df_all

river_codes <- read.csv("river_codes.csv")

left_join(df_all, river_codes, by = River_Code = river_codes)


# sum catch ----
## first, create a table for abun = T (total catch) and biomass
df_sum <- df_all |>
   group_by(Year, 
            Month, 
            Station, 
            Species, 
            Sweep_Number) |>
   summarise(abun = n()) 
str(df_sum, give.attr = F)

# filter records with weight and repeat above - this will give 
df_sum_bio <- df_all |>
   group_by(Year, 
            Month, 
            Station, 
            Species, 
            Sweep_Number) |>
   filter(!is.na(Weight)) |>
   summarise(bio.sum = sum(Weight, na.rm = T),
             abun = n()) 

# Calculate T ----
## look for any issues such as lots of sweeps etc.



