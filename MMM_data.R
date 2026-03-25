# After a great deal of reflection (mid-March, 2026), I have decided that all the work on bootstrapping and the delta method is just not appropriate.  See the tech report.  

# Briefly, the density and biomass estimates for each time series often contain many zeros and a wide range of values (e.g., high variance) values, e.g., GC.  As such, the lower confidence levels for density and biomass estimates are often less than zero when calculated by assuming a normal distribution but the uppper limit is very, very large when using the log-normal.  Bootstrap is biased bc of low sample sizes.  The same issues apply with the Delta Method.  

# Therefore, simply present the minimum and maximum along with a mean if the sample size warranted (MMM).  

## this file is for the study areas with the raw data available



# Data rank 1 ----
library(dplyr)
library(tidyr)

# Stoney Brook ----
## import ----
df_sb <- read.csv("../data/stoneybrook/stoney2001_2003.csv")
str(df_sb)

unique(df_sb$Species) # "AS"      "BT"      "BN"      "SMELT"   "ASYOY"   "BNYOY"   "EEL"    "BTYOY"   "EELS"    "STICKLE"
unique(df_sb$Site) # all "Stoney"
unique(df_sb$Station) # all OK - "R1"   "R2"   "R3"   "R4"   "F1"   "F2"   "POOL"

## clean ----
# filter out unneeded species
species_filter <- c("BN", "SMELT", "BNYOY", "EEL", "EELS", "STICKLE")
df_sb <- df_sb |> filter(!Species %in% species_filter)

## summarize ----
df_sum <- df_sb |>
   filter(Sweep <= 3) |>
   group_by(Year, Species, Station, Area) |>
   summarise(bio.sum = sum(Weight.g, na.rm = T), abun = n()) |>
   mutate(abun.stand = abun/Area*100, bio.stand = bio.sum/Area*100)
write.csv(df_sum, "data_derived/MMM/SB_dage_2001_2003.csv", row.names = F)

sb_site <- df_sum |>
   group_by(Species, Year) |>
   summarise(n = n(),
             min_den = min(abun.stand), 
             max_den = max(abun.stand),
             mean_den = mean(abun.stand),
             sd_den = sd(abun.stand),
             min_bio = min(bio.stand),
             max_bio = max(bio.stand),
             mean_bio = mean(bio.stand),
             sd_bio = sd(bio.stand),
             min_bio = min(bio.stand)
   )
write.csv(sb_site, "data_derived/mmm/SB_site_2001_2003.csv", row.names = F)

sb_year <- df_sum |>
   group_by(Species) |>
   summarise(n = n(),
             min_den = min(abun.stand), 
             max_den = max(abun.stand),
             mean_den = mean(abun.stand),
             sd_den = sd(abun.stand),
             min_bio = min(bio.stand),
             max_bio = max(bio.stand),
             mean_bio = mean(bio.stand),
             sd_bio = sd(bio.stand),
             min_bio = min(bio.stand)
   )
write.csv(sb_year, "data_derived/mmm/SB_year_2001_2003.csv", row.names = F)



# Trepassey ----
# & Highlands ----
## import ----
# library(readxl)
df_HLTP <- read.csv("../data/Highlands_Trepassey_Compiled_AM_02052026.csv")

str(df_HLTP)

## check ----
# check values for all fields and clean as needed
unique(df_HLTP$Study_area) # "Trepassey" "Highland" 
df_HLTP$Study_area[is.na(df_HLTP$Study_area)] # 30 rows
df_HLTP[is.na(df_HLTP$Study_area),] # 30 rows

unique(df_HLTP$Species) #[1] "AS"        "BT"        "ASY"       "EEL"       "BTY"       "SB" "killifish" NA          "Other"    # FILTER THESE OUT BELOW
df_HLTP[df_HLTP$Species == "AS ", "Species"] <- "AS"

# length with ~X; 
df_HLTP[c(3410, 3581, 3623),] # all of these have no species name or EEL so OK; they will get filtered below

## clean ----
df_HLTP[is.na(df_HLTP$Weight.g),] # 30 rows
df_HLTP[!is.na(df_HLTP$Weight.g) & df_HLTP$Weight.g == "nw", "Weight.g"] <- NA # 4 with nw but printing NA
df_HLTP[!is.na(df_HLTP$Weight.g) & df_HLTP$Weight.g == "NW", "Weight.g"] <- NA# 4 with nw but printing NA
df_HLTP[!is.na(df_HLTP$Weight.g) & df_HLTP$Weight.g == "N\\A","Weight.g"] <- NA

# df_HL
unique(df_HLTP$Stn_name) # NA, "Railway Bridge", [3] "Gillams Farm"                    "Rainy Brook East", [5] "River Brook Lower"               "Rainy Brook" [7] "Main Stem above Rainy"           "Gilliams Farm"         [9] "Rainy"                           "Main Stem below highway (lower)"[11] "River Brook- Lower"             
## all good but lots of blanks but Stn_no has areas so all OK


unique(df_HLTP$Stn_no)

df_HLTP[is.na(df_HLTP$Area), ] # lots iwth no area

# filter out unneeded species
species_filter <- c("EEL","SB", "killifish", "", "Other")
df_HLTP <- df_HLTP |> filter(!Species %in% species_filter) 
str(df_HLTP)

## change data type
df_HLTP$Date <- as.Date(df_HLTP$Date)
df_HLTP$Length.mm <- as.numeric(df_HLTP$Length.mm)
df_HLTP$Weight.g <- as.numeric(df_HLTP$Weight.g)

# check for blanks in area
# df_HLTP |>
#    group_by(Study_area, Year, Stn_no) |>
#    summarise(blanks_nas_count = sum(is.na(Area)),
# non_blanks_count = sum(!is.na(Area)),
# .groups = 'drop'
# ) |>
#    print(n=Inf)

## add area ----
# create pivot table to show blanks

df_HLTP |>
   group_by(Study_area, Year, Stn_no) |> 
   summarise(area = mean(Area)) |>
   pivot_wider(names_from = Stn_no, values_from = area)
# no area measuremeans for Highlands 2013:50, 2015:5, and 2017:15
# no area measurements for Trepassey 2014 for all and & 2018 for 5 & 8 but should be fish for both HL and TP - perhaps average the sites if you can't find them in the notebooks

## from notebooks, the dimensions for Trepassey:2018: Stn_no 5&8 is
col_names <- c("feet", "inches")
s5 <- matrix(c(36, 34, 34,
               6, 10, 2), 
             3, 2)
site5 <-  as.data.frame(s5)
colnames(site5) <- col_names
site5$total_inches <- site5$feet*12 + site5$inches
site5$width.m <- site5$total_inches*0.0254
length <- (35*12 + 4)*0.0254 # site was 35' 4"
area5 <- mean(site5$width.m)*length

s8 <- matrix(c(13, 9, 8,
               8, 6, 2), 
             3, 2)
site8 <-  as.data.frame(s8)
colnames(site8) <- col_names
site8$total_inches <- site8$feet*12 + site8$inches
site8$width.m <- site8$total_inches*0.0254
length8 <- (120*12 + 3)*0.0254 # site was 35' 4"
area8 <- mean(site8$width.m)*length8

# insert values into dataframe
df_HLTP$Area[df_HLTP$Study_area == "Trepassey" & df_HLTP$Year == 2018 & df_HLTP$Stn_no == 5] <- area5
df_HLTP$Area[df_HLTP$Study_area == "Trepassey" & df_HLTP$Year == 2018 & df_HLTP$Stn_no == 8] <- area8


# average other values
## after talking with Curtins, he felt that the sites should be very invariant so averagging seems like the way to go.  Indeed, looking at the pivot table from above, som sites vary by 1m^2 while others is over 100 - still, should have small influence on density

# just to see averages
df_HLTP |>
   group_by(Study_area, Stn_no) |>
   summarise(mean_area = mean(Area, na.rm = T)) 

# average Area by station and insert into NA values
df_HLTP <- df_HLTP %>%
   group_by(Study_area, Stn_no) %>%
   mutate(Area = ifelse(is.na(Area), mean(Area, na.rm = TRUE), Area)) %>%
   ungroup()


## summary ----

df_HLTP_sum <- df_HLTP |>
   filter(Sweep <= 3) |>
   group_by(Study_area, Year, Species, Stn_no, Area) |>
   summarise(bio.sum = sum(Weight.g, na.rm = T), abun = n()) |>
   mutate(abun.stand = abun/Area*100, bio.stand = bio.sum/Area*100)
write.csv(df_HLTP_sum, "data_derived/mmm/HLTP_dage_2012_2018.csv", row.names = F)

HLTP_site <- df_HLTP_sum |>
   group_by(Study_area, Species, Year) |>
   summarise(n = n(),
             min_den = min(abun.stand), 
             max_den = max(abun.stand),
             mean_den = mean(abun.stand),
             sd_den = sd(abun.stand),
             min_bio = min(bio.stand),
             max_bio = max(bio.stand),
             mean_bio = mean(bio.stand),
             sd_bio = sd(bio.stand),
             min_bio = min(bio.stand)
   )
write.csv(HLTP_site, "data_derived/mmm/HLTP_site_2012_2018.csv", row.names = F)

HLTP_year <- df_HLTP_sum |>
   group_by(Study_area, Species) |>
   summarise(n = n(),
             min_den = min(abun.stand), 
             max_den = max(abun.stand),
             mean_den = mean(abun.stand),
             sd_den = sd(abun.stand),
             min_bio = min(bio.stand),
             max_bio = max(bio.stand),
             mean_bio = mean(bio.stand),
             sd_bio = sd(bio.stand),
             min_bio = min(bio.stand)
   )
write.csv(HLTP_year, "data_derived/mmm/HLTP_year_2012_2018.csv", row.names = F)
