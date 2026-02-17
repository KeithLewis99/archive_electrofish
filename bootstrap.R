# this file is to produce bootstrap estimates of the stoney brook data to get density and biomass estimates and ultimately, a table of all the electrofishing data.
## see table on slide 6 of m9-2026-01-09.ppt in synthesis folder.

# Data rank 1 ----
# Hmisc - ben bolker approach
#https://stackoverflow.com/questions/38554383/bootstrapped-confidence-intervals-with-dplyr
library(Hmisc)
library(dplyr)

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

## summarize
df_sum <- df_sb |>
   filter(Sweep <= 3) |>
   group_by(Year, Species, Station, Area) |>
   summarise(bio.sum = sum(Weight.g, na.rm = T), abun = n()) |>
   mutate(abun.stand = abun/Area*100, bio.stand = bio.sum/Area*100)


## boostrap ----
### aggregate stations
spp.den.boot.ci <- df_sum |> 
   group_by(Year, Species) |>
   do(data.frame(rbind(Hmisc::smean.cl.boot(.$abun.stand)))) |>
   rename(mean = Mean, ll = Lower, ul = Upper)


spp.bio.boot.ci <- df_sum |> 
   group_by(Year, Species) |>
   do(data.frame(rbind(Hmisc::smean.cl.boot(.$bio.stand)))) |>
   rename(mean = Mean, ll = Lower, ul = Upper)

sb.boot.ci <- full_join(spp.den.boot.ci, spp.bio.boot.ci, by = c("Year", "Species")) |>
   rename(density = mean.x, den.ll = ll.x, den.ul = ul.x,
          biomass = mean.y, bio.ll = ll.y, bio.ul = ul.y)
sb.boot.ci
write.csv(sb.boot.ci, "../data/stoneybrook/bootstrap_ci_2001_2003.csv", row.names = F)

### agg year ----
spp.den.boot.year.ci <- df_sum |> 
   group_by(Species) |>
   do(data.frame(rbind(Hmisc::smean.cl.boot(.$abun.stand)))) |>
   rename(mean = Mean, ll = Lower, ul = Upper)


spp.bio.boot.year.ci <- df_sum |> 
   group_by(Species) |>
   do(data.frame(rbind(Hmisc::smean.cl.boot(.$bio.stand)))) |>
   rename(mean = Mean, ll = Lower, ul = Upper)

sb.boot.year.ci <- full_join(spp.den.boot.year.ci, spp.bio.boot.year.ci, by = c("Species")) |>
   rename(density = mean.x, den.ll = ll.x, den.ul = ul.x,
          biomass = mean.y, bio.ll = ll.y, bio.ul = ul.y)
sb.boot.year.ci
write.csv(sb.boot.ci, "../data/stoneybrook/bootstrap_year_ci_2001_2003.csv", row.names = F)



# Trepassey ----
# & Highlands ----
## import ----
# library(readxl)
# df_HLTP <- read_excel("../data/Highlands_Trepassey_Compiled_AM_02052026.xlsx")
# Excel is evil!!!
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
library(tidyr)
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

# this worked based on pivot table above 

## split ----
# split into Trepassey and Highlands
df_tp <- df_HLTP |>
   filter(Study_area == "Trepassey")

df_hl <- df_HLTP |>
   filter(Study_area == "Highland")


## TP bootstrap ----
df_tp_spp_yr_stn <- df_tp |>
   filter(Sweep <= 3) |>
   group_by(Year, Species, Stn_no, Area) |>
   summarise(bio.sum = sum(Weight.g, na.rm = T), abun = n()) |>
   mutate(abun.stand = abun/Area*100, bio.stand = bio.sum/Area*100)


# den
spp.den.tp.boot.ci <- df_tp_spp_yr_stn |> 
   group_by(Year, Species) |>
   do(data.frame(rbind(Hmisc::smean.cl.boot(.$abun.stand)))) |>
   rename(mean = Mean, ll = Lower, ul = Upper)

# bio
spp.bio.tp.boot.ci <- df_tp_spp_yr_stn |> 
   group_by(Year, Species) |>
   do(data.frame(rbind(Hmisc::smean.cl.boot(.$bio.stand)))) |>
   rename(mean = Mean, ll = Lower, ul = Upper)

#
tp.boot.ci <- full_join(spp.den.tp.boot.ci, spp.bio.tp.boot.ci, by = c("Year", "Species")) |>
   rename(density = mean.x, den.ll = ll.x, den.ul = ul.x,
          biomass = mean.y, bio.ll = ll.y, bio.ul = ul.y)
tp.boot.ci

df_tp_spp_yr_stn |> filter(Year == "2016" & Species == "BTY") # NA's for this filter are bc no other fish caught

write.csv(tp.boot.ci, "output/TP_bootstrap_ci_2012_2018.csv", row.names = F)

### agg year ----
spp.den.tp.year.boot.ci <- df_tp_spp_yr_stn |> 
   group_by(Species) |>
   do(data.frame(rbind(Hmisc::smean.cl.boot(.$abun.stand)))) |>
   rename(mean = Mean, ll = Lower, ul = Upper)


spp.bio.tp.year.boot.ci <- df_tp_spp_yr_stn |> 
   group_by(Species) |>
   do(data.frame(rbind(Hmisc::smean.cl.boot(.$bio.stand)))) |>
   rename(mean = Mean, ll = Lower, ul = Upper)


tp.year.boot.ci <- full_join(spp.den.tp.year.boot.ci, spp.bio.tp.year.boot.ci, by = c("Species")) |>
   rename(density = mean.x, den.ll = ll.x, den.ul = ul.x,
          biomass = mean.y, bio.ll = ll.y, bio.ul = ul.y)
tp.year.boot.ci
write.csv(tp.year.boot.ci, "output/TP_year_bootstrap_ci_2012_2018.csv", row.names = F)


# Highlands ----
## bootstrap
df_hl_spp_yr_stn <- df_hl |>
   filter(Sweep <= 3) |>
   group_by(Year, Species, Stn_no, Area) |>
   summarise(bio.sum = sum(Weight.g, na.rm = T), abun = n()) |>
   mutate(abun.stand = abun/Area*100, bio.stand = bio.sum/Area*100)


# den
spp.den.hl.boot.ci <- df_hl_spp_yr_stn |> 
   group_by(Year, Species) |>
   do(data.frame(rbind(Hmisc::smean.cl.boot(.$abun.stand)))) |>
   rename(mean = Mean, ll = Lower, ul = Upper)

# bio
spp.bio.hl.boot.ci <- df_hl_spp_yr_stn |> 
   group_by(Year, Species) |>
   do(data.frame(rbind(Hmisc::smean.cl.boot(.$bio.stand)))) |>
   rename(mean = Mean, ll = Lower, ul = Upper)

#
hl.boot.ci <- full_join(spp.den.hl.boot.ci, spp.bio.hl.boot.ci, by = c("Year", "Species")) |>
   rename(density = mean.x, den.ll = ll.x, den.ul = ul.x,
          biomass = mean.y, bio.ll = ll.y, bio.ul = ul.y)
hl.boot.ci

write.csv(hl.boot.ci, "output/hl_bootstrap_ci_2012_2018.csv", row.names = F)

### agg year ----
spp.den.hl.year.boot.ci <- df_hl_spp_yr_stn |> 
   group_by(Species) |>
   do(data.frame(rbind(Hmisc::smean.cl.boot(.$abun.stand)))) |>
   rename(mean = Mean, ll = Lower, ul = Upper)


spp.bio.hl.year.boot.ci <- df_hl_spp_yr_stn |> 
   group_by(Species) |>
   do(data.frame(rbind(Hmisc::smean.cl.boot(.$bio.stand)))) |>
   rename(mean = Mean, ll = Lower, ul = Upper)


hl.year.boot.ci <- full_join(spp.den.hl.year.boot.ci, spp.bio.hl.year.boot.ci, by = c("Species")) |>
   rename(density = mean.x, den.ll = ll.x, den.ul = ul.x,
          biomass = mean.y, bio.ll = ll.y, bio.ul = ul.y)
hl.year.boot.ci
write.csv(hl.year.boot.ci, "output/hl_year_bootstrap_ci_2012_2018.csv", row.names = F)

# End ----