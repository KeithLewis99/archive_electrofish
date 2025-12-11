# this is to bootstrap the stoney brook data to get density and biomass estimates

# Hmisc - ben bolker approach
#https://stackoverflow.com/questions/38554383/bootstrapped-confidence-intervals-with-dplyr
library(Hmisc)
library(dplyr)

df_sb <- read.csv("../data/stoneybrook/stoney2001_2003.csv")
str(df_sb)

unique(df_sb$Species) # "AS"      "BT"      "BN"      "SMELT"   "ASYOY"   "BNYOY"   "EEL"    "BTYOY"   "EELS"    "STICKLE"
unique(df_sb$Site) # all "Stoney"
unique(df_sb$Station) # all OK - "R1"   "R2"   "R3"   "R4"   "F1"   "F2"   "POOL"

# filter out unneeded species
species_filter <- c("BN", "SMELT", "BNYOY", "EEL", "EELS", "STICKLE")
df_sb <- df_sb |> filter(!Species %in% species_filter)

df_sum <- df_sb |>
   filter(Sweep <= 3) |>
   group_by(Year, Species, Station, Area) |>
   summarise(bio.sum = sum(Weight.g, na.rm = T), abun = n()) |>
   mutate(abun.stand = abun/Area*100, bio.stand = bio.sum/Area*100)


# boostrap
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
