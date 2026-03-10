# the purpose of this file is to create tables of density and biomass estimates including the CIs for bootstrap, deltamethod, and simpler files.

# load packages
library(dplyr)
library(kableExtra)

# boot ----
## disagg ---- 
### load files ----
# create pattern
temp1 = list.files(path = "data_derived/boot", pattern=".+disagg.+.csv$", full.names = T)

# read all files in a folder that match a pattern and name each one
name_files = function(x) {
   name = gsub(".*data_derived/boot/", "", x)
   name = gsub(".csv$", "", paste0(name, ""))
   return(name)
}

# create a list of dataframes, change the subscript to a name, extract as dataframes
ls_disagg = (lapply(temp1, read.csv))
names(ls_disagg) <- name_files(temp1)
list2env(ls_disagg, envir = .GlobalEnv)
str(ls_disagg, 1)

## agg ----
HL_disagg_2013_2019$type <- "dis_agg"
SB_disagg_2001_2003$type <- "dis_agg"
TP_disagg_2012_2018$type <- "dis_agg"

HL_disagg_2013_2019$study_area <- "Highlands"
SB_disagg_2001_2003$study_area <- "Stoney_Brook"
TP_disagg_2012_2018$study_area <- "Trepassey"

SB_disagg_2001_2003 <- SB_disagg_2001_2003 |> rename(Stn_no = Station)

temp <- rbind(HL_disagg_2013_2019,
              SB_disagg_2001_2003,
              TP_disagg_2012_2018)
temp$Area <- round(temp$Area, 1)
temp$abun.stand <- round(temp$abun.stand, 2)
temp$bio.sum <- round(temp$bio.sum, 1)
temp$bio.stand <- round(temp$bio.stand, 2)

# make table
tab_test <- kbl(temp[, c(10, 1:4, 6, 7, 5, 8)], 
    col.names = c('Study_area', 'Year', 'Species', 'Station', 'Area',
                  'abundance', 'density', 'biomass', 'biomass/area'),
    align = 'c', caption = "Density and Biomass (bootstrap) by Station", digits = 3, booktabs = TRUE, longtable = TRUE) |>
   collapse_rows(valign = "top",
                 latex_hline = "major") |>
   add_header_above(header = c(" " = 5, "Density" = 2, "Biomass" = 2)) |>
   #   add_header_above(header = c(" " = 2, "Summer" = 6)) |>
   kable_paper()

save_kable(tab_test, file = "output/tab_boot_age_HL_SB_TP.html")


### load files ----
# create pattern
temp1 = list.files(path = "data_derived/boot", pattern=".+boot.+.csv$", full.names = T)

# read all files in a folder that match a pattern and name each one
name_files = function(x) {
   name = gsub(".*data_derived/boot/", "", x)
   name = gsub(".csv$", "", paste0(name, ""))
   return(name)
}

# create a list of dataframes, change the subscript to a name, extract as dataframes
ls_site = (lapply(temp1, read.csv))
names(ls_site) <- name_files(temp1)
list2env(ls_site, envir = .GlobalEnv)
str(ls_site, 1)

# augment
hl_boot_site_2012_2018$type <- "agg"
SB_boot_site_2001_2003$type <- "agg"
TP_boot_site_2012_2018$type <- "agg"

hl_boot_site_2012_2018$study_area <- "Highlands"
SB_boot_site_2001_2003$study_area <- "Stoney_Brook"
TP_boot_site_2012_2018$study_area <- "Trepassey"


temp <- rbind(hl_boot_site_2012_2018,
              SB_boot_site_2001_2003,
              TP_boot_site_2012_2018)


# make table
tab_agg <- kbl(temp[, c(10, 1:8)], 
    col.names = c('Study_area', 'Year', 'Species', 
                  'density', '2.5%', '97.5%',
                  'biomass', '2.5%', '97.5%'),
    align = 'c', caption = "Density and Biomass CIs", digits = 3 ) |>
   collapse_rows(valign = "top",
                 latex_hline = "major") |>
   add_header_above(header = c(" " = 3, "Density" = 3, "Biomass" = 3)) |>
#   add_header_above(header = c(" " = 2, "Summer" = 6)) |>
   kable_paper()

save_kable(tab_agg, file = "output/tab_boot_site_HL_SB_TP.html")

## yr-agg ----

hl_boot_yr_2012_2018$type <- "yr_agg"
SB_boot_yr_2001_2003$type <- "yr_agg"
TP_boot_yr_2012_2018$type <- "yr_agg"

hl_boot_yr_2012_2018$study_area <- "Highlands"
SB_boot_yr_2001_2003$study_area <- "Stoney_Brook"
TP_boot_yr_2012_2018$study_area <- "Trepassey"


temp1 <- rbind(hl_boot_yr_2012_2018,
              SB_boot_yr_2001_2003,
              TP_boot_yr_2012_2018)


tab_yr_agg <- kbl(temp1[, c(9, 1:7)], 
    col.names = c('Study_area', 'Species', 
                  'mean', '2.5%', '97.5%',
                  'mean', '2.5%', '97.5%'),
    align = 'c', caption = "Density and Biomass CIs", digits = 3 ) |>
   collapse_rows(valign = "top",
                 latex_hline = "major") |>
   add_header_above(header = c(" " = 2, "Density" = 3, "Biomass" = 3)) |>
   #   add_header_above(header = c(" " = 2, "Summer" = 6)) |>
   kable_paper()

save_kable(tab_yr_agg, file = "output/tab_boot_yr_HL_SB_TP.html")



# delta ----
### load files ----
# create pattern
temp1 = list.files(path = "data_derived", pattern=".+age.+.csv$", full.names = T)

# read all files in a folder that match a pattern and name each one
name_files = function(x) {
   name = gsub(".*data_derived/", "", x)
   name = gsub(".csv$", "", paste0(name, ""))
   return(name)
}

# create a list of dataframes, change the subscript to a name, extract as dataframes
ls_delta_ageagg = (lapply(temp1, read.csv))
names(ls_delta_ageagg) <- name_files(temp1)
list2env(ls_delta_ageagg, envir = .GlobalEnv)
str(ls_delta_ageagg, 1)

### TP ----
tp_bio_delta_age_1984_1996 <- tp_bio_delta_age_1984_1996 |>
   rename(biomass = dsum,
          bio_var = dsum_var,
          bio_ll = ll_site,
          bio_ul = ul_site)


tp_den_delta_age_1984_1996 <- tp_den_delta_age_1984_1996 |>
   rename(density = dsum,
          den_var = dsum_var,
          den_ll = ll_site,
          den_ul = ul_site)



common <- intersect(names(tp_bio_delta_age_1984_1996), 
                    names(tp_den_delta_age_1984_1996)
                    )
out <- bind_cols(tp_den_delta_age_1984_1996,
                 tp_bio_delta_age_1984_1996 %>% 
                    select(-all_of(common)))

tp_pool_delta_age_1984_1996$trt <- "pool"
tp_pool_delta_age_1984_1996 <- tp_pool_delta_age_1984_1996 |>
   rename(den_var = density_se,
          bio_var = biomass_se)


keys <- c("study_area", "year", "species", "trt", "age")

res <- out %>%
   # out already has density/biomass + CI/var; keep it as the primary table
   full_join(
      tp_pool_delta_age_1984_1996 %>%
         select(all_of(keys)), 
      by = keys
   )

tab2b_ageagg <- kbl(res[, c(1:4, 9, 5, 7:8, 10, 12:13)], 
                  col.names = c('Study_area', 'Year', 'Species', 'trt', 'age',
                                'mean', '2.5%', '97.5%',
                                'mean', '2.5%', '97.5%'),
                  align = 'c', caption = "Trepassey (1984-1996): Density and Biomass CIs", digits = 3 ) |>
   collapse_rows(valign = "top",
                 latex_hline = "major") |>
   add_header_above(header = c(" " = 5, "Density" = 3, "Biomass" = 3)) |>
   #   add_header_above(header = c(" " = 2, "Summer" = 6)) |>
   kable_paper()

save_kable(tab2b_ageagg, file = "output/tab_tp_age.html")


### CL ----
str(ls_delta_ageagg, 1)
cl_den_delta_age_1993_1995 <- cl_den_delta_age_1993_1995 |>
   rename(density = dsum,
          den_var = dsum_var,
          den_ll = ll_site,
          den_ul = ul_site)

cl_bio_delta_age_1993_1995 <- cl_bio_delta_age_1993_1995 |>
   #group_by(study_area, year, species, stations) |>
   mutate(bio_ll = biomass - biomass_se*1.96,
             bio_ul = biomass + biomass_se*1.96
   ) |>
   relocate(bio_ll, bio_ul, .after = biomass_se)

common <- intersect(names(cl_den_delta_age_1993_1995), 
                    names(cl_bio_delta_age_1993_1995)
)
out <- bind_cols(cl_den_delta_age_1993_1995,
                 cl_bio_delta_age_1993_1995 %>% 
                    select(-all_of(common)))

tab2a_ageagg <- kbl(out[, c(1:4, 9, 5, 7:8, 10, 12:13)], 
                    col.names = c('Study_area', 'Year', 'Species', 'stations', 'age',
                                  'mean', '2.5%', '97.5%',
                                  'mean', '2.5%', '97.5%'),
                    align = 'c', caption = "Copper Lake (1993-1995): Density and Biomass CIs", digits = 3 ) |>
   #collapse_rows(valign = "top",
    #             latex_hline = "major") |>
   add_header_above(header = c(" " = 5, "Density" = 3, "Biomass" = 3)) |>
   #   add_header_above(header = c(" " = 2, "Summer" = 6)) |>
   kable_paper()

save_kable(tab2a_ageagg, file = "output/tab_2aCL_age_agg.html")


### WS 1992 ----
ws_den_delta_age_1992 <- ws_den_delta_age_1992 |>
   rename(density = dsum,
          den_var = dsum_var,
          den_ll = ll_site,
          den_ul = ul_site)

ws_bio_delta_age_1992 <- ws_bio_delta_age_1992 |>
   mutate(bio_ll = biomass - 1.96*biomass_se,
          bio_ul = biomass + 1.96*biomass_se)

common <- intersect(names(ws_den_delta_age_1992), 
                    names(ws_bio_delta_age_1992)
)
out <- bind_cols(ws_den_delta_age_1992,
                 ws_bio_delta_age_1992 %>% 
                    select(-all_of(common)))

tab2a_WSage <- kbl(out[, c(1:4, 9, 5, 7:8, 11, 13:14)], 
                    col.names = c('Study_area', 'Year', 'Species', 'stations', 'age',
                                  'mean', '2.5%', '97.5%',
                                  'mean', '2.5%', '97.5%'),
                    align = 'c', caption = "West Salmon (1992): Density and Biomass CIs", digits = 3 ) |>
   #collapse_rows(valign = "top",
   #             latex_hline = "major") |>
   add_header_above(header = c(" " = 5, "Density" = 3, "Biomass" = 3)) |>
   #   add_header_above(header = c(" " = 2, "Summer" = 6)) |>
   kable_paper()

save_kable(tab2a_WSage, file = "output/tab_2aWS_age.html")

### HL & JF ----

jf_den_delta_age_1993_1995 <- jf_den_delta_age_1993_1995 |>
   rename(density = dsum,
          den_var = dsum_var,
          den_ll = ll_site,
          den_ul = ul_site)

HL_den_delta_age_2002 <- HL_den_delta_age_2002 |>
   mutate(den_ll = density - 1.96*density_se,
         den_ul = density + 1.96*density_se)

keys <- c("study_area", "year", "species", "trt", "age")

res <- jf_den_delta_age_1993_1995 %>%
   # out already has density/biomass + CI/var; keep it as the primary table
   full_join(
      HL_den_delta_age_2002 %>%
         select(all_of(keys)), 
      by = keys
   )

tab2b_HL_JF <- kbl(res[, c(1:5, 10, 6, 8:9)], 
                    col.names = c('Study_area', 'Year', 'Species', 'site' , 'trt', 'age',
                                  'mean', '2.5%', '97.5%'),
                    align = 'c', caption = "Joe Farrells (1993-95) and Highlands (2002): Density and Biomass CIs", digits = 3 ) |>
   collapse_rows(valign = "top",
                 latex_hline = "major") |>
   add_header_above(header = c(" " = 6, "Density" = 3)) |>
   #   add_header_above(header = c(" " = 2, "Summer" = 6)) |>
   kable_paper()

save_kable(tab2b_HL_JF, file = "output/tab_HL_JF_age.html")


# SJ ----
   

# GG ----
GG_age_1995_1996

GG_age_1995_1996 <- GG_age_1995_1996 |>
   select("study_area", "year", "stations", 
          "density_new", "dll_new", "dul_new",
          "biomass_new", "bll_new", "bul_new"
   )

tab_GG_age_1995_1996 <- kbl(GG_age_1995_1996, 
                   col.names = c('Study_area', 'Year', 'site',
                                 'mean', '2.5%', '97.5%',
                                 'mean', '2.5%', '97.5%'),
                   align = 'c', caption = "Great Gull Brook (Gander) 1997-1998: Density and Biomass CIs", digits = 3 ) |>
   collapse_rows(valign = "top",
                 latex_hline = "major") |>
   add_header_above(header = c(" " = 3, "Density" = 3, "Biomass" = 3)) |>
   #   add_header_above(header = c(" " = 2, "Summer" = 6)) |>
   kable_paper()

save_kable(tab_GG_age_1995_1996, file = "output/tab_GG_age_1995_1996.html")


# END ----