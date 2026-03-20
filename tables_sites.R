# this is a follow up to tables but will be only for tables aggregated over sites, i.e., they years will be displayed

# library ----
## delta method only
library(dplyr)
library(kableExtra)


# site ----
## 2b load files ----
# create pattern
temp1 = list.files(path = "data_derived", pattern=".+site.+.csv$", full.names = T)

# read all files in a folder that match a pattern and name each one
name_files = function(x) {
   name = gsub(".*data_derived/", "", x)
   name = gsub(".csv$", "", paste0(name, ""))
   return(name)
}

# create a list of dataframes, change the subscript to a name, extract as dataframes
ls_delta_siteagg = (lapply(temp1, read.csv))
names(ls_delta_siteagg) <- name_files(temp1)
list2env(ls_delta_siteagg, envir = .GlobalEnv)
str(ls_delta_siteagg, 1)

### TP ----
# no trepassey because no site level info

## 2a load files ----
# create pattern
str(ls_delta_siteagg, 1)

### CL ----
cl_den_delta_site_1993_1995 <- cl_den_delta_site_1993_1995 |>
   rename(density = mean_site,
          den_se = se_site,
          den_ll = ll_site,
          den_ul = ul_site)


cl_bio_site_1993_1995 <- cl_bio_site_1993_1995 |>
   rename(biomass = mean_site,
          bio_se = se_site,
          bio_ll = ll_site,
          bio_ul = ul_site)

common <- intersect(names(cl_den_delta_site_1993_1995), 
                    names(cl_bio_site_1993_1995)
)
out <- bind_cols(cl_den_delta_site_1993_1995,
                 cl_bio_site_1993_1995 %>% 
                    select(-all_of(common)))

tab2a_siteagg <- kbl(out[, c(1:3, 8, 4, 6:7, 9, 11:12)], 
                    col.names = c('Study_area', 'Year', 'Species', 'age',
                                  'mean', '2.5%', '97.5%',
                                  'mean', '2.5%', '97.5%'),
                    align = 'c', caption = "Copper Lake (1993-1995): Density and Biomass CIs aggregated over sites", digits = 3 ) |>
   #collapse_rows(valign = "top",
   #             latex_hline = "major") |>
   add_header_above(header = c(" " = 4, "Density" = 3, "Biomass" = 3)) |>
   #   add_header_above(header = c(" " = 2, "Summer" = 6)) |>
   kable_paper()

save_kable(tab2a_siteagg, file = "output/tab_2aCL_site_agg.html")


### WS 1992 ----
ws_den_delta_site_1992 <- ws_den_delta_site_1992 |>
   rename(density = mean_site,
          den_ll = ll_site,
          den_ul = ul_site
   )

ws_bio_delta_site_1992 <- ws_bio_delta_site_1992 |>
   mutate(den_ll = mean_site - 1.96*se_site,
          den_ul = mean_site + 1.96*se_site) |>
   rename(biomass = mean_site)

common <- intersect(names(ws_den_delta_site_1992), 
                    names(ws_bio_delta_site_1992)
)
out <- bind_cols(ws_den_delta_site_1992,
                 ws_bio_delta_site_1992 %>% 
                    select(-all_of(common)))

tab2a_WSsite <- kbl(out[, c(1:3, 8, 4, 6:7, 10:12)], 
                   col.names = c('Study_area', 'Year', 'Species', 'age',
                                 'mean', '2.5%', '97.5%',
                                 'mean', '2.5%', '97.5%'),
                   align = 'c', caption = "West Salmon (1992): Density and Biomass CIs", digits = 3 ) |>
   #collapse_rows(valign = "top",
   #             latex_hline = "major") |>
   add_header_above(header = c(" " = 4, "Density" = 3, "Biomass" = 3)) |>
   #   add_header_above(header = c(" " = 2, "Summer" = 6)) |>
   kable_paper()

save_kable(tab2a_WSsite, file = "output/tab_2aWS_site.html")

### HL & JF ----
jf_den_delta_site_1993_1995 <- jf_den_delta_site_1993_1995 |>
   rename(density = mean_site,
          den_var = se_site,
          den_ll = ll_site,
          den_ul = ul_site)


HL_den_delta_site_2002 <- HL_den_delta_site_2002 |>
   mutate(den_ll = density_new - 1.96*density_se,
          den_ul = density_new + 1.96*density_se)

keys <- c("study_area", "year", "species", "trt", "age")

res <- jf_den_delta_site_1993_1995 %>%
   # out already has density/biomass + CI/var; keep it as the primary table
   full_join(
      HL_den_delta_site_2002 %>%
         select(all_of(keys)), 
      by = keys
   )

tab2b_HL_JF <- kbl(res[, c(1:4, 9, 5, 7:8)], 
                   col.names = c('Study_area', 'Year', 'Species', 
                                 'trt', 'age',
                                 'mean', '2.5%', '97.5%'),
                   align = 'c', caption = "Joe Farrells (1993-95) and Highlands (2002): Density and Biomass CIs", digits = 3 ) |>
   collapse_rows(valign = "top",
                 latex_hline = "major") |>
   add_header_above(header = c(" " = 5, "Density" = 3)) |>
   #   add_header_above(header = c(" " = 2, "Summer" = 6)) |>
   kable_paper()

save_kable(tab2b_HL_JF, file = "output/tab_HL_JF_site.html")

### SJ ----
# 1980s
# sj_den_delta_site_1983_1985 <- sj_den_delta_site_1983_1985 |>
#    rename(density = mean_site,
#           den_ll = ll_site,
#           den_ul = ul_site
#    )
# 
# sj_bio_delta_site_1983_1985 <- sj_bio_delta_site_1983_1985 |>
#    rename(biomass = mean_site,
#           bio_ll = ll_site,
#           bio_ul = ul_site
#    )
# 
# 
# tmp <- left_join(sj_den_delta_site_1983_1985, 
#                  sj_bio_delta_site_1983_1985, 
#                  by = c("study_area", "year", "species", "age"))
# tmp$trt <- "riffle"
# 
# 
# # 1990s
# common <- intersect(names(sj_den_delta_site_1995_1996), 
#                     names(sj_bio_delta_site_1995_1996)
# )
# 
# out <- bind_cols(sj_den_delta_site_1995_1996,
#                  sj_bio_delta_site_1995_1996 %>% 
#                     select(-all_of(common)))
# out <- out |>
#    group_by(study_area, year, species, age) |>
#    mutate(den_ll = density - density_se*1.96,
#           den_ul = density + density_se*1.96,
#           bio_ll = biomass - biomass_se*1.96,
#           bio_ul = biomass + biomass_se*.196)
#    
# out$trt <- "riffle"
# 
# # pools
# sj_pool_delta_site_1995_1996 <- sj_pool_delta_site_1995_1996 |>
#    rename(density = density_new,
#           den_ll = dll_new,
#           den_ul = dul_new,
#           biomass = biomass_new,
#           bio_ll = bll_new,
#           bio_ul = bul_new,
#           age = age_new)
# sj_pool_delta_site_1995_1996$trt <- "pool"
# 
# 
# SJ_all <- rbind(tmp[,-c(5, 10)], out[, -c(6,8)])
# SJ_all <- rbind(SJ_all, sj_pool_delta_site_1995_1996)
# 
# 
# tab2b_SJ_all <- kbl(SJ_all[, c(1:3, 7, 11, 4:6, 8:10)], 
#                    col.names = c('Study_area', 'Year', 'Species',  'trt', 'age',
#                                  'mean', '2.5%', '97.5%',
#                                  'mean', '2.5%', '97.5%'),
#                    align = 'c', caption = "St. John's (1982-1996): Density and Biomass CIs", digits = 3 ) |>
#    collapse_rows(valign = "top",
#                  latex_hline = "major") |>
#    add_header_above(header = c(" " = 5, "Density" = 3, "Biomass" = 3)) |>
#    #   add_header_above(header = c(" " = 2, "Summer" = 6)) |>
#    kable_paper()
# 
# save_kable(tab2b_SJ_all, file = "tab2b_SJ_all_site.html")



# year ----
## load files ----
# create pattern
temp2 = list.files(path = "data_derived", pattern=".+_yr_.+.csv$", full.names = T)

# create a list of dataframes, change the subscript to a name, extract as dataframes
ls_delta_yearagg = (lapply(temp2, read.csv))
names(ls_delta_yearagg) <- name_files(temp2)
list2env(ls_delta_yearagg, envir = .GlobalEnv)
str(ls_delta_yearagg, 1)


### TP ----
tp_bio_delta_yr_1984_1996 <- tp_bio_delta_yr_1984_1996 |>
   rename(biomass = mean_year,
          bio_var = var_year,
          bio_ll = ll_site,
          bio_ul = ul_site)


tp_den_delta_yr_1984_1996 <- tp_den_delta_yr_1984_1996 |>
   rename(density = mean_year,
          den_var = var_year,
          den_ll = ll_site,
          den_ul = ul_site)



common <- intersect(names(tp_bio_delta_yr_1984_1996), 
                    names(tp_den_delta_yr_1984_1996)
)
out <- bind_cols(tp_den_delta_yr_1984_1996,
                 tp_bio_delta_yr_1984_1996 %>% 
                    select(-all_of(common)))

# add pool
tp_pool_den_delta_yr_1984_1996$trt <- "pool"
tp_pool_den_delta_yr_1984_1996 <- tp_pool_den_delta_yr_1984_1996 |>
   rename(density = mean_year,
          den_var = var_year,
          den_ll = ll_site,
          den_ul = ul_site)

tp_pool_bio_delta_yr_1984_1996$trt <- "pool"
tp_pool_bio_delta_yr_1984_1996 <- tp_pool_bio_delta_yr_1984_1996 |>
   rename(biomass = mean_year,
          bio_var = var_year,
          bio_ll = ll_site,
          bio_ul = ul_site)

common <- intersect(names(tp_pool_den_delta_yr_1984_1996), 
                    names(tp_pool_bio_delta_yr_1984_1996)
)
out_pool <- bind_cols(tp_pool_den_delta_yr_1984_1996,
                 tp_pool_bio_delta_yr_1984_1996 %>% 
                    select(-all_of(common)))

keys <- c("study_area", "species", "trt", "age")

res_all <- out %>%
   # out already has density/biomass + CI/var; keep it as the primary table
   full_join(
      out_pool %>%
         select(all_of(keys)), 
      by = keys
   )

tab2b_yragg <- kbl(res_all[, c(1:3, 8, 4, 6:7, 9, 11:12)], 
                    col.names = c('Study_area', 'Species', 
                                  'trt', 'age',
                                  'mean', '2.5%', '97.5%',
                                  'mean', '2.5%', '97.5%'),
                    align = 'c', caption = "Trepassey (1984-1996): Density and Biomass CIs", digits = 3 ) |>
   collapse_rows(valign = "top",
                 latex_hline = "major") |>
   add_header_above(header = c(" " = 4, "Density" = 3, "Biomass" = 3)) |>
   #   add_header_above(header = c(" " = 2, "Summer" = 6)) |>
   kable_paper()

save_kable(tab2b_yragg, file = "output/tab_tp_yr.html")




### CL ----
str(ls_delta_yearagg, 1)
cl_den_delta_yr_1993_1995 <- cl_den_delta_yr_1993_1995 |>
   rename(density = mean_year,
          den_se = var_year,
          den_ll = ll_site,
          den_ul = ul_site)


cl_bio_delta_yr_1993_1995 <- cl_bio_delta_yr_1993_1995 |>
   rename(biomass = mean_year,
          bio_se = var_year,
          bio_ll = ll_site,
          bio_ul = ul_site)

common <- intersect(names(cl_den_delta_yr_1993_1995), 
                    names(cl_bio_delta_yr_1993_1995)
)
out <- bind_cols(cl_den_delta_yr_1993_1995,
                 cl_bio_delta_yr_1993_1995 %>% 
                    select(-all_of(common)))

tab2a_yragg <- kbl(out[, c(1:2, 7, 3, 5:6, 8, 10:11)], 
                     col.names = c('Study_area', 'Species', 'age',
                                   'mean', '2.5%', '97.5%',
                                   'mean', '2.5%', '97.5%'),
                     align = 'c', caption = "Copper Lake (1993-1995): Density and Biomass CIs aggregated over years", digits = 3 ) |>
   #collapse_rows(valign = "top",
   #             latex_hline = "major") |>
   add_header_above(header = c(" " = 3, "Density" = 3, "Biomass" = 3)) |>
   #   add_header_above(header = c(" " = 2, "Summer" = 6)) |>
   kable_paper()

save_kable(tab2a_yragg, file = "output/tab_2aCL_yr_agg.html")


### WS 1992 ----
# ws_den_delta_site_1992 <- ws_den_delta_site_1992 |>
#    rename(density = mean_site,
#           den_ll = ll_site,
#           den_ul = ul_site
#    )
# 
# ws_bio_delta_site_1992 <- ws_bio_delta_site_1992 |>
#    mutate(den_ll = mean_site - 1.96*se_site,
#           den_ul = mean_site + 1.96*se_site) |>
#    rename(biomass = mean_site)
# 
# common <- intersect(names(ws_den_delta_site_1992), 
#                     names(ws_bio_delta_site_1992)
# )
# out <- bind_cols(ws_den_delta_site_1992,
#                  ws_bio_delta_site_1992 %>% 
#                     select(-all_of(common)))
# 
# tab2a_WSsite <- kbl(out[, c(1:3, 8, 4, 6:7, 10:12)], 
#                     col.names = c('Study_area', 'Year', 'Species', 'age',
#                                   'mean', '2.5%', '97.5%',
#                                   'mean', '2.5%', '97.5%'),
#                     align = 'c', caption = "West Salmon (1992): Density and Biomass CIs", digits = 3 ) |>
#    #collapse_rows(valign = "top",
#    #             latex_hline = "major") |>
#    add_header_above(header = c(" " = 4, "Density" = 3, "Biomass" = 3)) |>
#    #   add_header_above(header = c(" " = 2, "Summer" = 6)) |>
#    kable_paper()
# 
# save_kable(tab2a_WSsite, file = "output/tab_2aWS_site.html")

### JF ----
# no Highlands for year because it was only done in 2002
jf_den_delta_yr_1993_1995 <- jf_den_delta_yr_1993_1995 |>
   rename(density = mean_year,
          den_var = var_year,
          den_ll = ll_site,
          den_ul = ul_site)

# HL_den_delta_site_2002 <- HL_den_delta_site_2002 |>
#    mutate(den_ll = density - 1.96*density_se,
#           den_ul = density + 1.96*density_se)
# 
# keys <- c("study_area", "year", "species", "trt", "age")

# res <- jf_den_delta_yr_1993_1995 %>%
#    # out already has density/biomass + CI/var; keep it as the primary table
#    full_join(
#       HL_den_delta_yr_2002 %>%
#          select(all_of(keys)), 
#       by = keys
#    )

tab2b_JF <- kbl(jf_den_delta_yr_1993_1995[, -c(5)], 
                   col.names = c('Study_area', 'Species', 'trt', 'age',
                                 'mean', '2.5%', '97.5%'),
                   align = 'c', caption = "Joe Farrells (1993-95): Density and Biomass CIs", digits = 3 ) |>
   collapse_rows(valign = "top",
                 latex_hline = "major") |>
   add_header_above(header = c(" " = 4, "Density" = 3)) |>
   #   add_header_above(header = c(" " = 2, "Summer" = 6)) |>
   kable_paper()

save_kable(tab2b_JF, file = "output/tab_JF_site.html")

### 2c ----
### GG ----


# make year like site df
GG_den_yr_1995_1996 <- GG_den_yr_1995_1996 |>
   rename(mean_site = mean_year, se_site = var_year)
GG_den_yr_1995_1996$year <- "both"

GG_bio_yr_1995_1996 <- GG_bio_yr_1995_1996 |>
   rename(mean_site = mean_year, se_site = var_year)
GG_bio_yr_1995_1996$year <- "both"

GG_bio_site_1995_1996
GG_den_site_1995_1996 
### 
#GG_yr_1995_1996$species <- "salmonid"

# GG_yr_1995_1996 <- GG_yr_1995_1996 |>
#    select("study_area", "year", "species", "mean_year",  "ll_site",    "ul_site", "age") |>
#    rename(mean_site = mean_year)
# GG_yr_1995_1996$species <- "salmonid"

GG_den_ <- rbind(GG_den_site_1995_1996[,-5], GG_den_yr_1995_1996[-4])
GG_bio_ <- rbind(GG_bio_site_1995_1996[,-5], GG_bio_yr_1995_1996[-4])


GG_den_ <- GG_bio_ |>
   rename(density = mean_site,
          den_ll = ll_site,
          den_ul = ul_site
   )

GG_bio_ <- GG_bio_ |>
   rename(biomass = mean_site,
          bio_ll = ll_site,
          bio_ul = ul_site
          )

common <- intersect(names(GG_den_), 
                    names(GG_bio_)
)

out <- bind_cols(GG_den_,
                 GG_bio_ %>% 
                    select(-all_of(common)))
# biomass

tab2c_GG <- kbl(out[, c(1:3, 7, 4:6, 8:10)], 
                   col.names = c('Study_area', 'Year', 'Species', 'age',
                                 'mean', '2.5%', '97.5%',
                                 'mean', '2.5%', '97.5%'),
                   align = 'c', caption = "Great Gull Brook (1997-98): Density and Biomass CIs", digits = 3 ) |>
   collapse_rows(valign = "top",
                 latex_hline = "major") |>
   add_header_above(header = c(" " = 4, "Density" = 3, "Biomass" = 3)) |>
   #   add_header_above(header = c(" " = 2, "Summer" = 6)) |>
   kable_paper()

save_kable(tab2c_GG, file = "output/tab2c_GG.html")


## JB ----
JB_site_2017_2018
JB2c_yr_2017_2018$year <- "both"
JB2c_yr_2017_2018 <- JB2c_yr_2017_2018 |>
   rename(mean_site = mean_year,
          se_site = var_year)

JB_site_yr_2017_2018 <- rbind(JB_site_2017_2018, JB2c_yr_2017_2018)

tabJB_site_yr_2017_2018 <- kbl(JB_site_yr_2017_2018[,-c(3, 5)], 
                col.names = c('Study_area', 'Year', 'age',
                              'mean', '2.5%', '97.5%'),
                align = 'c', caption = "Jumper's Brook (2017-2018): Density CIs", digits = 3 ) |>
   collapse_rows(valign = "top",
                 latex_hline = "major") |>
   add_header_above(header = c(" " = 3, "Density" = 3)) |>
   kable_paper()

save_kable(tabJB_site_yr_2017_2018, file = "output/tab2c_JB_site_yr_2017_2018.html")


# SJ ----
# # 1980s
# sj_den_delta_yr_1982_1985 <- sj_den_delta_yr_1982_1985 |>
#    rename(density = mean_year,
#           den_ll = ll_site,
#           den_ul = ul_site
#    )
# 
# sj_bio_delta_yr_1983_1985 <- sj_bio_delta_yr_1983_1985 |>
#    rename(biomass = mean_year,
#           bio_ll = ll_site,
#           bio_ul = ul_site
#    )
# 
# 
# tmp <- left_join(sj_den_delta_yr_1982_1985, 
#                  sj_bio_delta_yr_1983_1985, 
#                  by = c("study_area", "species", "age"))
# tmp$trt <- "riffle"
# tmp$period <- "1982-1985"
# 
# # 1990s
# sj_den_delta_yr_1995_1996 <- sj_den_delta_yr_1995_1996 |>
#    rename(density = mean_year,
#           den_ll = ll_site,
#           den_ul = ul_site
#    )
# 
# sj_bio_delta_yr_1993_1995 <- sj_bio_delta_yr_1993_1995 |>
#    rename(biomass = mean_year,
#           bio_ll = ll_site,
#           bio_ul = ul_site
#    )
# 
# common <- intersect(names(sj_den_delta_yr_1995_1996), 
#                     names(sj_bio_delta_yr_1993_1995)
# )
# 
# out <- bind_cols(sj_den_delta_yr_1995_1996,
#                  sj_bio_delta_yr_1993_1995 %>% 
#                     select(-all_of(common)))
# 
# out$trt <- "riffle"
# out$period <- '1995-1996'
# 
# 
# sj_pool_delta_yr_1995_1996 <- sj_pool_delta_yr_1995_1996 |>
#    rename(density = density_new,
#           den_ll = dll_new,
#           den_ul = dul_new,
#           biomass = biomass_new,
#           bio_ll = bll_new,
#           bio_ul = bul_new,
#           age = age_new)
# sj_pool_delta_site_1995_1996$trt <- "pool"
# sj_pool_delta_site_1995_1996$period <- "1995-1996 pools"
# 
# SJ_all_yr <- rbind(tmp[,-c(4, 9)], out[, -c(4)])
# SJ_all_yr <- rbind(SJ_all_yr, sj_pool_delta_yr_1995_1996)
# 
# 
# tab2b_SJ_all <- kbl(SJ_all[, c(1:3, 7, 11, 4:6, 8:10)], 
#                     col.names = c('Study_area', 'Year', 'Species',  'trt', 'age',
#                                   'mean', '2.5%', '97.5%',
#                                   'mean', '2.5%', '97.5%'),
#                     align = 'c', caption = "St. John's (1982-1996): Density and Biomass CIs", digits = 3 ) |>
#    collapse_rows(valign = "top",
#                  latex_hline = "major") |>
#    add_header_above(header = c(" " = 5, "Density" = 3, "Biomass" = 3)) |>
#    #   add_header_above(header = c(" " = 2, "Summer" = 6)) |>
#    kable_paper()
# 
# save_kable(tab2b_SJ_all, file = "tab2b_SJ_all_site.html")

# 4 ----
CB_IB_GB_site_2000 

tab4_site_CB_IB_GB <- kbl(CB_IB_GB_site_2000, 
                col.names = c('Study_area', 'Species', 
                              'Density', 'Biomass'),
                align = 'c', caption = "Corner Brook, Indian Bay, Gander River (2000): Density and Biomass", digits = 3 ) |>
   kable_paper()

save_kable(tab4_site_CB_IB_GB, file = "output/tab4_site_CB_IB_GB.html")


# 5a ----
# this is not aggregated by site but rather the data from Cote 2007
TN_site_2002 <- TN_site_2002 |>
   select("study_area", "year", "species", "stations", "density_new", "biomass_new")
 
tab_TN_site_2002 <- kbl(TN_site_2002, 
                          col.names = c('Study_area', 'year', 'Species',
                                        'stations', 'Density', 'Biomass'),
                        align = 'c', 
                        caption = "Terra Nova National Park (2002): Density and Biomass", 
                        digits = 3 ) |>
   kable_paper()

save_kable(tab_TN_site_2002, file = "output/tab5_TN_site_2002.html")

TN_yr_2002 
tab_TN_year_2002 <- kbl(TN_yr_2002, 
                        col.names = c('Study_area', 'year', 'Species', 
                                      'Density', 'Biomass'),
                        align = 'c', caption = "Terra Nova National Park (2002): Density and Biomass", digits = 3 ) |>
   kable_paper()

save_kable(tab_TN_year_2002, file = "output/tab5_TN_year_2002.html")

# 5b ----
# this is also not aggregated by site but just the data as in the report
TI_site_2006_2010 <- TI_site_2006_2010 |>
   select("study_area", "year", "species", "density_new")

tab_TI_site_2006_2010 <- kbl(TI_site_2006_2010, 
                        col.names = c('Study_area', 'year', 'Species',
                                      'Density'),
                        align = 'c', 
                        caption = "Tinto Brook (2006-2010): Density", 
                        digits = 3 ) |>
   kable_paper()

save_kable(tab_TI_site_2006_2010, file = "output/tab5_TI_site_2006_2010.html")

 
tab_TI_yr_2006_2010 <- kbl(TI_yr_2006_2010, 
                        col.names = c('Study_area', 'Species', 
                                      'Density'),
                        align = 'c', caption = "Tinto Brook (2006-2010): Density", digits = 3 ) |>
   kable_paper()

save_kable(tab_TI_yr_2006_2010, file = "output/tab5_TI_year_2006_2010.html")


# END ----