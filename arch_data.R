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

tmp <- read.csv("../archival_data/data/electro_fish.csv")
str(tmp) # ~37K records
length(tmp$Weight[is.na(tmp$Weight)]) # 13.7K are NA

# convert so that codes make sense
library(dplyr)

# Station



# Species
unique(tmp$Species)

tmp$Species <- as.factor(tmp$Species)
## ll = landlocked or ouananiche; an = anadramous
## need cut offs for YOY from length
levels(tmp$Species) <- c("4", "AS-ll", "AS-an", "BnT", "BT", "700")

