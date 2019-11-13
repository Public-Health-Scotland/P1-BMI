### P1 BMI - create Tableau data file.

### 1 - Housekeeping ----

## Install Packages
install.packages("readxl")
install.packages("here")
install.packages("readr")
install.packages("dplyr")
install.packages("tidyr")
install.packages("haven")
install.packages("lubridate")

## Packages
library(haven)    # used for importing SPSS files
library(dplyr)
library(lubridate)
library(readxl)
library(here)
library(readr)
library(tidyr)


source(here("Code", "functions.R"))


## File Locations
# Source Data
server_desktop <- "server"

if (server_desktop == "server") {
  host_folder <- "//PHI_conf/ChildHealthSurveillance/Topics/Obesity/Publications/Primary1BMI/20191210/RAP/"
  source_folder <- "//PHI_conf/ChildHealthSurveillance/Portfolio/Data"
  lookup_folder <- "/conf/linkage/output/lookups"
} else if (server_desktop == "desktop") {
  host_folder <- "//stats/ChildHealthSurveillance/Topics/Obesity/Publications/Primary1BMI/20191210/RAP/"
  source_folder <- "//stats/ChildHealthSurveillance/Portfolio/Data"
  lookup_folder <-"//Isdsf00d03/cl-out/lookups"
}

## Variables
# Current School/Financial Year (e.g. 1819)
current_year <- 1819


### Create the data file for Tableau ----

# start with the final output from the main P1 BMI script
bmi_all_data <- readRDS(paste0(host_folder, "Output/bmi_all_data.rds")) 

bmi_all_data <- bmi_all_data %>% 
  subset(select = c(location_lookup, location_name, total_reviews, population,
                    valid_reviews)) %>% 
  mutate(coverage_flag = substr(location_lookup,1,8)) %>% 
  subset(coverage_flag == "Coverage")
  









# list the variables needed for final file
# pop_estimate
# tot_revs
# valid_revs
# HB_RESIDENCE_DESC
# SchoolYear
# YearEnding
# COUNCIL_AREA_DESC










