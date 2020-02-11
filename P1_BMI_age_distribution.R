# Age distribution supplementary analysis

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


## File Locations
# Source Data
server_desktop <- "server"

if (server_desktop == "server") {
  host_folder <- "//PHI_conf/ChildHealthSurveillance/Topics/Obesity/Publications/Primary1BMI/20191210/RAP/"
  source_folder <- "//PHI_conf/ChildHealthSurveillance/Portfolio/Data"
  lookup_folder <- "/conf/linkage/output/lookups"
  reference_files <- "//PHI_conf/ChildHealthSurveillance/Portfolio/Data/ReferenceFiles"
} else if (server_desktop == "desktop") {
  host_folder <- "//stats/ChildHealthSurveillance/Topics/Obesity/Publications/Primary1BMI/20191210/RAP/"
  source_folder <- "//stats/ChildHealthSurveillance/Portfolio/Data"
  lookup_folder <-"//Isdsf00d03/cl-out/lookups"
  reference_files <- "//stats/ChildHealthSurveillance/Portfolio/Data/ReferenceFiles"
}

## Variables
# Current School/Financial Year (e.g. 1819)
current_year <- 1819

# read in basefile
bmi_basefile <- readRDS(paste0(host_folder, "BMI_data_0102_1819.rds"))

age_distribution <- bmi_basefile %>% 
  subset(select = c(chi, schlyr_exam, HB2019, HB2019Name, agemth)) %>%
  mutate(age_less_than_4.5 = case_when(agemth < 4.5 ~ 1,
                                       TRUE ~ 0),
         age_4.5_to_5.5 = case_when((agemth >= 54 & agemth <= 66) ~ 1,
                                    TRUE ~ 0),
         age_5.5_to_6.25 = case_when((agemth > 66 & agemth <= 75) ~ 1,
                                     TRUE ~ 0),
         age_6.25_to_7 = case_when((agemth > 75 & agemth <= 84) ~ 1,
                                   TRUE ~ 0),
         age_7_to_8 = case_when(agemth > 84 ~ 1,
                                TRUE ~ 0)) %>% 
  subset(schlyr_exam == current_year)

table(age_distribution$age_less_than_4.5)
table(age_distribution$age_4.5_to_5.5)
table(age_distribution$age_5.5_to_6.25)
table(age_distribution$age_6.25_to_7)
table(age_distribution$age_7_to_8)






