# Independent schools supplementary analysis

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

### match on the independent school lookup and select only for current_year ----
# read in school lookup
ind_school_lookup <- read_spss(paste(file.path(
  reference_files, "Independent_primary_schools.sav")))

# rename the school code variables
bmi_basefile <- bmi_basefile %>% 
  rename(school_code = schlgivn)

# join the two files together
independent_schools <- full_join(bmi_basefile, ind_school_lookup, 
                                  by = c("school_code"))

independent_schools <- rbind(independent_schools %>% 
                                    group_by(school_name,
                                             schlyr_exam) %>% 
                                    summarise(total_reviews = sum(tot)) %>% 
                                    ungroup()) %>% 
  subset(schlyr_exam == current_year)

#### END OF SCRIPT ### ----