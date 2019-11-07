### P1 BMI open data

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


# bmi basefile
bmi_basefile <- readRDS(paste0(host_folder, "BMI_data_0102_1819.rds"))

# coverage file
bmi_data_coverage <- readRDS(paste0(host_folder, "bmi_data_coverage.rds"))


## HB level
# start with the file created within the main P1 bmi script to create both
# the epidemiological and clinical open data files
hb_open_data <- readRDS(paste0(host_folder, "OpenData/hb_open_data.rds"))

## hb epidemiological
# rename variables
hb_open_data_epi <- hb_open_data %>% 
  rename(SchoolYear = schlyr_exam,
         HBR2014 = HB2019, 
         ValidReviews = total_reviews,
         EpiUnderweight = per_epi_undw,
         LCIEpiUnderweight = epi_undw_lci,
         UCIEpiUnderweight = epi_undw_uci,
         EpiHealthyWeight = per_epi_hw,
         LCIEpiHealthyWeight = epi_hw_lci,
         UCIEpiHealthyWeight = epi_hw_uci,
         EpiOverweight = per_epi_over,
         LCIEpiOverweight = epi_over_lci,
         UCIEpiOverweight = epi_over_uci,
         EpiObese = per_epi_obe,
         LCIEpiObese = epi_obe_lci,
         UCIEpiObese = epi_obe_uci,
         EpiOverweightAndObese = per_epi_overobe,
         LCIEpiOverweightAndObese = epi_overobe_lci,
         UCIEpiOverweightAndObese = epi_overobe_uci) %>%
  subset(select = c(SchoolYear,	HBR2014,	ValidReviews,	EpiUnderweight,
                    LCIEpiUnderweight,	UCIEpiUnderweight, EpiHealthyWeight,
                    LCIEpiHealthyWeight,	UCIEpiHealthyWeight,	EpiOverweight,
                    LCIEpiOverweight, UCIEpiOverweight,	EpiObese,	LCIEpiObese,
                    UCIEpiObese,	EpiOverweightAndObese,
                    LCIEpiOverweightAndObese, UCIEpiOverweightAndObese))

# apply function to format school year
hb_open_data_epi <- apply_school_year_format(hb_open_data_epi)

# save hb epi file as csv
write_csv(hb_open_data_epi, paste0(host_folder, "OpenData/OD_P1BMI_HB_Epi.csv"))


## hb clinical
# rename variables
hb_open_data_clin <- hb_open_data %>% 
  rename(SchoolYear = schlyr_exam,
         HBR2014 = HB2019,
         ValidReviews = total_reviews,
         ClinUnderweight = per_clin_undw, 
         LCIClinUnderweight = clin_undw_lci, 
         UCIClinUnderweight = clin_undw_uci,
         ClinHealthyWeight = per_clin_hw, 
         LCIClinHealthyWeight = clin_hw_lci, 
         UCIClinHealthyWeight = clin_hw_uci,
         ClinOverweight = per_clin_over, 
         LCIClinOverweight = clin_over_lci, 
         UCIClinOverweight = clin_over_uci,
         ClinObese = per_clin_obe, 
         LCIClinObese = clin_obe_lci, 
         UCIClinObese = clin_obe_uci, 
         ClinSeverelyObese = per_clin_sobe, 
         LCIClinSeverelyObese = clin_sobe_lci, 
         UCIClinSeverelyObese = clin_sobe_uci, 
         ClinOverweightObeseAndSeverelyObese = per_clin_overwplus, 
         LCIClinOverweightObeseAndSeverelyObese = clin_overwplus_lci, 
         UCIClinOverweightObeseAndSeverelyObese = clin_overwplus_uci,
         ClinObeseAndSeverelyObese = per_clin_obeplus, 
         LCIClinObeseAndSeverelyObese = clin_obeplus_lci, 
         UCIClinObeseAndSeverelyObese = clin_obeplus_uci) %>%
  subset(select = c(SchoolYear,	HBR2014,	ValidReviews,	ClinUnderweight,
                    LCIClinUnderweight, UCIClinUnderweight,	ClinHealthyWeight,
                    LCIClinHealthyWeight,	UCIClinHealthyWeight, ClinOverweight,
                    LCIClinOverweight,	UCIClinOverweight,	ClinObese,
                    LCIClinObese,	UCIClinObese,	ClinSeverelyObese,
                    LCIClinSeverelyObese,	UCIClinSeverelyObese,	
                    ClinOverweightObeseAndSeverelyObese,
                    LCIClinOverweightObeseAndSeverelyObese,
                    UCIClinOverweightObeseAndSeverelyObese,	
                    ClinObeseAndSeverelyObese, LCIClinObeseAndSeverelyObese,
                    UCIClinObeseAndSeverelyObese))  

# apply function to format school year
hb_open_data_clin <- apply_school_year_format(hb_open_data_clin) 

# save hb clin file as csv
write_csv(hb_open_data_clin, paste0(host_folder, "OpenData/OD_P1BMI_HB_Clin.csv"))





ca_open_data <- readRDS(paste0(host_folder, "OpenData/ca_open_data.rds"))

gender_open_data <- readRDS(paste0(host_folder, "OpenData/gender_open_data.rds"))

simd_open_data <- readRDS(paste0(host_folder, "OpenData/simd_open_data.rds"))



### End of script