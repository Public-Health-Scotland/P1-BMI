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

## Variables
# Current School/Financial Year (e.g. 1819)
current_year <- 1819


# read in bmi basefile
bmi_basefile <- readRDS(paste0(host_folder, "BMI_data_0102_1819.rds"))

# read in coverage file
bmi_data_coverage <- readRDS(paste0(host_folder, "bmi_data_coverage.rds"))


### HB open data analysis ----
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
hb_open_data_epi <- apply_school_year_format(hb_open_data_epi) %>% 
  mutate_all(., replace_na, 0) 

# save file as csv
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

# apply function to format school year and get rid of all NaN values
hb_open_data_clin <- apply_school_year_format(hb_open_data_clin) %>% 
  mutate_all(., replace_na, 0) 

# save file as csv
write_csv(hb_open_data_clin, paste0(host_folder, "OpenData/OD_P1BMI_HB_Clin.csv"))


### CA open data analysis ----
# start with the file created within the main P1 bmi script to create both
# the epidemiological and clinical open data files
ca_open_data <- readRDS(paste0(host_folder, "OpenData/ca_open_data.rds"))

## hb epidemiological
# rename variables
ca_open_data_epi <- ca_open_data %>% 
  rename(SchoolYear = schlyr_exam,
         CA2011 = CA2019, 
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
  subset(select = c(SchoolYear,	CA2011,	ValidReviews,	EpiUnderweight,
                    LCIEpiUnderweight,	UCIEpiUnderweight, EpiHealthyWeight,
                    LCIEpiHealthyWeight,	UCIEpiHealthyWeight,	EpiOverweight,
                    LCIEpiOverweight, UCIEpiOverweight,	EpiObese,	LCIEpiObese,
                    UCIEpiObese,	EpiOverweightAndObese,
                    LCIEpiOverweightAndObese, UCIEpiOverweightAndObese))

# apply function to format school year
ca_open_data_epi <- apply_school_year_format(ca_open_data_epi) %>% 
  mutate_all(., replace_na, 0) 

# save file as csv
write_csv(ca_open_data_epi, paste0(host_folder, "OpenData/OD_P1BMI_CA_Epi.csv"))


## ca clinical
# rename variables
ca_open_data_clin <- ca_open_data %>% 
  rename(SchoolYear = schlyr_exam,
         CA2011 = CA2019,
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
  subset(select = c(SchoolYear,	CA2011,	ValidReviews,	ClinUnderweight,
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
ca_open_data_clin <- apply_school_year_format(ca_open_data_clin) %>% 
  mutate_all(., replace_na, 0) 

# save file as csv
write_csv(ca_open_data_clin, paste0(host_folder, "OpenData/OD_P1BMI_CA_Clin.csv"))



### Gender open data analysis for hb and ca ----

# use the bmi basefile as the starting point for hb gender open data files
# hb gender epidemiological
hb_gender_open_data <- rbind(bmi_basefile %>% 
                            group_by(schlyr_exam, HB2019, sex) %>%
                       summarise_at(vars(tot:clin_cent_grp7), sum) %>% 
                         ungroup())

# rename variables
hb_gender_open_data_epi <- hb_gender_open_data %>% 
  rename(SchoolYear = schlyr_exam,
         HBR2014 = HB2019,
         Sex = sex,
         ValidReviews = tot,
         EpiUnderweight = cent_grp1,
         EpiHealthyWeight = cent_grp2,
         EpiOverweight = cent_grp3,
         EpiObese = cent_grp4,
         EpiOverweightAndObese = cent_grp5) %>%
  subset(select = c(SchoolYear,	HBR2014, Sex, ValidReviews,	EpiUnderweight,
                    EpiHealthyWeight, EpiOverweight, 
                    EpiObese,	EpiOverweightAndObese))

# apply function to format school year
hb_gender_open_data_epi <- apply_school_year_format(hb_gender_open_data_epi)

# save file as csv
write_csv(hb_gender_open_data_epi, paste0(host_folder, "OpenData/OD_P1BMI_HB_Gender_Epi.csv"))


# hb gender clinical
# rename variables
hb_gender_open_data_clin <- hb_gender_open_data %>% 
  rename(SchoolYear = schlyr_exam,
         HBR2014 = HB2019,
         Sex = sex,
         ValidReviews = tot,
         ClinUnderweight = clin_cent_grp1,
         ClinHealthyWeight = clin_cent_grp2,
         ClinOverweight = clin_cent_grp3,
         ClinObese = clin_cent_grp4,
         ClinSeverelyObese = clin_cent_grp5,
         ClinOverweightObeseAndSeverelyObese = clin_cent_grp6,
         ClinObeseAndSeverelyObese = clin_cent_grp7) %>%
  subset(select = c(SchoolYear, HBR2014, Sex,	ValidReviews,	ClinUnderweight,	
                    ClinHealthyWeight, ClinOverweight, ClinObese,
                    ClinSeverelyObese, ClinOverweightObeseAndSeverelyObese,
                    ClinObeseAndSeverelyObese))

# apply function to format school year
hb_gender_open_data_clin <- apply_school_year_format(hb_gender_open_data_clin) 

# save file as csv
write_csv(hb_gender_open_data_clin, paste0(host_folder, "OpenData/OD_P1BMI_HB_Gender_Clin.csv"))


# use the bmi basefile as the starting point for ca gender open data files
# ca gender epidemiological
ca_gender_open_data <- rbind(bmi_basefile %>% 
                               group_by(schlyr_exam, CA2019, sex) %>%
                               summarise_at(vars(tot:clin_cent_grp7), sum) %>% 
                               ungroup())

# rename variables
ca_gender_open_data_epi <- ca_gender_open_data %>% 
  rename(SchoolYear = schlyr_exam,
         CA2011 = CA2019,
         Sex = sex,
         ValidReviews = tot,
         EpiUnderweight = cent_grp1,
         EpiHealthyWeight = cent_grp2,
         EpiOverweight = cent_grp3,
         EpiObese = cent_grp4,
         EpiOverweightAndObese = cent_grp5) %>%
  subset(select = c(SchoolYear,	CA2011, Sex, ValidReviews,	EpiUnderweight,
                    EpiHealthyWeight, EpiOverweight, 
                    EpiObese,	EpiOverweightAndObese))

# apply function to format school year
ca_gender_open_data_epi <- apply_school_year_format(ca_gender_open_data_epi)

# save file as csv
write_csv(ca_gender_open_data_epi, paste0(host_folder, "OpenData/OD_P1BMI_CA_Gender_Epi.csv"))


# ca gender clinical
# rename variables
ca_gender_open_data_clin <- ca_gender_open_data %>% 
  rename(SchoolYear = schlyr_exam,
         CA2011 = CA2019,
         Sex = sex,
         ValidReviews = tot,
         ClinUnderweight = clin_cent_grp1,
         ClinHealthyWeight = clin_cent_grp2,
         ClinOverweight = clin_cent_grp3,
         ClinObese = clin_cent_grp4,
         ClinSeverelyObese = clin_cent_grp5,
         ClinOverweightObeseAndSeverelyObese = clin_cent_grp6,
         ClinObeseAndSeverelyObese = clin_cent_grp7) %>%
  subset(select = c(SchoolYear, CA2011, Sex,	ValidReviews,	ClinUnderweight,	
                    ClinHealthyWeight, ClinOverweight, ClinObese,
                    ClinSeverelyObese, ClinOverweightObeseAndSeverelyObese,
                    ClinObeseAndSeverelyObese))

# apply function to format school year
ca_gender_open_data_clin <- apply_school_year_format(ca_gender_open_data_clin) 

# save file as csv
write_csv(ca_gender_open_data_clin, paste0(host_folder, "OpenData/OD_P1BMI_CA_Gender_Clin.csv"))



### SIMD open data analysis for hb and ca ----

# use the bmi basefile as the starting point for all simd open data files
# hb simd epidemiological
hb_simd_open_data <- rbind(bmi_basefile %>% group_by(simd, HB2019, schlyr_exam) %>%
                     summarise_at(vars(tot:clin_cent_grp7), sum)  %>% ungroup())

# rename variables
hb_simd_open_data_epi <- hb_simd_open_data %>% 
  rename(SchoolYear = schlyr_exam,
         HBR2014 = HB2019,
         SIMD = simd,
         ValidReviews = tot,
         EpiUnderweight = cent_grp1,
         EpiHealthyWeight = cent_grp2,
         EpiOverweight = cent_grp3,
         EpiObese = cent_grp4,
         EpiOverweightAndObese = cent_grp5) %>%
  subset(select = c(SchoolYear,	HBR2014, SIMD, ValidReviews,	EpiUnderweight,
                    EpiHealthyWeight, EpiOverweight, 
                    EpiObese,	EpiOverweightAndObese))

# apply function to format school year
hb_simd_open_data_epi <- apply_school_year_format(hb_simd_open_data_epi)

# save file as csv
write_csv(hb_simd_open_data_epi, paste0(host_folder, "OpenData/OD_P1BMI_HB_SIMD_Epi.csv"))


# hb simd clinical
# rename variables
hb_simd_open_data_clin <- hb_simd_open_data %>% 
  rename(SchoolYear = schlyr_exam,
         HBR2014 = HB2019,
         SIMD = simd,
         ValidReviews = tot,
         ClinUnderweight = clin_cent_grp1,
         ClinHealthyWeight = clin_cent_grp2,
         ClinOverweight = clin_cent_grp3,
         ClinObese = clin_cent_grp4,
         ClinSeverelyObese = clin_cent_grp5,
         ClinOverweightObeseAndSeverelyObese = clin_cent_grp6,
         ClinObeseAndSeverelyObese = clin_cent_grp7) %>%
  subset(select = c(SchoolYear, HBR2014, SIMD,	ValidReviews,	ClinUnderweight,	
                    ClinHealthyWeight, ClinOverweight, ClinObese,
                    ClinSeverelyObese, ClinOverweightObeseAndSeverelyObese,
                    ClinObeseAndSeverelyObese))

# apply function to format school year
hb_simd_open_data_clin <- apply_school_year_format(hb_simd_open_data_clin) 

# save file as csv
write_csv(hb_simd_open_data_clin, paste0(host_folder, "OpenData/OD_P1BMI_HB_SIMD_Clin.csv"))


# use the bmi basefile as the starting point for all simd open data files
# ca simd epidemiological
ca_simd_open_data <- rbind(bmi_basefile %>% group_by(simd, CA2019, schlyr_exam) %>%
                             summarise_at(vars(tot:clin_cent_grp7), sum)  %>% ungroup())

# rename variables
ca_simd_open_data_epi <- ca_simd_open_data %>% 
  rename(SchoolYear = schlyr_exam,
         CA2011 = CA2019,
         SIMD = simd,
         ValidReviews = tot,
         EpiUnderweight = cent_grp1,
         EpiHealthyWeight = cent_grp2,
         EpiOverweight = cent_grp3,
         EpiObese = cent_grp4,
         EpiOverweightAndObese = cent_grp5) %>%
  subset(select = c(SchoolYear,	CA2011, SIMD, ValidReviews,	EpiUnderweight,
                    EpiHealthyWeight, EpiOverweight, 
                    EpiObese,	EpiOverweightAndObese))

# apply function to format school year
ca_simd_open_data_epi <- apply_school_year_format(ca_simd_open_data_epi)

# save file as csv
write_csv(ca_simd_open_data_epi, paste0(host_folder, "OpenData/OD_P1BMI_CA_SIMD_Epi.csv"))


# ca simd clinical
# rename variables
ca_simd_open_data_clin <- ca_simd_open_data %>% 
  rename(SchoolYear = schlyr_exam,
         CA2011 = CA2019,
         SIMD = simd,
         ValidReviews = tot,
         ClinUnderweight = clin_cent_grp1,
         ClinHealthyWeight = clin_cent_grp2,
         ClinOverweight = clin_cent_grp3,
         ClinObese = clin_cent_grp4,
         ClinSeverelyObese = clin_cent_grp5,
         ClinOverweightObeseAndSeverelyObese = clin_cent_grp6,
         ClinObeseAndSeverelyObese = clin_cent_grp7) %>%
  subset(select = c(SchoolYear, CA2011, SIMD,	ValidReviews,	ClinUnderweight,	
                    ClinHealthyWeight, ClinOverweight, ClinObese,
                    ClinSeverelyObese, ClinOverweightObeseAndSeverelyObese,
                    ClinObeseAndSeverelyObese))

# apply function to format school year
ca_simd_open_data_clin <- apply_school_year_format(ca_simd_open_data_clin) 

# save file as csv
write_csv(ca_simd_open_data_clin, paste0(host_folder, "OpenData/OD_P1BMI_CA_SIMD_Clin.csv"))


### HB open data coverage ----

## create totals for the number of records with and without a valid height and 
## weight for individual hb and add the population estimates

# create population estimate for each hb
hb_open_data_pop_estimates <- readRDS(paste0(
  lookup_folder, "/Unicode/Populations/Estimates/HB2019_pop_est_1981_2018.rds")) %>%
  rename(year = Year, age = Age, pop = Pop)

# call the function for filtering the population estimate file
hb_open_data_pop_estimates <- apply_pop_est_filter(hb_open_data_pop_estimates)

# call the function for creating HB cypher
hb_open_data_pop_estimates <- apply_hb2019_cypher(hb_open_data_pop_estimates) 

# Exclude schlyr 02/03 from Borders data
hb_open_data_pop_estimates <- hb_open_data_pop_estimates %>%
  subset(!(hb2019_cypher == 'B' & schlyr_exam == '0203'))

# call the function for selecing the relevant year for each board
hb_open_data_pop_estimates <- hb_open_data_pop_estimates %>%
  apply_hb_year(HB = 'F', ey = 102, cy = current_year) %>% 
  apply_hb_year(HB = 'L', ey = 102, cy = current_year) %>% 
  apply_hb_year(HB = 'S', ey = 102, cy = current_year) %>% 
  apply_hb_year(HB = 'T', ey = 203, cy = current_year) %>% 
  apply_hb_year(HB = 'W', ey = 304, cy = current_year) %>% 
  apply_hb_year(HB = 'Y', ey = 405, cy = current_year) %>% 
  apply_hb_year(HB = 'V', ey = 506, cy = current_year) %>% 
  apply_hb_year(HB = 'G', ey = 607, cy = current_year) %>% 
  apply_hb_year(HB = 'A', ey = 708, cy = current_year) %>% 
  apply_hb_year(HB = 'H', ey = 809, cy = current_year) %>% 
  apply_hb_year(HB = 'Z', ey = 809, cy = current_year) %>% 
  apply_hb_year(HB = 'N', ey = 910, cy = current_year) %>% 
  apply_hb_year(HB = 'R', ey = 1011, cy = current_year) 

# create totals for individual hb 
hb_open_data_pop_estimates <- rbind(hb_open_data_pop_estimates %>% 
                                      group_by(HB2019, schlyr_exam) %>%
                                      summarise(pop = sum(pop)) %>% ungroup())

# read in the coverage file from the main p1 bmi script
bmi_data_coverage <- readRDS(paste0(host_folder, "bmi_data_coverage.rds"))


# create a variable for the total number of reviews
bmi_data_coverage <- bmi_data_coverage %>% mutate(count = 1)
# board level
hb_p1rev_open_data <- rbind(bmi_data_coverage %>% group_by(HB2019 ,schlyr_exam) %>%
                         summarise(total_reviews = sum(count))  %>% ungroup())

# create a variable for the total number of valid reviews
bmi_basefile <- bmi_basefile %>% mutate(count = 1) 
# board level  
hb_valid_p1rev_open_data <- rbind(bmi_basefile %>% group_by(HB2019, schlyr_exam) %>%
                               summarise(valid_reviews = sum(count))  %>%
                               ungroup())

# add files together containing the total reviews, valid reviews and 
# population estimates. rename variables into open data format
hb_coverage_open_data <- full_join(hb_open_data_pop_estimates,
                                   hb_p1rev_open_data, 
                                  by = c("HB2019", "schlyr_exam")) %>% 
  full_join(hb_valid_p1rev_open_data, by = c("HB2019", "schlyr_exam")) %>%
  rename(HBR2014 = HB2019,
       SchoolYear = schlyr_exam,
       PopulationEstAge5 = pop,
       TotalReviews = total_reviews,
       ValidReviews = valid_reviews)

# apply function to format school year
hb_coverage_open_data <- apply_school_year_format(hb_coverage_open_data) 

# save file as csv
write_csv(hb_coverage_open_data, paste0(host_folder, "OpenData/OD_P1BMI_Coverage_Board.csv"))


### CA open data coverage ----

## create totals for the number of records with and without a valid height and 
## weight for individual ca and add the population estimates

# create population estimate for each ca
ca_open_data_pop_estimates <- readRDS(paste0(
  lookup_folder, "/Unicode/Populations/Estimates/CA2019_pop_est_1981_2018.rds")) %>%
  rename(year = Year, age = Age, pop = Pop)

# call the function for filtering the population estimate file
ca_open_data_pop_estimates <- apply_pop_est_filter(ca_open_data_pop_estimates)


# create totals for individual council areas (for ca_pop_estimates)
# council area level
ca_open_data_pop_estimates <- rbind(ca_open_data_pop_estimates %>%
                                      group_by(CA2019, schlyr_exam) %>%
                                      summarise(pop = sum(pop)) %>% ungroup())

# create a variable for the total number of reviews
ca_p1rev_open_data <- rbind(bmi_data_coverage %>% 
                         group_by(CA2019, schlyr_exam)%>%
                         summarise(total_reviews = sum(count)) %>% 
                         ungroup())

# create totals for the number of records with a valid height and weight
# for individual council area (for bmi_basefile)
ca_valid_p1rev_open_data <- rbind(bmi_basefile %>% group_by(CA2019, 
                                                            schlyr_exam) %>%
                               summarise(valid_reviews = sum(count))  %>% 
                               ungroup())

# add files together containing the total reviews, valid reviews and 
# population estimates. rename variables into open data format
ca_coverage_open_data <- full_join(ca_open_data_pop_estimates,
                                   ca_p1rev_open_data, 
                                   by = c("CA2019", "schlyr_exam")) %>% 
  full_join(ca_valid_p1rev_open_data, by = c("CA2019", "schlyr_exam")) %>%
  rename(CA2011 = CA2019,
         SchoolYear = schlyr_exam,
         PopulationEstAge5 = pop,
         TotalReviews = total_reviews,
         ValidReviews = valid_reviews) %>% 
  subset(TotalReviews >50)

# apply function to format school year
ca_coverage_open_data <- apply_school_year_format(ca_coverage_open_data) 

# save file as csv
write_csv(ca_coverage_open_data, paste0(host_folder, "OpenData/OD_P1BMI_Coverage_CA.csv"))


### End of script