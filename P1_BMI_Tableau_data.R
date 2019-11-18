### P1 BMI - create Tableau data file.
# create two separate files for Tableau - data file and coverage file.

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
  tableau_folder <- "//PHI_conf/ChildHealthSurveillance/Topics/Obesity/Publications/Primary1BMI/20191210/Tableau/"
} else if (server_desktop == "desktop") {
  host_folder <- "//stats/ChildHealthSurveillance/Topics/Obesity/Publications/Primary1BMI/20191210/RAP/"
  source_folder <- "//stats/ChildHealthSurveillance/Portfolio/Data"
  lookup_folder <-"//Isdsf00d03/cl-out/lookups"
  tableau_folder <- "//PHI_conf/ChildHealthSurveillance/Topics/Obesity/Publications/Primary1BMI/20191210/Tableau/"
}

## Variables
# Current School/Financial Year (e.g. 1819)
current_year <- 1819


### Create the hb file for Tableau aggregating by year, hb, simd, sex ----

# read in the basefile from main P1 BMI script
bmi_basefile <- readRDS(paste0(host_folder, "BMI_data_0102_1819.rds"))

# hb file
# create totals for individual hb and all participating boards
tab_hb_data <- bind_rows(bmi_basefile %>% group_by(schlyr_exam, HB2019Name,
                                                   simd, sex) %>%
                       summarise_at(vars(tot:clin_cent_grp7), sum)
                     %>% ungroup(),
                     # Scotland level (all participating boards)
                     bmi_basefile %>% group_by(schlyr_exam, simd, sex) %>% 
                       summarise_at(vars(tot:clin_cent_grp7), sum) %>%
                       mutate(HB2019Name = "Total") %>% ungroup()) %>% 
  # replace simd 'na' values with 'U' for unknown
  replace_na(list(simd = "U"))


# ca file 
# create totals for individual hb and all participating boards (for ca_data)
# council area level
tab_ca_data <- rbind(bmi_basefile %>% group_by(schlyr_exam, CA2019, CA2019Name,
                                               simd, sex) %>%
                   summarise_at(vars(tot:clin_cent_grp7), sum)
                   %>% ungroup()) %>% 
  # replace simd 'na' values with 'U' for unknown
  replace_na(list(simd = "U")) %>% 
  rename(SchoolYear = schlyr_exam)

# apply school year function from list of open data functions
tab_ca_data <- apply_school_year_format(tab_ca_data)

# apply ca exclusion for years when ca's have less than 50 records
tab_ca_data <- apply_ca_exclusion(tab_ca_data)

# filter out the flagged records
tab_ca_data <- tab_ca_data %>% 
  filter(flag != 1)







# list the variables needed for final data file
# SchoolYear
# YearEnding
# HB_RESIDENCE_DESC
# simd
# sex
# N_Valid_Height_Weight
# Epi_Underweight
# Epi_HealthyWeight
# Epi_Overweight
# Epi_Obese
# Epi_OverweightObese
# Clin_Underweight
# Clin_HealthyWeight
# Clin_Overweight
# Clin_Obese
# Clin_SeverelyObese
# Clin_OverweightObeseSevObese
# Clin_Obese_SevObese
# COUNCIL_AREA_DESC
# flag



### Coverage for Tableau file

### Create the hb coverage file for Tableau ----

# start with the coverage file from the main P1 bmi script
bmi_data_coverage <- readRDS(paste0(host_folder, "bmi_data_coverage.rds"))

# create totals for the number of records with and without a valid height and 
# weight for individual hb and all participating boards (for bmi_data_coverage)
# create a variable for the total number of reviews
bmi_data_coverage <- bmi_data_coverage %>% mutate(count = 1)
# board level
tab_hb_p1rev_data <- rbind(bmi_data_coverage %>% group_by(HB2019Name,
                                                      schlyr_exam) %>%
                         summarise(total_reviews = sum(count))  %>% ungroup(),
                       # Scotland level (all participating boards)
                       bmi_data_coverage %>% group_by(schlyr_exam) %>% 
                         summarise(total_reviews = sum(count)) %>%
                         mutate(HB2019Name = "APB") %>% ungroup())

# create totals for the number of records with a valid height and weight
# for individual hb and all participating boards (for bmi_basefile)
# create a variable for the total number of valid reviews
bmi_basefile <- bmi_basefile %>% mutate(count = 1) 
# board level  
tab_hb_valid_p1rev_data <- rbind(bmi_basefile %>% group_by(HB2019Name, 
                                                       schlyr_exam) %>%
                               summarise(valid_reviews = sum(count))  %>%
                               ungroup(),
                             # Scotland level (all participating boards)
                             bmi_basefile %>% group_by(schlyr_exam) %>%
                               summarise(valid_reviews = sum(count)) %>%
                               mutate(HB2019Name = "APB") %>%
                               ungroup())

# create population file from the GRO mid year population estimates
# of five year olds in each HB and for all participating boards
tab_hb_pop_estimates <- readRDS(paste0(
  lookup_folder, "/Unicode/Populations/Estimates/HB2019_pop_est_1981_2018.rds")) %>%
  rename(year = Year, age = Age, pop = Pop) %>% 
  mutate(HB2019Name = paste0("NHS ", HB2019Name))

# call the function for filtering the population estimate file
tab_hb_pop_estimates <- apply_pop_est_filter(tab_hb_pop_estimates)

# call the function for creating HB cypher
tab_hb_pop_estimates <- apply_hb2019_cypher(tab_hb_pop_estimates) 

# Exclude schlyr 02/03 from Borders data
tab_hb_pop_estimates <- tab_hb_pop_estimates %>%
  subset(!(hb2019_cypher == 'B' & schlyr_exam == '0203'))

# call the function for selecing the relevant year for each board
tab_hb_pop_estimates <- tab_hb_pop_estimates %>%
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

# create totals for individual hb and all 
# participating boards (for hb_pop_estimates)
# Board level
tab_hb_pop_estimates <- rbind(tab_hb_pop_estimates %>% 
                            group_by(HB2019Name, schlyr_exam)%>%
                            summarise(pop = sum(pop)) %>% ungroup(),
                          # Scotland level (all participating boards)
                          tab_hb_pop_estimates %>% group_by(schlyr_exam) %>%
                            summarise(pop = sum(pop)) %>%
                            mutate(HB2019Name = "APB") %>% ungroup())

# Add all health board files (population, all P1 reviews and
# P1 reviews with valid h&w measurements)
tab_hb_completeness_data <- full_join(tab_hb_pop_estimates, tab_hb_p1rev_data, 
                                  by = c("HB2019Name", "schlyr_exam")) %>% 
  full_join(tab_hb_valid_p1rev_data, by = c("HB2019Name", "schlyr_exam"))


### Create the ca coverage file for Tableau ----

# create totals for the number of records with and without a valid height and 
# weight for individual council area (for bmi_data_coverage)
tab_ca_p1rev_data <- rbind(bmi_data_coverage %>% 
                         group_by(CA2019Name, schlyr_exam) %>%
                         summarise(total_reviews = sum(count)) %>% 
                         ungroup()) 

# create totals for the number of records with a valid height and weight
# for individual council area (for bmi_basefile)
tab_ca_valid_p1rev_data <- rbind(bmi_basefile %>% group_by(CA2019Name,
                                                       schlyr_exam) %>%
                               summarise(valid_reviews = sum(count))  %>% 
                               ungroup())

# create population file from the GRO mid year population estimates
# of five year olds in each ca 
tab_ca_pop_estimates <- readRDS(paste0(
  lookup_folder, "/Unicode/Populations/Estimates/CA2019_pop_est_1981_2018.rds")) %>%
  rename(year = Year, age = Age, pop = Pop)

# call the function for filtering the population estimate file
tab_ca_pop_estimates <- apply_pop_est_filter(tab_ca_pop_estimates)


# create totals for individual council areas (for ca_pop_estimates)
# council area level
tab_ca_pop_estimates <- rbind(tab_ca_pop_estimates %>% 
                            group_by(CA2019Name, schlyr_exam)%>%
                            summarise(pop = sum(pop)) %>% ungroup())


# Add all council area files (population, all P1 reviews and
# P1 reviews with valid h&w measurements)
tab_ca_completeness_data <- full_join(tab_ca_pop_estimates, tab_ca_p1rev_data, 
                                  by = c("CA2019Name", "schlyr_exam")) %>% 
  full_join(tab_ca_valid_p1rev_data, by = c("CA2019Name", "schlyr_exam")) %>% 
  subset(total_reviews >50)
  

### add both hb and ca coverage files together ----

tab_coverage_data <- bind_rows(tab_hb_completeness_data,
                               tab_ca_completeness_data)

# format data to the correct naming convention for Tableau
tab_coverage_data <- tab_coverage_data %>% 
  rename(pop_estimate = pop,
         tot_revs = total_reviews,
         valid_revs = valid_reviews,
         HB_RESIDENCE_DESC = HB2019Name,
         SchoolYear = schlyr_exam,
         COUNCIL_AREA_DESC = CA2019Name)

# apply school year formatting function
tab_coverage_data <- apply_school_year_format(tab_coverage_data)


# apply the function to create YearEnding variable
tab_coverage_data <- apply_year_ending_variable(tab_coverage_data) %>%
# select out only the required variables  
  subset(select = c(pop_estimate, tot_revs, valid_revs, 
                    HB_RESIDENCE_DESC, SchoolYear, YearEnding,
                    COUNCIL_AREA_DESC)) %>% 
# ensure both hb and ca descriptions contain "all participating boards"
  mutate(HB_RESIDENCE_DESC = case_when(
    HB_RESIDENCE_DESC == "APB" ~ "All Participating Boards",
    TRUE ~ HB_RESIDENCE_DESC)) %>%
  mutate(COUNCIL_AREA_DESC = case_when(
    HB_RESIDENCE_DESC == "All Participating Boards" ~ "All Participating Boards",
    TRUE ~ COUNCIL_AREA_DESC))

# save out the final coverage file for Tableau
haven::write_sav(tab_coverage_data,
                 file.path(tableau_folder,
                           "P1_coverage_data.sav"), compress = FALSE)






#### END OF SCRIPT ### ----