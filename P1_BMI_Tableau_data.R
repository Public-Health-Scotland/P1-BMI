### P1 BMI - create Tableau data files.
# create 3 separate files for Tableau:
# 1 - combined hb and ca bmi data file.
# 2 - donut chart data.
# 3 - combined hb and ca coverage file.

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


# read in the basefile from main P1 BMI script
bmi_basefile <- readRDS(paste0(host_folder, "BMI_data_0102_1819.rds"))

### Create the hb file for Tableau aggregating by year, hb, simd, sex ----

# hb file
# create totals for individual hb and all participating boards
tab_hb_data <- bind_rows(bmi_basefile %>% group_by(schlyr_exam, HB2019Name,
                                                   simd, sex) %>%
                       summarise_at(vars(tot:clin_cent_grp7), sum)
                     %>% ungroup(),
                     # Scotland level (all participating boards)
                     bmi_basefile %>% group_by(schlyr_exam, simd, sex) %>% 
                       summarise_at(vars(tot:clin_cent_grp7), sum) %>%
                       mutate(HB2019Name = "All Participating Boards") %>% 
                       ungroup()) %>% 
  # replace simd 'na' values with 'U' for unknown
  replace_na(list(simd = "U")) %>% 
  rename(SchoolYear = schlyr_exam,
         HB_RESIDENCE_DESC = HB2019Name,
         N_Valid_Height_Weight = tot,
         Epi_Underweight = cent_grp1,
         Epi_HealthyWeight = cent_grp2,
         Epi_Overweight = cent_grp3,
         Epi_Obese = cent_grp4,
         Epi_OverweightObese = cent_grp5,
         Clin_Underweight = clin_cent_grp1,
         Clin_HealthyWeight = clin_cent_grp2,
         Clin_Overweight = clin_cent_grp3,
         Clin_Obese = clin_cent_grp4,
         Clin_SeverelyObese = clin_cent_grp5,
         Clin_OverweightObeseSevObese = clin_cent_grp6,
         Clin_Obese_SevObese = clin_cent_grp7)

# call the function to format school year
tab_hb_data <- apply_school_year_format(tab_hb_data)

# call the function to create YearEnding variable
tab_hb_data <- apply_year_ending_variable(tab_hb_data)

# re-order the variables
tab_hb_data <- tab_hb_data %>% 
  subset(select = c(SchoolYear, YearEnding, HB_RESIDENCE_DESC, simd,
                    sex, N_Valid_Height_Weight, Epi_Underweight, 
                    Epi_HealthyWeight, Epi_Overweight, Epi_Obese, 
                    Epi_OverweightObese, Clin_Underweight, Clin_HealthyWeight,
                    Clin_Overweight, Clin_Obese, Clin_SeverelyObese, 
                    Clin_OverweightObeseSevObese, Clin_Obese_SevObese))


### Create the ca file for Tableau aggregating by year, hb, simd, sex ----

# ca file
# create totals for individual ca's
tab_ca_data <- rbind(bmi_basefile %>% group_by(schlyr_exam, CA2019, CA2019Name,
                                               simd, sex) %>%
                   summarise_at(vars(tot:clin_cent_grp7), sum)
                   %>% ungroup()) %>% 
  # replace simd 'na' values with 'U' for unknown
  replace_na(list(simd = "U")) %>% 
  rename(SchoolYear = schlyr_exam,
         COUNCIL_AREA_DESC = CA2019Name,
         N_Valid_Height_Weight = tot,
         Epi_Underweight = cent_grp1,
         Epi_HealthyWeight = cent_grp2,
         Epi_Overweight = cent_grp3,
         Epi_Obese = cent_grp4,
         Epi_OverweightObese = cent_grp5,
         Clin_Underweight = clin_cent_grp1,
         Clin_HealthyWeight = clin_cent_grp2,
         Clin_Overweight = clin_cent_grp3,
         Clin_Obese = clin_cent_grp4,
         Clin_SeverelyObese = clin_cent_grp5,
         Clin_OverweightObeseSevObese = clin_cent_grp6,
         Clin_Obese_SevObese = clin_cent_grp7)
         

# call the function to format school year (from list of open data functions)
tab_ca_data <- apply_school_year_format(tab_ca_data)

# call the function to create YearEnding variable
tab_ca_data <- apply_year_ending_variable(tab_ca_data)

# call the function to flag ca for years when ca's have less than 50 records
tab_ca_data <- apply_ca_exclusion(tab_ca_data)

# filter out the flagged records and re-order variables
tab_ca_data <- tab_ca_data %>% 
  filter(flag != 1) %>% 
  subset(select = c(SchoolYear, YearEnding, simd,
                    sex, N_Valid_Height_Weight, Epi_Underweight, 
                    Epi_HealthyWeight, Epi_Overweight, Epi_Obese, 
                    Epi_OverweightObese, Clin_Underweight, Clin_HealthyWeight,
                    Clin_Overweight, Clin_Obese, Clin_SeverelyObese, 
                    Clin_OverweightObeseSevObese, Clin_Obese_SevObese,
                    COUNCIL_AREA_DESC, flag))

# add both the hb and ca files together
tableau_data <- bind_rows(tab_hb_data, tab_ca_data) %>% 
  mutate_all(., replace_na, 0) %>% 
  mutate(COUNCIL_AREA_DESC = case_when(
    HB_RESIDENCE_DESC == "All Participating Boards" ~ "All Participating Boards",
    TRUE ~ COUNCIL_AREA_DESC))

# save the final data file for Tableau as SPSS .sav file (1 of 3) ----
haven::write_sav(tableau_data,
                 file.path(tableau_folder,
                           "P1BMI_agg.sav"), compress = FALSE)


### donut chart data, separate hb and ca files required ----

# hb file ----
# create epidemiological grouping variable
tab_donut_hb_data <- bmi_basefile %>% 
  mutate(Epidemiological_Grouping = case_when(
    cent_grp1 == 1 ~ "at risk of underweight",
    cent_grp2 == 1 ~ "healthy weight",
    cent_grp3 == 1 ~ "at risk of overweight",
    cent_grp4 == 1 ~ "at risk of obesity")) %>% 
# create clinical grouping variable
  mutate(Clinical_Grouping = case_when(
    clin_cent_grp1 == 1 ~ "clinically underweight",
    clin_cent_grp2 == 1 ~ "clinically healthy weight",
    clin_cent_grp3 == 1 ~ "clinically overweight",
    clin_cent_grp4 == 1 ~ "clinically obese",
    clin_cent_grp5 == 1 ~ "clinically severely obese")) %>% 
  rename(SchoolYear = schlyr_exam,
         N_Records = tot,
         HB_RESIDENCE_DESC = HB2019Name)

# call the function to format school year
tab_donut_hb_data <- apply_school_year_format(tab_donut_hb_data)

# call the function to create YearEnding variable
tab_donut_hb_data <- apply_year_ending_variable(tab_donut_hb_data)


# aggregate data for individual hb and for all participating boards
tab_donut_hb_data <- rbind(tab_donut_hb_data %>%
                             group_by(SchoolYear, YearEnding, 
                                      Epidemiological_Grouping, 
                                      Clinical_Grouping, simd, sex, 
                                      HB_RESIDENCE_DESC) %>%
                             summarise(N_Records = sum(N_Records)) %>%
                             ungroup(),
                           # Scotland level (all participating boards)
                           tab_donut_hb_data %>% 
                             group_by(SchoolYear, YearEnding, 
                                      Epidemiological_Grouping, 
                                      Clinical_Grouping, simd, sex) %>%
                             summarise(N_Records = sum(N_Records)) %>%
                             mutate(HB_RESIDENCE_DESC = 
                                      "All Participating Boards") %>%
                             ungroup()) %>% 
  replace_na(list(simd = "U"))


# ca file ----
# create epidemiological grouping variable
tab_donut_ca_data <- bmi_basefile %>% 
  mutate(Epidemiological_Grouping = case_when(
    cent_grp1 == 1 ~ "at risk of underweight",
    cent_grp2 == 1 ~ "healthy weight",
    cent_grp3 == 1 ~ "at risk of overweight",
    cent_grp4 == 1 ~ "at risk of obesity")) %>% 
  # create clinical grouping variable
  mutate(Clinical_Grouping = case_when(
    clin_cent_grp1 == 1 ~ "clinically underweight",
    clin_cent_grp2 == 1 ~ "clinically healthy weight",
    clin_cent_grp3 == 1 ~ "clinically overweight",
    clin_cent_grp4 == 1 ~ "clinically obese",
    clin_cent_grp5 == 1 ~ "clinically severely obese")) %>% 
  rename(SchoolYear = schlyr_exam,
         COUNCIL_AREA_DESC = CA2019Name,
         N_Records = tot)

# call the function to format school year
tab_donut_ca_data <- apply_school_year_format(tab_donut_ca_data)

# call the function to create YearEnding variable
tab_donut_ca_data <- apply_year_ending_variable(tab_donut_ca_data)

# call the function to flag ca for years when ca's have less than 50 records
tab_donut_ca_data <- apply_ca_exclusion(tab_donut_ca_data) %>% 
  # filter out the flagged records
  filter(flag != 1)


# aggregate data
tab_donut_ca_data <- rbind(tab_donut_ca_data %>%
                             group_by(SchoolYear, YearEnding, COUNCIL_AREA_DESC,
                                      Epidemiological_Grouping,
                                      Clinical_Grouping, simd, sex) %>%
                             summarise(N_Records = sum(N_Records)) %>%
                             ungroup()) %>% 
  replace_na(list(simd = "U"))


# add both the hb and ca files together
tab_donut_data <- bind_rows(tab_donut_hb_data, tab_donut_ca_data) %>% 
  mutate_all(., replace_na, 0) %>% 
  mutate(COUNCIL_AREA_DESC = case_when(
    HB_RESIDENCE_DESC == "All Participating Boards" ~ "All Participating Boards",
    TRUE ~ COUNCIL_AREA_DESC)) %>% 
  # re-order variables
  subset(select = c(SchoolYear, YearEnding, COUNCIL_AREA_DESC, 
         Epidemiological_Grouping, Clinical_Grouping, 
         simd, sex, N_Records, HB_RESIDENCE_DESC))

# save the final donut chart data file for Tableau as SPSS .sav file (2 of 3) ----
haven::write_sav(tab_donut_data,
                 file.path(tableau_folder,
                           "P1BMI_donut_data.sav"), compress = FALSE)


### Coverage for Tableau file ----

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
                               summarise(valid_reviews = sum(count)) %>%
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

# call the function to format school year
tab_coverage_data <- apply_school_year_format(tab_coverage_data)

# call the function to create YearEnding variable
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

# save the final coverage file for Tableau as SPSS .sav file (3 of 3) ----
haven::write_sav(tab_coverage_data,
                 file.path(tableau_folder,
                           "P1_coverage_data.sav"), compress = FALSE)


### END OF SCRIPT ### ----