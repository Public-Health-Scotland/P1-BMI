#### Script Information ----
# Population Health, Child Health: Primary 1 BMI Statistics Publication Syntax
# Original Author: Russell McCreath
# Original Date: February 2019
# Latest update author:
# Latest update date:
# Latest update description:
# Written/run on: R Studio 1.1.453
# Version of R: 3.5.1
# Description: Data preparation script for P1 BMI Statistics publication.
# Approximate run time: ? min
#### ### ### ### ### ### ### ### ### ### ### ### ### ### ### ###


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
  lookupFolder <- "/conf/linkage/output/lookups"
} else if (server_desktop == "desktop") {
  host_folder <- "//PHI_conf/ChildHealthSurveillance/Topics/Obesity/Publications/Primary1BMI/20191210/RAP/"
  source_folder <- "//stats/ChildHealthSurveillance/Portfolio/Data"
  lookupFolder <-"//Isdsf00d03/cl-out/lookups"
}

## Variables
# Current School/Financial Year (e.g. 1819)
currentYr <- 1718


### 2 - Data Import / Inital Sort ----

## Import Data
# Created R file for efficiency (SPSS syntax below)
# bmi_data <- read_spss(paste(file.path(host_folder, "school_results.zsav")))
bmi_data <- readRDS(paste(file.path(source_folder, "20181008", "School", "school_results.rds")))            #1.614m obs.

## Subset for required variables only
bmi_data <- subset(bmi_data, select = c(chi, schlyr_exam, id, rev_num, schlgivn, height, weight, bmi,
                                        centile_h, centile_w, centile_bmi, date_hw, schlyr_hw,
                                        date_exam, hb_exam, pc_review))

## Sort
# Restrict to relevant years (var: schlyr_exam)
bmi_data <- bmi_data %>%
  filter(as.integer(schlyr_exam) >= 0102 & as.integer(schlyr_exam) <= 1718)       #1.541m obs.

# Restrict to review number 60/61 (var: rev_num)
bmi_data <- bmi_data %>%
  filter(rev_num == "60" | rev_num == "61")                                       #688,520 obs.

# Extract blank height/weight valued records
blankData <- bmi_data %>%
  filter(is.na(height) | height == "0000" | is.na(weight) | weight == "00000")     #13,266 obs.

# Remove blank height/weight
bmi_data <- bmi_data %>%
  filter((!is.na(height) & height != "0000") & (!is.na(weight) & weight != "00000"))   #675,254 obs.


# Define single date variable
bmi_data$daterec <- if_else(bmi_data$date_hw != "00000000", bmi_data$date_hw, bmi_data$date_exam)


# Select single record per CHI (only using review 61 where there's no review 60 - var:rev_num)
bmi_data <- bmi_data %>%
  arrange(chi, daterec) %>% 
  group_by(chi) %>%
  mutate(rec_num = row_number()) %>% 
  filter(rec_num == min(rec_num)) %>% 
  select(-rec_num)                                                               #652,916 obs.


### 3 - Match Postcodes ----
# School year 2010/11 is first full cohort to have postcode recorded (chiQ).
# For years 2001/02 to 2009/10, postcode is matched using lookup/reference file.

## Postcode match for 2001/02 to 2009/10
# Import postcode reference file
pcRef <- readRDS(file.path(source_folder, "ReferenceFiles", "postcode_at_review_2000_2010_school.rds"))

# Merge bmi_data with the postcode reference file
bmi_data <- merge(bmi_data, pcRef[,c("chi", "postcode", "schlyr_exam")],
                  by = c("chi", "schlyr_exam"), all.x = TRUE, all.y = FALSE)

# Create one postcode variable using pc_review & the postcode reference
bmi_data$pc_review <- if_else(bmi_data$pc_review == "", bmi_data$postcode, bmi_data$pc_review)


# Remove redundant postcode variable
bmi_data <- subset(bmi_data, select = -c(postcode))

# Remove postcode reference file pcRef
rm(pcRef)
gc()


## Postcode match for 2010/11 on (chiQ)
# Create variable to show number of quarters from Jan 1990 (J-M = 1)
bmi_data$examQN <- floor((difftime(as.Date(bmi_data$daterec, '%Y%m%d'), as.Date("19900101", '%Y%m%d'),
                                   units = "days")/365)*4+1)

# New variable to concatenate chi and examQN
bmi_data$chiQ <- paste(bmi_data$chi, bmi_data$examQN, sep = "")

# Import reference file
pcQRef <- haven::read_spss(file.path(source_folder, "Cohorts", "chiQ.zsav"))

# Merge bmi_data with postcode reference file
bmi_data <- merge(bmi_data, pcQRef[,c("chiQ", "pcodeCHI")], by = c("chiQ"), all.x = TRUE, all.y = FALSE)

# Remove postcode reference file pcQRef
rm(pcQRef)
gc()

# Create one postcode variable (pc7)
bmi_data$pc7 <- if_else(bmi_data$pc_review != "", bmi_data$pc_review, bmi_data$pcodeCHI)

# Remove unnecessary variables
bmi_data <- subset(bmi_data, select = -c(chiQ, examQN, pcodeCHI))

## Add CA, HSCP, etc. from lookup
# Import Reference File
pcd <- read_rds(file.path(lookupFolder, "Unicode/Geography/Scottish Postcode Directory", 
                          "Scottish_Postcode_Directory_2019_2.rds"))

## Merge data
bmi_data <- merge(bmi_data, pcd, by = c("pc7"), all.x = TRUE, all.y = FALSE)

# Remove reference file
rm(pcd)
gc()


### 4 - Health Board Data Sort ----

## Recode HB variable to single character cypher
bmi_data$HB2018 <- bmi_data$HB2018 %>%
  recode('S08000015' = 'A',
         'S08000016' = 'B',
         'S08000017' = 'Y',
         'S08000019' = 'V',
         'S08000020' = 'N',
         'S08000021' = 'G',
         'S08000022' = 'H',
         'S08000023' = 'L',
         'S08000024' = 'S',
         'S08000025' = 'R',
         'S08000026' = 'Z',
         'S08000028' = 'W',
         'S08000029' = 'F',
         'S08000030' = 'T')

## Exclude cases
# Exclude West Lothian for 2007/08 unless school attendance is outwith West Lothian
bmi_data_1 <- bmi_data %>%
  subset(!(HB2018 == 'S' & CA2018 == 'S12000040' & (schlyr_exam == "0607" | schlyr_exam == "0708")))  # 650,955 obs.

# Extract west lothian excluded data
blankDataWestLothian <- bmi_data %>%
  subset(HB2018 == 'S' & CA2018 == 'S12000040' & (schlyr_exam == "0607" | schlyr_exam == "0708"))  #1,501 obs.

# Exclude Kircaldy schools during 2008/09
kircaldy <- c('F735L','F736L','F737L','F738L','F739L','F740L','F741L',
              'F743L','F744L','F745L','F746L','F747L','F749L','F882L','F884L')
bmi_data <- bmi_data %>%
  subset(!(schlyr_exam == "0809" & schlgivn %in% kircaldy))                       #650,756 obs.

# Remove variable Kircaldy
rm(kircaldy)
gc()

# Exclude schlyr 02/03 from Borders data
bmi_data <- bmi_data %>%
  subset(!(HB2018 == 'B' & schlyr_exam == '0203'))                                #650,158 obs.

## Select relevant years function
apply_hb_year <- function(x, HB, ey, cy) {
  
  x <- x %>%
    filter((HB2018 == HB & as.numeric(schlyr_exam) >= ey & 
              as.numeric(schlyr_exam) <= cy) | HB2018 != HB)
  
  return(x)
  
}


# code below should work
test <- bmi_data %>%
  apply_hb_year(HB = 'F', ey = 102, cy = currentYr) %>% 
  apply_hb_year(HB = 'L', ey = 102, cy = currentYr) %>% 
  apply_hb_year(HB = 'S', ey = 102, cy = currentYr) %>% 
  apply_hb_year(HB = 'T', ey = 203, cy = currentYr) %>% 
  apply_hb_year(HB = 'W', ey = 304, cy = currentYr) %>% 
  apply_hb_year(HB = 'Y', ey = 405, cy = currentYr) %>% 
  apply_hb_year(HB = 'V', ey = 506, cy = currentYr) %>% 
  apply_hb_year(HB = 'G', ey = 607, cy = currentYr) %>% 
  apply_hb_year(HB = 'A', ey = 708, cy = currentYr) %>% 
  apply_hb_year(HB = 'H', ey = 809, cy = currentYr) %>% 
  apply_hb_year(HB = 'Z', ey = 809, cy = currentYr) %>% 
  apply_hb_year(HB = 'N', ey = 910, cy = currentYr) %>% 
  apply_hb_year(HB = 'R', ey = 1011, cy = currentYr)                              #650,158 obs.


### 5 - Child Data Sort/Analysis ----

# Create sex variable based on 9th digit of CHI

bmi_data <- mutate(bmi_data, sex = case_when(
  substr(chi,9,9) %in% c("0","2","4","6","8") ~ "F",
  substr(chi,9,9) %in% c("1","3","5","7","9") ~ "M"
))

# create 2 digit year, month and day of birth variables
bmi_data <- bmi_data %>%
  mutate(yob = as.integer(substr(chi,5,6)),
         mob = as.integer(substr(chi,3,4)),
         dob = as.integer(substr(chi,1,2)),
         # create a 4 digit year of birth variable
         nyob = case_when(yob >= 80 ~ 1900 + yob, 
                          TRUE ~ 2000 + yob),
         datedob = ymd(paste(yob,mob,dob)),
         daterev = ymd(daterec),
         # Create agemth variable to show child's age in months
         agemth = lubridate::interval(datedob,daterev) %/% months(1))

# Apply the dictionary from the Dictionary school file
dictdf <- haven::read_spss(paste(file.path(source_folder, "ReferenceFiles", "Dictionary_School.sav")))
fulldatadic <- tibble(name = colnames(dictdf),
                      label = dictdf %>%
                        purrr::map(~ attr(.x, "label")) %>%
                        unlist())
#create list of labels used in bmi_data
lookup_relevant <- semi_join(fulldatadic, bmi_data %>% colnames(.) %>% tibble::enframe(name = NULL),
                             by = c("name" = "value"))
# at the date of review/height-weight recording




# Select children aged 4 years to under 8 years.
bmi_data <- subset(bmi_data, agemth >=48 & agemth <96)                            #650,059 obs


## Save out a file at this point that can be used to produce the total
## number of reviews for HB and CA for the coverage calculations.
## This file contains all reviews (with and without valid height and weight)
## save as data frame?
#saveRDS(host_folder, "allReviews.rds")



# Convert the height and weight variables from string to numeric
bmi_data <- mutate(bmi_data, height = as.numeric(height),
                   weight = as.numeric(weight))

# Select only records with a valid (i.e. non-zero) height and weight
bmi_data <- subset(bmi_data, height != 0)
bmi_data <- subset(bmi_data, weight != 0)                                         #650,043 obs

# Create variable to show height in metres
#height in mm, weight in cg???
bmi_data <- mutate(bmi_data, height_m = height/1000)
bmi_data <- mutate(bmi_data, weight_kg = weight/100)


# Calculate BMI
bmi_data <- mutate(bmi_data, bmi = weight_kg/(height_m*height_m))

# Child's age in months will lie between two ages in 
# in the lookup table. Line below calculates the next 
# lowest whole month and converts to years.
bmi_data <- mutate(bmi_data, ageyr = round(1000*trunc(agemth)/12)/1000)




# The look-up file is matched in using the lowest whole month converted to years.
# The corresponding L,M,S are the lowest (LO) values used in the interpolation.
bmi_data <- arrange(bmi_data, ageyr)

grd <- readRDS(file.path(source_folder, "ReferenceFiles",
                         "UK1990_BMI_Growth_Reference_Data.rds"))

# Merge data
bmi_data <- bmi_data %>%
  left_join(grd, by = c("ageyr"), all.x = TRUE, all.y = TRUE) %>%
  # Rename the Growth Reference variables for the lowest whole month
  # converted to years (LO)
  dplyr::rename(LMLO_b = bmi_male_l, MMLO_b = bmi_male_m, SMLO_b = bmi_male_s,
                LFLO_b = bmi_female_l,MFLO_b = bmi_female_m, SFLO_b = bmi_female_s,
                LMLO_h = height_male_l, MMLO_h = height_male_m, SMLO_h = height_male_s,
                LFLO_h = height_female_l, MFLO_h = height_female_m, SFLO_h = height_female_s,
                LMLO_w = weight_male_l, MMLO_w = weight_male_m, SMLO_w = weight_male_s,
                LFLO_w = weight_female_l, MFLO_w = weight_female_m, SFLO_w = weight_female_s,
                agelo = ageyr)


# Child's age in months will lie between two ages in 
# in the lookup table. Line below calculates the next 
# highest whole month and converts to years.
bmi_data <- bmi_data %>%
  mutate(ageyr = round(1000*(trunc(agemth)+1)/12)/1000) %>%
  # Merge data
  left_join(grd, by = c("ageyr"), all.x = TRUE, all.y = TRUE) %>%
  # Rename the Growth Reference variables for the highest whole month
  # converted to years (HI)
  dplyr::rename(LMHI_b = bmi_male_l, MMHI_b = bmi_male_m, SMHI_b = bmi_male_s,
                LFHI_b = bmi_female_l, MFHI_b = bmi_female_m, SFHI_b = bmi_female_s,
                LMHI_h = height_male_l, MMHI_h = height_male_m, SMHI_h = height_male_s,
                LFHI_h = height_female_l, MFHI_h = height_female_m, SFHI_h = height_female_s,
                LMHI_w = weight_male_l, MMHI_w = weight_male_m, SMHI_w = weight_male_s,
                LFHI_w = weight_female_l, MFHI_w = weight_female_m, SFHI_w = weight_female_s,
                agehi = ageyr)

# Calculate age in years to 2 decimal places for BMI, Height and Weight.
bmi_data <- bmi_data %>%
  mutate(ageyrs2decimal = round((agemth/12), 2))

# Interpolation - BMI
bmi_data <- bmi_data %>%
  mutate(LINT_b = case_when(sex == "M" ~
                              (LMHI_b-((agehi-(ageyrs2decimal))/(agehi-agelo))*(LMHI_b-LMLO_b)),
                            sex == "F" ~
                              (LFHI_b-((agehi-(ageyrs2decimal))/(agehi-agelo))*(LFHI_b-LFLO_b)),
                            TRUE ~ 0),
         MINT_b= case_when(sex == "M" ~
                             (MMHI_b-((agehi-(ageyrs2decimal))/(agehi-agelo))*(MMHI_b-MMLO_b)),
                           sex == "F" ~
                             (MFHI_b-((agehi-(ageyrs2decimal))/(agehi-agelo))*(MFHI_b-MFLO_b)),
                           TRUE ~ 0),
         SINT_b= case_when(sex == "M" ~
                             (SMHI_b-((agehi-(ageyrs2decimal))/(agehi-agelo))*(SMHI_b-SMLO_b)),
                           sex == "F" ~
                             (SFHI_b-((agehi-(ageyrs2decimal))/(agehi-agelo))*(SFHI_b-SFLO_b)),
                           TRUE ~ 0))

# Interpolation - Height
bmi_data <- bmi_data %>%
  mutate(LINT_h = case_when(sex == "M" ~ 
                              (LMHI_h-((agehi-(ageyrs2decimal))/(agehi-agelo))*(LMHI_h-LMLO_h)),
                            sex == "F" ~
                              (LFHI_h-((agehi-(ageyrs2decimal))/(agehi-agelo))*(LFHI_h-LFLO_h)),
                            TRUE ~ 0),
         MINT_h= case_when(sex == "M" ~
                             (MMHI_h-((agehi-(ageyrs2decimal))/(agehi-agelo))*(MMHI_h-MMLO_h)),
                           sex == "F" ~
                             (MFHI_h-((agehi-(ageyrs2decimal))/(agehi-agelo))*(MFHI_h-MFLO_h)),
                           TRUE ~ 0),
         SINT_h= case_when(sex == "M" ~
                             (SMHI_h-((agehi-(ageyrs2decimal))/(agehi-agelo))*(SMHI_h-SMLO_h)),
                           sex == "F" ~
                             (SFHI_h-((agehi-(ageyrs2decimal))/(agehi-agelo))*(SFHI_h-SFLO_h)),
                           TRUE ~ 0))

# Interpolation - Weight
bmi_data <- bmi_data %>%
  mutate(LINT_w = case_when(sex == "M" ~
                              (LMHI_w-((agehi-(ageyrs2decimal))/(agehi-agelo))*(LMHI_w-LMLO_w)),
                            sex == "F" ~
                              (LFHI_w-((agehi-(ageyrs2decimal))/(agehi-agelo))*(LFHI_w-LFLO_w)),
                            TRUE ~ 0),
         MINT_w= case_when(sex == "M" ~
                             (MMHI_w-((agehi-(ageyrs2decimal))/(agehi-agelo))*(MMHI_w-MMLO_w)),
                           sex == "F" ~
                             (MFHI_w-((agehi-(ageyrs2decimal))/(agehi-agelo))*(MFHI_w-MFLO_w)),
                           TRUE ~ 0),
         SINT_w= case_when(sex == "M" ~
                             (SMHI_w-((agehi-(ageyrs2decimal))/(agehi-agelo))*(SMHI_w-SMLO_w)),
                           sex == "F" ~
                             (SFHI_w-((agehi-(ageyrs2decimal))/(agehi-agelo))*(SFHI_w-SFLO_w)),
                           TRUE ~ 0))


# Calculate standard deviation scores
# pnorm is the R function for Cumulative Distribution Function (CDF)
bmi_data <- bmi_data %>%
  mutate((bmi_2nd = round(bmi, 2)),
         # Calculate sd (standardised) score using BMI to two decimal places.
         sds2_b = (((bmi_2nd/MINT_b)**LINT_b)-1)/(LINT_b*SINT_b),
         #compute sds to 2 decimal places.
         sds_b=round(sds2_b,2),
         # Calculate centiles. 
         cent_b=100 * pnorm(sds_b, mean = 0, sd = 1),
         height_2nd = round((height/10), 2),
         # Calculate sd (standardised) score using height to two decimal places.
         sds2_h = (((height_2nd/MINT_h)**LINT_h)-1)/(LINT_h*SINT_h),
         #compute sds to 2 decimal places.
         sds_h=round(sds2_h,2),
         # Calculate centiles.
         cent_h=100 * pnorm(sds_h, mean = 0, sd = 1),
         weight_2nd = round(weight_kg, 2),
         # Calculate sd (standardised) score using weight to two decimal places.
         sds2_w = (((weight_2nd/MINT_w)**LINT_w)-1)/(LINT_w*SINT_w),
         #compute sds to 2 decimal places.
         sds_w=round(sds2_w,2),
         # Calculate centiles.
         cent_w=100 * pnorm(sds_w, mean = 0, sd = 1))

saveRDS(bmi_data, paste0(host_folder, "RAPtemp_all_reviews.rds"))
bmi_data <- readRDS(paste0(host_folder, "RAPtemp_all_reviews.rds"))
# select out those outwith the range deemed to be real.
bmi_data <- subset(bmi_data, (sds_b >= -7 & sds_b <= 7) & 
                     (sds_h >= -7 & sds_h <= 7) & (sds_w >= -7 & sds_w <= 7))     #649,529 obs


# Create epidemiological and clinical thresholds
# epidemiological
bmi_data <- bmi_data %>%
  mutate(cent_grp1 = ifelse(cent_b <= 2, 1, 0),
         cent_grp2 = ifelse((cent_b > 2) & (cent_b < 85), 1, 0),
         cent_grp3 = ifelse((cent_b >= 85) & (cent_b < 95), 1, 0),
         cent_grp4 = ifelse(cent_b >= 95, 1, 0),
         cent_grp5 = ifelse(cent_b >= 85, 1, 0))


# clinical
bmi_data <- bmi_data %>%
  mutate(clin_cent_grp1 = ifelse(sds_b <= -2.67, 1, 0),
         clin_cent_grp2 = ifelse((sds_b > -2.67) & (sds_b < 1.33), 1, 0),
         clin_cent_grp3 = ifelse((sds_b >=1.33) & (sds_b < 2), 1, 0),
         clin_cent_grp4 = ifelse((sds_b >= 2) & (sds_b < 2.67), 1, 0),
         clin_cent_grp5 = ifelse(sds_b >= 2.67, 1, 0),
         clin_cent_grp6 = ifelse(sds_b >= 1.33, 1, 0),
         clin_cent_grp7 = ifelse(sds_b >= 2, 1, 0),
         tot = 1)

#this can't be piped??
arrange(bmi_data, pc7)

#saveRDS(host_folder, "BMI_data_0102_1718.rds")

# read in deprivation lookup. 
simd_2016 <- readRDS(paste0(
  lookupFolder, "/Unicode/Deprivation/postcode_2019_2_simd2016.rds")) %>%
  select(pc7, simd2016_sc_quintile)

simd_2012 <- readRDS(paste0(
  lookupFolder, "/Unicode/Deprivation/postcode_2016_1_simd2012.rds")) %>%
  select(pc7, simd2012_sc_quintile)

simd_2009 <- readRDS(paste0(
  lookupFolder, "/Unicode/Deprivation/", "postcode_2012_2_simd2009v2.rds")) %>%
  select(pc7, simd2009v2_sc_quintile)

simd_2006 <- readRDS(paste0(
  lookupFolder, "/Unicode/Deprivation/", "postcode_2009_2_simd2006.rds")) %>%
  select(pc7, simd2006_sc_quintile)

simd_2004 <- readRDS(paste0(
  lookupFolder, "/Unicode/Deprivation/", "postcode_2006_2_simd2004.rds")) %>%
  select(pc7, simd2004_sc_quintile)

# Recode simd 2004 & 2006 so all go from 1=most to 5=least - this was
# required for creating allsimdyear/allsimdfinyear
# Still to change labels
rescale <- function(x_i){
  6-x_i
}

all_simd <- list(simd_2004, simd_2006, simd_2009, simd_2012, simd_2016) %>%
  Reduce(function(dtf1,dtf2) left_join(dtf1,dtf2,by="pc7"), .)

#to fix - not being applied???
all_simd <- all_simd %>%
  mutate(simd2004_sc_quintile = sapply(simd2004_sc_quintile, rescale),
         simd2006_sc_quintile = sapply(simd2006_sc_quintile, rescale))

#Assign the appropriate SIMD value to a record depending on the year
bmi_data <- bmi_data %>%
  merge(all_simd, by = "pc7") %>%
  mutate(simd = case_when(
    schlyr_exam %in% c("1415", "1516", "1617", "1718", "1819") 
    ~ simd2016_sc_quintile,
    schlyr_exam %in% c("1011", "1112", "1213", "1314") 
    ~ simd2012_sc_quintile,
    schlyr_exam %in% c("0708", "0809", "0910") 
    ~ simd2009v2_sc_quintile,
    schlyr_exam %in% c("0405", "0506", "0607") 
    ~ simd2006_sc_quintile,
    schlyr_exam %in% c("0102", "0203", "0304") 
    ~ simd2004_sc_quintile
  ))

# No longer need the old ca code. Was only used to generate table key. 
# use current ca code.
# Councils - create the old CA with codes 01-32 ; the new codes are 
# made unique by the last 2 chars so use them   .
# mutate(bmi_data, ca = substr(CA2019, 8, 9))
# bmi_data <- bmi_data %>%       
#   for (ca in c("33", "34", "41", "35", "26", "05", "39", "06", "42", "08", 
#                       "45", "10", "11", "36", "14", "47", "46", "17", "18", "19", 
#                       "20", "21", "44", "23", "48", "38", "27", "28", "29", "30", 
#                       "40", "13")) {
#     ca <-  c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", 
#                       "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", 
#                       "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", 
#                       "31", "32")}

#mutate(council_area_name <- sapply(council_area_name, rescale)), 
bmi_data <- mutate(bmi_data, ca_name = case_when(
  substr(CA2019, 8, 9) == "33" ~ "Aberdeen City",
  substr(CA2019, 8, 9) == "34" ~ "Aberdeenshire",
  substr(CA2019, 8, 9) == "41" ~ "Angus",
  substr(CA2019, 8, 9) == "35" ~ "Argyll & Bute",
  substr(CA2019, 8, 9) == "26" ~ "Scottish Borders",
  substr(CA2019, 8, 9) == "05" ~ "Clackmannanshire",
  substr(CA2019, 8, 9) == "39" ~ "West Dunbartonshire",
  substr(CA2019, 8, 9) == "06" ~ "Dumfries & Galloway",
  substr(CA2019, 8, 9) == "42" ~ "Dundee City",
  substr(CA2019, 8, 9) == "08" ~ "East Ayrshire",
  substr(CA2019, 8, 9) == "45" ~ "East Dunbartonshire",
  substr(CA2019, 8, 9) == "10" ~ "East Lothian",
  substr(CA2019, 8, 9) == "11" ~ "East Renfrewshire",
  substr(CA2019, 8, 9) == "36" ~ "City of Edinburgh",
  substr(CA2019, 8, 9) == "14" ~ "Falkirk",
  substr(CA2019, 8, 9) == "46" ~ "Fife",
  substr(CA2019, 8, 9) == "47" ~ "Glasgow City",
  substr(CA2019, 8, 9) == "17" ~ "Highland",
  substr(CA2019, 8, 9) == "18" ~ "Inverclyde",
  substr(CA2019, 8, 9) == "19" ~ "Midlothian",
  substr(CA2019, 8, 9) == "20" ~ "Moray",
  substr(CA2019, 8, 9) == "21" ~ "North Ayrshire",
  substr(CA2019, 8, 9) == "44" ~ "North Lanarkshire",
  substr(CA2019, 8, 9) == "43" ~ "Orkney Islands",
  substr(CA2019, 8, 9) == "48" ~ "Perth & Kinross",
  substr(CA2019, 8, 9) == "38" ~ "Renfrewshire",
  substr(CA2019, 8, 9) == "27" ~ "Shetland Islands",
  substr(CA2019, 8, 9) == "28" ~ "South Ayrshire",
  substr(CA2019, 8, 9) == "29" ~ "South Lanarkshire",
  substr(CA2019, 8, 9) == "30" ~ "Stirling",
  substr(CA2019, 8, 9) == "31" ~ "West Lothian",
  substr(CA2019, 8, 9) == "32" ~ "Comhairle nan Eilean Siar",
  TRUE ~ "Other"))

bmi_data <- mutate(bmi_data, carea = paste(substr(CA2019, 8, 9),ca_name))


arrange(bmi_data, chi)


bmi_basefile <- bmi_data %>%
  subset(select = c(chi, id, HB2019, CA2019, carea, sex, height, weight, daterec, 
                    schlyr_exam, schlgivn, rev_num, agemth, nyob, mob, dob, pc7,
                    simd2016_sc_quintile, simd2012_sc_quintile, 
                    simd2009v2_sc_quintile, simd2006_sc_quintile, 
                    simd2004_sc_quintile, simd, sex, height_m, weight_kg, bmi, 
                    tot, cent_grp1, cent_grp2, cent_grp3, cent_grp4, cent_grp5, 
                    clin_cent_grp1, clin_cent_grp2, clin_cent_grp3, clin_cent_grp4, 
                    clin_cent_grp5, clin_cent_grp6, clin_cent_grp7))

# This file contains data for school years 2001/02 to 2017/18 and should 
# be used for information requests etc. so that any figures produced match 
# those published in financial year 2017/18.
saveRDS(bmi_basefile, paste0(host_folder, "BMI_data_0102_1718.rds"))              #614,752 obs.

bmi_basefile <- readRDS(paste0(host_folder, "BMI_data_0102_1718.rds"))


### Health board analysis

# create population file from the GRO mid year population estimates
# of five year olds in each HB and for all participating boards
hb_pop_estimates <- readRDS(paste0(
  lookupFolder, "/Unicode/Populations/Estimates/HB2019_pop_est_1981_2018.rds")) %>%
  rename(year = Year, age = Age, pop = Pop)

hb_pop_estimates <- hb_pop_estimates %>% 
  filter(age == 5) %>%
  filter(year >= 2000 & year <=2018) %>% 
  mutate(schlyr_exam = case_when(year == 2001 ~ "0102", 
                                 year == 2002 ~ "0203",
                                 year == 2003 ~ "0304",
                                 year == 2004 ~ "0405",
                                 year == 2005 ~ "0506",
                                 year == 2006 ~ "0607",
                                 year == 2007 ~ "0708",
                                 year == 2008 ~ "0809",
                                 year == 2009 ~ "0910",
                                 year == 2010 ~ "1011",
                                 year == 2011 ~ "1112",
                                 year == 2012 ~ "1213",
                                 year == 2013 ~ "1314",
                                 year == 2014 ~ "1415",
                                 year == 2015 ~ "1516",
                                 year == 2016 ~ "1617",
                                 year == 2017 ~ "1718",
                                 year == 2018 ~ "1819")) #%>% 
# call the function for creating HB cypher
# apply_hb_cypher(hb_pop_estimates) # %>%
##### commented out until we have the functions script working #####
# call the function for selecing the relevant year for each board
# apply_hb_year(hb_pop_estimates, HB = 'F', ey = 102)
# apply_hb_year(hb_pop_estimates, HB = 'L', ey = 102)
# apply_hb_year(hb_pop_estimates, HB = 'S', ey = 102)
# apply_hb_year(hb_pop_estimates, HB = 'T', ey = 203)
# apply_hb_year(hb_pop_estimates, HB = 'W', ey = 304)
# apply_hb_year(hb_pop_estimates, HB = 'Y', ey = 405)
# apply_hb_year(hb_pop_estimates, HB = 'V', ey = 506)
# apply_hb_year(hb_pop_estimates, HB = 'G', ey = 607)
# apply_hb_year(hb_pop_estimates, HB = 'A', ey = 708)
# apply_hb_year(hb_pop_estimates, HB = 'H', ey = 809)
# apply_hb_year(hb_pop_estimates, HB = 'Z', ey = 809)
# apply_hb_year(hb_pop_estimates, HB = 'N', ey = 910)
# apply_hb_year(hb_pop_estimates, HB = 'R', ey = 1011)


# create totals for individual hb and all 
# participating boards (for hb_pop_estimates)
# Board level
hb_pop_estimates <- rbind(hb_pop_estimates %>% 
                            group_by(HB2019, schlyr_exam)%>%
                            summarise(pop = sum(pop)) %>% ungroup,
                          # Scotland level (all participating boards)
                          hb_pop_estimates %>% group_by(schlyr_exam) %>%
                            summarise(pop = sum(pop)) %>%
                            mutate(HB2019 = "Total") %>% ungroup)


# create totals for individual hb and all participating boards (for hb_data)
# Board level
hb_data <- rbind(bmi_basefile %>% group_by(HB2019, schlyr_exam) %>%
                   summarise_at(vars(tot:clin_cent_grp7), sum)  %>% ungroup,
                 # Scotland level (all participating boards)
                 bmi_basefile %>% group_by(schlyr_exam) %>% 
                   summarise_at(vars(tot:clin_cent_grp7), sum) %>%
                   mutate(HB2019 = "Total") %>% ungroup)

# JV helped to fix the above rbind, script below still useful to test
# by HB and all participating boards separately
#test <- bmi_basefile %>% group_by(HB2019, schlyr_exam) %>%
#  summarise_at(vars(tot:clin_cent_grp7), sum)  %>% ungroup
#test1 <- bmi_basefile %>% group_by(schlyr_exam) %>% 
#  summarise_at(vars(tot:clin_cent_grp7), sum) %>%
#  mutate(HB2019 = "Total") %>% ungroup

#test2 <- rbind(test, test1)

# Match hb data to hb population estimates
hb_data <- left_join(hb_data, hb_pop_estimates, 
                     by = c("HB2019", "schlyr_exam"))


# Confidence intervals (hb)
# use the function to calculate confidence intervals
calculate_ci(hb_data)




### Council area analysis

# create population file from the GRO mid year population estimates
# of five year olds in each ca 
ca_pop_estimates <- readRDS(paste0(
  lookupFolder, "/Unicode/Populations/Estimates/CA2019_pop_est_1981_2018.rds")) %>%
  rename(year = Year, age = Age, pop = Pop)

ca_pop_estimates <- ca_pop_estimates %>% 
  filter(age == 5) %>%
  filter(year >= 2000 & year <=2018) %>% 
  mutate(schlyr_exam = case_when(year == 2001 ~ "0102", 
                                 year == 2002 ~ "0203",
                                 year == 2003 ~ "0304",
                                 year == 2004 ~ "0405",
                                 year == 2005 ~ "0506",
                                 year == 2006 ~ "0607",
                                 year == 2007 ~ "0708",
                                 year == 2008 ~ "0809",
                                 year == 2009 ~ "0910",
                                 year == 2010 ~ "1011",
                                 year == 2011 ~ "1112",
                                 year == 2012 ~ "1213",
                                 year == 2013 ~ "1314",
                                 year == 2014 ~ "1415",
                                 year == 2015 ~ "1516",
                                 year == 2016 ~ "1617",
                                 year == 2017 ~ "1718",
                                 year == 2018 ~ "1819")) 

# create totals for individual council areas (for ca_pop_estimates)
# council area level
ca_pop_estimates <- rbind(ca_pop_estimates %>% 
                            group_by(CA2019, schlyr_exam)%>%
                            summarise(pop = sum(pop)) %>% ungroup)


# create totals for individual hb and all participating boards (for ca_data)
# council area level
ca_data <- rbind(bmi_basefile %>% group_by(CA2019, schlyr_exam) %>%
                   summarise_at(vars(tot:clin_cent_grp7), sum)  %>% ungroup)


# Select council areas with a valid population. Some council areas 
# may have small numbers for years that they did not participate.
ca_data <- subset(ca_data, tot >50)

# Match ca data to ca population estimates
ca_data <- left_join(ca_data, ca_pop_estimates, 
                     by = c("CA2019", "schlyr_exam"))


# Confidence intervals (ca)
# use the function to calculate confidence intervals
calculate_ci(ca_data)




### Gender analysis

# create population file from the GRO mid year population estimates
# of five year olds by gender.
gender_pop_estimates <- readRDS(paste0(
  lookupFolder, "/Unicode/Populations/Estimates/HB2019_pop_est_1981_2018.rds")) %>%
  rename(year = Year, age = Age, pop = Pop, sex = Sex)

gender_pop_estimates <- gender_pop_estimates %>% 
  filter(age == 5) %>%
  filter(year >= 2000 & year <=2018) %>% 
  mutate(schlyr_exam = case_when(year == 2001 ~ "0102", 
                                 year == 2002 ~ "0203",
                                 year == 2003 ~ "0304",
                                 year == 2004 ~ "0405",
                                 year == 2005 ~ "0506",
                                 year == 2006 ~ "0607",
                                 year == 2007 ~ "0708",
                                 year == 2008 ~ "0809",
                                 year == 2009 ~ "0910",
                                 year == 2010 ~ "1011",
                                 year == 2011 ~ "1112",
                                 year == 2012 ~ "1213",
                                 year == 2013 ~ "1314",
                                 year == 2014 ~ "1415",
                                 year == 2015 ~ "1516",
                                 year == 2016 ~ "1617",
                                 year == 2017 ~ "1718",
                                 year == 2018 ~ "1819")) # %>% 
# call the function for creating HB cypher
# apply_hb_cypher(gender_pop_estimates) %>%
# call the function for selecing the relevant year for each board
# apply_hb_year(gender_pop_estimates, HB = 'F', ey = 102)
# apply_hb_year(gender_pop_estimates, HB = 'L', ey = 102)
# apply_hb_year(gender_pop_estimates, HB = 'S', ey = 102)
# apply_hb_year(gender_pop_estimates, HB = 'T', ey = 203)
# apply_hb_year(gender_pop_estimates, HB = 'W', ey = 304)
# apply_hb_year(gender_pop_estimates, HB = 'Y', ey = 405)
# apply_hb_year(gender_pop_estimates, HB = 'V', ey = 506)
# apply_hb_year(gender_pop_estimates, HB = 'G', ey = 607)
# apply_hb_year(gender_pop_estimates, HB = 'A', ey = 708)
# apply_hb_year(gender_pop_estimates, HB = 'H', ey = 809)
# apply_hb_year(gender_pop_estimates, HB = 'Z', ey = 809)
# apply_hb_year(gender_pop_estimates, HB = 'N', ey = 910)
# apply_hb_year(gender_pop_estimates, HB = 'R', ey = 1011)


# create totals for male and female (for gender_pop_estimates)
# all participating boards
gender_pop_estimates <- rbind(gender_pop_estimates %>% 
                                group_by(sex, schlyr_exam)%>%
                                summarise(pop = sum(pop)) %>% ungroup,
                              # totals by year (all participating boards)
                              gender_pop_estimates %>% group_by(schlyr_exam) %>%
                                summarise(pop = sum(pop)) %>%
                                mutate(sex = "Total") %>% ungroup)


# create totals for male and female (for gender_data)
# all participating boards
gender_data <- rbind(bmi_basefile %>% group_by(sex, schlyr_exam) %>%
                       summarise_at(vars(tot:clin_cent_grp7), sum)  %>% ungroup,
                     # totals by year (all participating boards)
                     bmi_basefile %>% group_by(schlyr_exam) %>% 
                       summarise_at(vars(tot:clin_cent_grp7), sum) %>%
                       mutate(sex = "Total") %>% ungroup)


# Match gender data to gender population estimates
gender_data <- left_join(gender_data, gender_pop_estimates, 
                         by = c("sex", "schlyr_exam"))


# Confidence intervals (gender)
# use the function to calculate confidence intervals
calculate_ci(gender_data)




### simd analysis

# create population file from the GRO mid year population estimates
# of five year olds by simd (for years 01/02 to 13/14)
simd_pop_estimates_1 <- readRDS(paste0(
  lookupFolder, "/Unicode/Populations/Estimates/DataZone2001_pop_est_2001_2014.rds")) %>%
  select(Year, DataZone2001, SEX, AGE5, simd2004_sc_quintile, 
         simd2006_sc_quintile, simd2009v2_sc_quintile, simd2012_sc_quintile)

simd_pop_estimates_1 <- simd_pop_estimates_1 %>%
  rename(year = Year, pop = AGE5, sex = SEX) %>%
  filter(year >= 2001 & year <=2013) %>% 
  mutate(schlyr_exam = case_when(year == 2001 ~ "0102", 
                                 year == 2002 ~ "0203",
                                 year == 2003 ~ "0304",
                                 year == 2004 ~ "0405",
                                 year == 2005 ~ "0506",
                                 year == 2006 ~ "0607",
                                 year == 2007 ~ "0708",
                                 year == 2008 ~ "0809",
                                 year == 2009 ~ "0910",
                                 year == 2010 ~ "1011",
                                 year == 2011 ~ "1112",
                                 year == 2012 ~ "1213",
                                 year == 2013 ~ "1314"))

# Recode simd 2004 & 2006 so all go from 1=most to 5=least



# assign the appropriate SIMD value to a record depending on the year
simd_pop_estimates_1 <- simd_pop_estimates_1 %>%
  mutate(simd = case_when(
    schlyr_exam %in% c("1011", "1112", "1213", "1314") 
    ~ simd2012_sc_quintile,
    schlyr_exam %in% c("0708", "0809", "0910") 
    ~ simd2009v2_sc_quintile,
    schlyr_exam %in% c("0405", "0506", "0607") 
    ~ simd2006_sc_quintile,
    schlyr_exam %in% c("0102", "0203", "0304") 
    ~ simd2004_sc_quintile
  ))

# create totals for simd_population_estimates_1
# all participating boards
simd_pop_estimates_1 <- rbind(simd_pop_estimates_1 %>% 
                                group_by(simd, schlyr_exam)%>%
                                summarise(pop = sum(pop)) %>% ungroup)


# create population file from the GRO mid year population estimates
# of five year olds by simd (for years 14/15 to current year)
simd_pop_estimates_2 <- readRDS(paste0(
  lookupFolder, "/Unicode/Populations/Estimates/DataZone2011_pop_est_2011_2018.rds")) %>%
  select(year, datazone2011, sex, age5, simd2016_sc_quintile)

simd_pop_estimates_2 <- simd_pop_estimates_2 %>%
  rename(pop = age5) %>%
  filter(year >= 2014 & year <=2018) %>% 
  mutate(schlyr_exam = case_when(year == 2014 ~ "1415",
                                 year == 2015 ~ "1516",
                                 year == 2016 ~ "1617",
                                 year == 2017 ~ "1718",
                                 year == 2018 ~ "1819"))

# assign the appropriate SIMD value to a record depending on the year
simd_pop_estimates_2 <- simd_pop_estimates_2 %>%
  mutate(simd = case_when(
    schlyr_exam %in% c("1415", "1516", "1617", "1718", "1819") 
    ~ simd2016_sc_quintile
  ))

# create totals for simd_population_estimates_2
# all participating boards
simd_pop_estimates_2 <- rbind(simd_pop_estimates_2 %>% 
                                group_by(simd, schlyr_exam)%>%
                                summarise(pop = sum(pop)) %>% ungroup)


# Match the two simd populations files together
simd_pop_estimates <- left_join(simd_pop_estimates_1, simd_pop_estimates_2,
                                by = c("simd" , "schlyr_exam"))



# create totals for simd (simd_data)
# all participating boards
simd_data <- rbind(bmi_basefile %>% group_by(simd, schlyr_exam) %>%
                     summarise_at(vars(tot:clin_cent_grp7), sum)  %>% ungroup)

# add simd_data to simd_population_estimates
simd_data <- left_join(simd_data, simd_pop_estimates, 
                       by = c("simd", "schlyr_exam"))


# Confidence intervals (simd)
# use the function to calculate confidence intervals
calculate_ci(simd_data)







### END OF SCRIPT ### ----