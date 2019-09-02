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
  hostFolder <- "//PHI_conf/ChildHealthSurveillance/Portfolio/Data"
  lookupFolder <- "/conf/linkage/output/lookups"
} else if (server_desktop == "desktop") {
  hostFolder <- "//stats/ChildHealthSurveillance/Portfolio/Data"
  lookupFolder <-"//Isdsf00d03/cl-out/lookups"
}

## Variables
# Current School/Financial Year (e.g. 1819)
currentYr <- 1718


### 2 - Data Import / Inital Sort ----

## Import Data
# Created R file for efficiency (SPSS syntax below)
# bmiData <- read_spss(paste(file.path(hostFolder, "school_results.zsav")))
bmiData <- readRDS(paste(file.path(hostFolder, "20181008", "School", "school_results.rds")))            #1.614m obs.

## Subset for required variables only
bmiData <- subset(bmiData, select = c(chi, schlyr_exam, id, rev_num, schlgivn, height, weight, bmi,
                                      centile_h, centile_w, centile_bmi, date_hw, schlyr_hw,
                                      date_exam, hb_exam, pc_review))

## Sort
# Restrict to relevant years (var: schlyr_exam)
bmiData <- bmiData %>%
  filter(as.integer(schlyr_exam) >= 0102 & as.integer(schlyr_exam) <= 1718)       #1.541m obs.

# Restrict to review number 60/61 (var: rev_num)
bmiData <- bmiData %>%
  filter(rev_num == "60" | rev_num == "61")                                       #688520 obs.

# Define single date variable
bmiData$daterec <- if_else(bmiData$date_hw != "00000000", bmiData$date_hw, bmiData$date_exam)

# Select only most recent record per CHI and review (var: rev_num)
bmiData <- bmiData %>%
  group_by(chi, rev_num) %>%
  slice(which.max(as.Date(daterec, '%Y%m%d')))                                    #667964 obs.

# Extract blank height/weight valued records
blankData <- bmiData %>%
  filter(is.na(height) | height == "0000" | is.na(weight) | weight == "0000")     #(12048 obs.)

# Remove blank height/weight
bmiData <- bmiData %>%
  filter(!is.na(height) & height != "0000" & !is.na(weight) & weight != "0000")   #655916 obs.

# Select single record per CHI (only using review 61 where there's no review 60 - var:rev_num)
bmiData <- bmiData %>%
  group_by(chi) %>%
  slice(which.min(as.numeric(rev_num)))                                           #653587 obs.


### 3 - Match Postcodes ----
# School year 2010/11 is first full cohort to have postcode recorded (chiQ).
# For years 2001/02 to 2009/10, postcode is matched using lookup/reference file.

## Postcode match for 2001/02 to 2009/10
# Import postcode reference file
pcRef <- readRDS(file.path(hostFolder, "ReferenceFiles", "postcode_at_review_2000_2010_school.rds"))

# Merge bmiData with the postcode reference file
bmiData <- merge(bmiData, pcRef[,c("chi", "postcode", "schlyr_exam")],
                 by = c("chi", "schlyr_exam"), all.x = TRUE, all.y = FALSE)

# Create one postcode variable using pc_review & the postcode reference
bmiData$pc_review <- if_else(bmiData$pc_review == "", bmiData$postcode, bmiData$pc_review)


# Remove redundant postcode variable
bmiData <- subset(bmiData, select = -c(postcode))

# Remove postcode reference file pcRef
rm(pcRef)
gc()


## Postcode match for 2010/11 on (chiQ)
# Create variable to show number of quarters from Jan 1990 (J-M = 1)
bmiData$examQN <- floor((difftime(as.Date(bmiData$daterec, '%Y%m%d'), as.Date("19900101", '%Y%m%d'),
                                  units = "days")/365)*4+1)

# New variable to concatenate chi and examQN
bmiData$chiQ <- paste(bmiData$chi, bmiData$examQN, sep = "")

# Import reference file
pcQRef <- haven::read_spss(file.path(hostFolder, "Cohorts", "chiQ.zsav"))

# Merge bmiData with postcode reference file
bmiData <- merge(bmiData, pcQRef[,c("chiQ", "pcodeCHI")], by = c("chiQ"), all.x = TRUE, all.y = FALSE)

# Remove postcode reference file pcQRef
rm(pcQRef)
gc()

# Create one postcode variable (pc7)
bmiData$pc7 <- if_else(bmiData$pc_review != "", bmiData$pc_review, bmiData$pcodeCHI)

# Remove unnecessary variables
bmiData <- subset(bmiData, select = -c(chiQ, examQN, pcodeCHI))

## Add CA, HSCP, etc. from lookup
# Import Reference File
pcd <- read_rds(file.path(lookupFolder, "Unicode/Geography/Scottish Postcode Directory", 
                                     "Scottish_Postcode_Directory_2019_1.5.rds"))

## Merge data
bmiData <- merge(bmiData, pcd, by = c("pc7"), all.x = TRUE, all.y = FALSE)

# Remove reference file
rm(pcd)
gc()


### 4 - Health Board Data Sort ----

## Recode HB variable to single character cypher
bmiData$HB2018 <- bmiData$HB2018 %>%
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
bmiData <- bmiData %>%
  subset(!(HB2018 == 'S' & CA2018 == 'S12000040' & (schlyr_exam == "0607" | schlyr_exam == "0708")))

# Exclude Kircaldy schools during 2008/09
kircaldy <- c('F735L','F736L','F737L','F738L','F739L','F740L','F741L',
              'F743L','F744L','F745L','F746L','F747L','F749L','F882L','F884L')
bmiData <- bmiData %>%
  subset(!(schlyr_exam == "0809" & schlgivn %in% kircaldy))

# Remove variable Kircaldy
rm(kircaldy)
gc()

# Exclude schlyr 02/03 from Borders data
bmiData <- bmiData %>%
  subset(!(HB2018 == 'B' & schlyr_exam == '0203'))

## Select relevant years function
hbYearFilter <- function(x = bmiData, HB, ey, cy = currentYr) {
  x <- x %>%
    subset(!(x$HB2018 == HB &
               as.numeric(schlyr_exam) <= ey &
               as.numeric(schlyr_exam) >= cy))
}

bmiDataT <- bmiData %>%
  filter(!(bmiData$HB2018 == 'T' & as.numeric(schlyr_exam) < 203))


hbYearFilter(HB = 'T', ey = 203)

## added by MN
# SQ commented that in order to run the code below I would need to
# write a separate function defining HB as a vector
# hbYearFilter(HB = 'F','L','S', ey = 102)
# hbYearFilter(HB = 'W', ey = 304)
# hbYearFilter(HB = 'Y', ey = 405)
# hbYearFilter(HB = 'V', ey = 506)
# hbYearFilter(HB = 'G', ey = 607)
# hbYearFilter(HB = 'A', ey = 708)
# hbYearFilter(HB = 'H', 'Z', ey = 809)
# hbYearFilter(HB = 'N', ey = 910)
# hbYearFilter(HB = 'R', ey = 1011)


# code below should work
hbYearFilter(HB = 'F', ey = 102)
hbYearFilter(HB = 'L', ey = 102)
hbYearFilter(HB = 'S', ey = 102)
hbYearFilter(HB = 'W', ey = 304)
hbYearFilter(HB = 'Y', ey = 405)
hbYearFilter(HB = 'V', ey = 506)
hbYearFilter(HB = 'G', ey = 607)
hbYearFilter(HB = 'A', ey = 708)
hbYearFilter(HB = 'H', ey = 809)
hbYearFilter(HB = 'Z', ey = 809)
hbYearFilter(HB = 'N', ey = 910)
hbYearFilter(HB = 'R', ey = 1011)


### 5 - Child Data Sort/Analysis ----

# Create sex variable based on 9th digit of CHI

bmiData <- mutate(bmiData, sex = case_when(
  substr(chi,9,9) %in% c("0","2","4","6","8") ~ "F",
  substr(chi,9,9) %in% c("1","3","5","7","9") ~ "M"
))

# create 2 digit year, month and day of birth variables
bmiData <- bmiData %>%
  mutate(yob = as.integer(substr(chi,5,6)),
         mob = as.integer(substr(chi,3,4)),
         dob = as.integer(substr(chi,1,2)),
        # create a 4 digit year of birth variable
        nyob = (if (yob >= 80) 1900 + yob else 2000 + yob),
        datedob = ymd(paste(yob,mob,dob)),
        daterev = ymd(date_hw),
        # Create agemth variable to show child's age in months
        agemth = lubridate::interval(datedob,daterev) %/% months(1))

# Apply the dictionary from the Dictionary school file

# at the date of review/height-weight recording




# Select children aged 4 years to under 8 years.
bmiData <- subset(bmiData, agemth >=48 | agemth <96) #604,209 obs


## Save out a file at this point that can be used to produce the total
## number of reviews for HB and CA for the coverage calculations.
## This file contains all reviews (with and without valid height and weight)
## save as data frame?
saveRDS(hostFolder, "allReviews.rds")



# Convert the height and weight variables from string to numeric
bmiData <- mutate(bmiData, height = as.numeric(height),
                  weight = as.numeric(weight))

# Select only records with a valid (i.e. non-zero) height and weight
bmiData <- subset(bmiData, height != 0)
bmiData <- subset(bmiData, weight != 0) #603,685 obs


# Create variable to show height in metres
#height in mm???
bmiData <- mutate(bmiData, height_m = height/100)


# Calculate BMI
bmiData <- mutate(bmiData, bmi = weight/(height_m*height_m))

# Child's age in months will lie between two ages in 
# in the lookup table. Line below calculates the next 
# lowest whole month and converts to years.
bmiData <- mutate(bmiData, ageyr = round(1000*trunc(agemth)/12)/1000)




# The look-up file is matched in using the lowest whole month converted to years.
# The corresponding L,M,S are the lowest (LO) values used in the interpolation.
bmiData <- arrange(bmiData, ageyr)

grd <- readRDS(file.path(hostFolder, "ReferenceFiles",
                         "UK1990_BMI_Growth_Reference_Data.rds"))

# Merge data
bmiData <- merge(bmiData, grd, by = c("ageyr"), 
                 all.x = TRUE, all.y = TRUE)

# Rename the Growth Reference variables for the lowest whole month
# converted to years (LO)
dplyr::rename(bmi_male_l=LMLO_b, bmi_male_m=MMLO_b, bmi_male_s=SMLO_b, 
              bmi_female_l=LFLO_b,bmi_female_m=MFLO_b, bmi_female_s=SFLO_b,
              height_male_l=LMLO_h, height_male_m=MMLO_h, height_male_s=SMLO_h,
              height_female_l=LFLO_h, height_female_m=MFLO_h, 
              height_female_s=SFLO_h, weight_male_l=LMLO_w, 
              weight_male_m=MMLO_w, weight_male_s=SMLO_w, 
              weight_female_l=LFLO_w, weight_female_m=MFLO_w, 
              weight_female_s=SFLO_w, ageyr_agelo)


# Child's age in months will lie between two ages in 
# in the lookup table. Line below calculates the next 
# highest whole month and converts to years.
bmiData <- mutate(bmiData, ageyr = round(1000*trunc(agemth)+1/12)/1000)

# Merge data
bmiData <- merge(bmiData, grd, by = c("ageyr"), 
                 all.x = TRUE, all.y = TRUE)
  
# Rename the Growth Reference variables for the highest whole month
# converted to years (HI)
dplyr::rename(bmi_male_l=LMHI_b, bmi_male_m=MMHI_b, bmi_male_s=SMHI_b, 
              bmi_female_l=LFHI_b,bmi_female_m=MFHI_b, bmi_female_s=SFHI_b,
              height_male_l=LMHI_h, height_male_m=MMHI_h, height_male_s=SMHI_h,
              height_female_l=LFHI_h, height_female_m=MFHI_h, 
              height_female_s=SFHI_h, weight_male_l=LMHI_w, 
              weight_male_m=MMHI_w, weight_male_s=SMHI_w, 
              weight_female_l=LFHI_w, weight_female_m=MFHI_w, 
              weight_female_s=SFHI_w, ageyr_agehi)

# Calculate age in years to 2 decimal places for BMI, Height and Weight.
bmiData <- bmiData %>%
  mutate(ageyrs2decimal = round((agemth/12), 2))


# Interpolation - BMI
bmiData <- bmiData %>%
  if_else(sex == M, mutate(
    LINT_b=(LMHI_b-((AGEHI-(ageyrs2decimal))/(AGEHI-AGELO))*(LMHI_b-LMLO_b)),
    MINT_b=(MMHI_b-((AGEHI-(ageyrs2decimal))/(AGEHI-AGELO))*(MMHI_b-MMLO_b)),
    SINT_b=(SMHI_b-((AGEHI-(ageyrs2decimal))/(AGEHI-AGELO))*(SMHI_b-SMLO_b))),
    mutate(
      LINT_b=(LFHI_b-((AGEHI-(ageyrs2decimal))/(AGEHI-AGELO))*(LFHI_b-LFLO_b)),
      MINT_b=(MFHI_b-((AGEHI-(ageyrs2decimal))/(AGEHI-AGELO))*(MFHI_b-MFLO_b)),
      SINT_b=(SFHI_b-((AGEHI-(ageyrs2decimal))/(AGEHI-AGELO))*(SFHI_b-SFLO_b))))


# Interpolation - Height
bmiData <- bmiData %>%
  if_else(sex == M, mutate(
    LINT_h=(LMHI_h-((AGEHI-(ageyrs2decimal))/(AGEHI-AGELO))*(LMHI_h-LMLO_h)),
    MINT_h=(MMHI_h-((AGEHI-(ageyrs2decimal))/(AGEHI-AGELO))*(MMHI_h-MMLO_h)),
    SINT_h=(SMHI_h-((AGEHI-(ageyrs2decimal))/(AGEHI-AGELO))*(SMHI_h-SMLO_h))),
    mutate(
      LINT_h=(LFHI_h-((AGEHI-(ageyrs2decimal))/(AGEHI-AGELO))*(LFHI_h-LFLO_h)),
      MINT_h=(MFHI_h-((AGEHI-(ageyrs2decimal))/(AGEHI-AGELO))*(MFHI_h-MFLO_h)),
      SINT_h=(SFHI_h-((AGEHI-(ageyrs2decimal))/(AGEHI-AGELO))*(SFHI_h-SFLO_h))))


# Interpolation - Weight
bmiData <- bmiData %>%
  if_else(sex == M, mutate(
    LINT_w=(LMHI_w-((AGEHI-(ageyrs2decimal))/(AGEHI-AGELO))*(LMHI_w-LMLO_w)),
    MINT_w=(MMHI_w-((AGEHI-(ageyrs2decimal))/(AGEHI-AGELO))*(MMHI_w-MMLO_w)),
    SINT_w=(SMHI_w-((AGEHI-(ageyrs2decimal))/(AGEHI-AGELO))*(SMHI_w-SMLO_w))),
    mutate(
      LINT_w=(LFHI_w-((AGEHI-(ageyrs2decimal))/(AGEHI-AGELO))*(LFHI_w-LFLO_w)),
      MINT_w=(MFHI_w-((AGEHI-(ageyrs2decimal))/(AGEHI-AGELO))*(MFHI_w-MFLO_w)),
      SINT_w=(SFHI_w-((AGEHI-(ageyrs2decimal))/(AGEHI-AGELO))*(SFHI_w-SFLO_w))))


#SQ section - standard deviation scores



# Create epidemiological and clinical thresholds
# epidemiological
bmiData <- bmiData %<%
  case_when(
            (cent_b <= 2) ~ cent_grp1 = 1,
            ((cent_b > 2) & (cent_b < 85)) ~ cent_grp2 = 1,
            ((cent_b >= 85) & (cent_b < 95)) ~ cent_grp3 = 1,
            (cent_b >= 95) ~ cent_grp4 = 1,
            (cent_b >= 85) ~ cent_grp5 = 1
            )


# clinical
bmiData <- bmiData %<%
  case_when(
            (SDS_b <= -2.67) ~ clin_cent_grp1 = 1,
            ((SDS_b > -2.67) & (SDS_b < 1.33)) ~ clin_cent_grp2 = 1,
            ((SDS_b >=1.33) & (SDS_b < 2)) ~ clin_cent_grp3 = 1,
            ((SDS_b >= 2) & (SDS_b < 2.67)) ~ clin_cent_grp4 = 1,
            (SDS_b >= 2.67) ~ clin_cent_grp5 = 1,
            (SDS_b >= 1.33) ~ clin_cent_grp6 = 1,
            (SDS_b >= 2) ~ clin_cent_grp7 = 1
            )










### END OF SCRIPT ### ----