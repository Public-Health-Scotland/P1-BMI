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
                                     "Scottish_Postcode_Directory_2019_2.rds"))

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
dictdf <- haven::read_spss(paste(file.path(hostFolder, "ReferenceFiles", "Dictionary_School.sav")))
fulldatadic <- tibble(name = colnames(dictdf),
                      label = dictdf %>%
                        purrr::map(~ attr(.x, "label")) %>%
                        unlist())
#create list of labels used in bmiData
lookup_relevant <- semi_join(fulldatadic, bmiData %>% colnames(.) %>% tibble::enframe(name = NULL),
                             by = c("name" = "value"))
# at the date of review/height-weight recording




# Select children aged 4 years to under 8 years.
bmiData <- subset(bmiData, agemth >=48 | agemth <96) #604,209 obs


## Save out a file at this point that can be used to produce the total
## number of reviews for HB and CA for the coverage calculations.
## This file contains all reviews (with and without valid height and weight)
## save as data frame?
#saveRDS(hostFolder, "allReviews.rds")



# Convert the height and weight variables from string to numeric
bmiData <- mutate(bmiData, height = as.numeric(height),
                  weight = as.numeric(weight))

# Select only records with a valid (i.e. non-zero) height and weight
bmiData <- subset(bmiData, height != 0)
bmiData <- subset(bmiData, weight != 0) #603,685 obs
bmiData <- subset(bmiData, !is.na(sex))

# Create variable to show height in metres
#height in mm???
bmiData <- mutate(bmiData, height_m = height/1000)


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
bmiData <- bmiData %>%
  merge(bmiData, grd, by = "ageyr", all.x = TRUE, all.y = TRUE)
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
bmiData <- mutate(bmiData, ageyr = round(1000*(trunc(agemth)+1)/12)/1000)

# Merge data
bmiData <-  merge(bmiData, grd, by = c("ageyr"), all.x = TRUE, all.y = TRUE)
  # Rename the Growth Reference variables for the highest whole month
  # converted to years (HI)
  dplyr::rename(bmiData, LMHI_b = bmi_male_l, MMHI_b = bmi_male_m, SMHI_b = bmi_male_s, 
                LFHI_b = bmi_female_l, MFHI_b = bmi_female_m, SFHI_b = bmi_female_s,
                LMHI_h = height_male_l, MMHI_h = height_male_m, SMHI_h = height_male_s,
                LFHI_h = height_female_l, MFHI_h = height_female_m, SFHI_h = height_female_s,
                LMHI_w = weight_male_l, MMHI_w = weight_male_m, SMHI_w = weight_male_s, 
                LFHI_w = weight_female_l, MFHI_w = weight_female_m, SFHI_w = weight_female_s,
                agehi = ageyr)

# Calculate age in years to 2 decimal places for BMI, Height and Weight.
bmiData <- bmiData %>%
  mutate(ageyrs2decimal = round((agemth/12), 2))

#Change merge above to alternative join so following line no needed?
bmiData <- subset(bmiData, !is.na(sex))
# Interpolation - BMI
bmiData <- bmiData %>%
  if (istrue(bmiData$sex == "M"))
    {mutate(LINT_b=(LMHI_b-((AGEHI-(ageyrs2decimal))/(AGEHI-AGELO))*(LMHI_b-LMLO_b)),
            MINT_b=(MMHI_b-((AGEHI-(ageyrs2decimal))/(AGEHI-AGELO))*(MMHI_b-MMLO_b)),
            SINT_b=(SMHI_b-((AGEHI-(ageyrs2decimal))/(AGEHI-AGELO))*(SMHI_b-SMLO_b)))
    } else if ((substr(bmiData$sex,1,1) == "F"))
    {mutate(LINT_b=(LFHI_b-((AGEHI-(ageyrs2decimal))/(AGEHI-AGELO))*(LFHI_b-LFLO_b)),
            MINT_b=(MFHI_b-((AGEHI-(ageyrs2decimal))/(AGEHI-AGELO))*(MFHI_b-MFLO_b)),
            SINT_b=(SFHI_b-((AGEHI-(ageyrs2decimal))/(AGEHI-AGELO))*(SFHI_b-SFLO_b)))
    } else {"Unexpected value for sex"
    }


# Interpolation - Height
bmiData <- bmiData %>%
  if (bmiData$sex == "M")
  {mutate(LINT_h=(LMHI_h-((AGEHI-(ageyrs2decimal))/(AGEHI-AGELO))*(LMHI_h-LMLO_h)),
          MINT_h=(MMHI_h-((AGEHI-(ageyrs2decimal))/(AGEHI-AGELO))*(MMHI_h-MMLO_h)),
          SINT_h=(SMHI_h-((AGEHI-(ageyrs2decimal))/(AGEHI-AGELO))*(SMHI_h-SMLO_h)))
  } else if (sex == "F")
    {mutate(LINT_h=(LFHI_h-((AGEHI-(ageyrs2decimal))/(AGEHI-AGELO))*(LFHI_h-LFLO_h)),
            MINT_h=(MFHI_h-((AGEHI-(ageyrs2decimal))/(AGEHI-AGELO))*(MFHI_h-MFLO_h)),
            SINT_h=(SFHI_h-((AGEHI-(ageyrs2decimal))/(AGEHI-AGELO))*(SFHI_h-SFLO_h)))
  }


# Interpolation - Weight
bmiData <- bmiData %>%
  if (bmiData$sex == "M")
  {mutate(LINT_w=(LMHI_w-((AGEHI-(ageyrs2decimal))/(AGEHI-AGELO))*(LMHI_w-LMLO_w)),
          MINT_w=(MMHI_w-((AGEHI-(ageyrs2decimal))/(AGEHI-AGELO))*(MMHI_w-MMLO_w)),
          SINT_w=(SMHI_w-((AGEHI-(ageyrs2decimal))/(AGEHI-AGELO))*(SMHI_w-SMLO_w)))
  } else if (sex == "F")
  {mutate(LINT_w=(LFHI_w-((AGEHI-(ageyrs2decimal))/(AGEHI-AGELO))*(LFHI_w-LFLO_w)),
          MINT_w=(MFHI_w-((AGEHI-(ageyrs2decimal))/(AGEHI-AGELO))*(MFHI_w-MFLO_w)),
          SINT_w=(SFHI_w-((AGEHI-(ageyrs2decimal))/(AGEHI-AGELO))*(SFHI_w-SFLO_w)))
  }


#Calculate standard deviation scores
bmiData <- bmiData %>%
  mutate((bmi_2nd = round(bmi, 2)),
         # Calculate SD (standardised) score using BMI to two decimal places.
         SDS2_b = (((bmi_2nd/MINT_b)**LINT_b)-1)/(LINT_b*SINT_b),
         #compute SDS to 2 decimal places.
         SDS2_b=round(SDS2_b,2),
         # Calculate centiles. pnorm is the R function for Cumulative Distribution Function (CDF)
         cent_b=100 * pnorm(SDS_b, mean = 0, sd = 1),
         (height_2nd = round(h, 2)),
         # Calculate SD (standardised) score using height to two decimal places.
         SDS2_h = (((height_2nd/MINT_h)**LINT_h)-1)/(LINT_h*SINT_h),
         #compute SDS to 2 decimal places.
         SDS2_h=round(SDS2_h,2),
         # Calculate centiles.
         cent_h=100 * pnorm(SDS_h, mean = 0, sd = 1),
         (weight_2nd = round(w, 2)),
         # Calculate SD (standardised) score using height to two decimal places.
         SDS2_w = (((weight_2nd/MINT_w)**LINT_w)-1)/(LINT_w*SINT_w),
         #compute SDS to 2 decimal places.
         SDS2_w=round(SDS2_w,2),
         # Calculate centiles. pnorm is the R function for Cumulative Distribution Function (CDF)
         cent_w=100 * pnorm(SDS_w, mean = 0, sd = 1))

# select out those outwith the range deemed to be real.
bmiData <- subset(bmiData, (sds_b >= -7 & sds_b <= 7) & (sds_h >= -7 & sds_h <= 7) & (sds_w >= -7 & sds_w <= 7))


#Lines 490-506. TO BE FIXED.
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

#Lines 508 - 
bmiData <- bmiData %>%
  mutate(bmiData, tot = 1)
arrange(pc7)

#saveRDS(hostFolder, "BMI_data_0102_1718.rds")

# read in deprivation lookup. 
simd2016 <- readRDS(paste0(lookupFolder,
                           "/Unicode/Deprivation/postcode_2019_2_simd2016.rds"))
simd2016 <- subset(simd2016, select = c(pc7, simd2016_sc_quintile))
simd2012 <- read_spss(paste0(lookupFolder,
                             "/Unicode/Deprivation/postcode_2016_1_simd2012.sav"))
simd2012 <- subset(simd2012, select = c(pc7, simd2012_sc_quintile))
simd2009 <- read_spss(paste0(lookupFolder,
                             "/Unicode/Deprivation/postcode_2012_2_simd2009v2.sav"))
simd2009 <- subset(simd2009, select = c(pc7, simd2009v2_sc_quintile))
dplyr::rename(simd2009, pc7 = PC7)
simd2006 <- read_spss(paste0(lookupFolder, 
                             "/Unicode/Deprivation/postcode_2009_2_simd2006.sav"))
simd2006 <- subset(simd2006, select = c(pc7, simd2006_sc_quintile))
dplyr::rename(simd2006, pc7 = PC7)
simd2004 <- read_spss(paste0(lookupFolder, 
                             "/Unicode/Deprivation/postcode_2006_2_simd2004.sav"))
simd2004 <- subset(simd2004, select = c(pc7, simd2004_sc_quintile))
dplyr::rename(simd2004, pc7 = PC7)


#Is this more efficient?
# simd2016 <- readRDS(paste0(lookupFolder, 
#                            "/Unicode/Deprivation/postcode_2019_2_simd2016.rds")) %>%
#   select(pc7, simd2016_sc_quintile) %>%
#   rename(postcode = pc7,
#          simd = simd2016_sc_quintile) %>%
#   mutate(year = "simd_2016")
# 
# simd_2012 <- read_spss(paste0(lookupFolder, 
#                               "/Unicode/Deprivation/postcode_2016_1_simd2012.sav")) %>%
#   select(pc7, simd2012_sc_quintile) %>%
#   rename(postcode = pc7,
#          simd = simd2012_sc_quintile) %>%
#   mutate(year = "simd_2012")
# 
# simd_2009 <- read_spss(paste0(lookupFolder,
#                               "/Unicode/Deprivation/",
#                               "postcode_2012_2_simd2009v2.sav")) %>%
#   select(PC7, simd2009v2_sc_quintile) %>%
#   rename(postcode = PC7,
#          simd = simd2009v2_sc_quintile) %>%
#   mutate(year = "simd_2009")
# 
# simd_2006 <- read_spss(paste0(lookupFolder,
#                               "/Unicode/Deprivation/",
#                               "postcode_2009_2_simd2006.sav")) %>%
#   select(PC7, simd2006_sc_quintile) %>%
#   rename(postcode = PC7,
#          simd = simd2006_sc_quintile) %>%
#   mutate(year = "simd_2006")
# 
# simd_2004 <- read_spss(paste0(lookupFolder,
#                               "/Unicode/Deprivation/",
#                               "postcode_2006_2_simd2004.sav")) %>%
#   select(PC7, simd2004_sc_quintile) %>%
#   rename(postcode = PC7,
#          simd = simd2004_sc_quintile) %>%
#   mutate(year = "simd_2004")

# Assign the appropriate SIMD value to a patient depending on the year they
# were admitted
# data <- data %>%
#   mutate(simd = case_when(
#     year >= 2014 ~ simd2016_sc_quintile,
#     year >= 2010 & year <= 2013 ~ simd2012_sc_quintile,
#     year >= 2007 & year <= 2009 ~ simd2009v2_sc_quintile,
#     year >= 2004 & year <= 2006 ~ simd2006_sc_quintile,
#     year <= 2003 ~ simd2004_sc_quintile
#   )) 

simdMerge <- function(x, y){
  df <- merge(x, y, by= "pc7", all.x= TRUE, all.y= TRUE)
  return(df)
}
simd <- Reduce(simdMerge, list(simd2016, simd2012, simd2009, simd2006, simd2004))

rescale <- function(x_i){
  5-x_i
}
bmiData <- bmiData %>%
  merge(bmiData, simd, by = pc7)
#Recode simd so all go from 1=most to 5=least - this was required for creating allsimdyear/allsimdfinyear----------------*.
#Still to change labels
mutate(simd2004_sc_quintile <- sapply(simd2004_sc_quintile,rescale),
       simd2006_sc_quintile <- sapply(simd2006_sc_quintile,rescale),
       allsimdfinyear=0)


#if(schlyr_exam ==a allsimdfinyear=b)
bmiData < bmiData %>%
  for (schlyr_exam in c('0102', '0203', '0304', '0405', '0506', '0607','0708', '0809', '0910','1011',
                        '1112', '1213', '1314', '1415', '1516', '1617', '1718')) {
    allsimdfinyear <- c('simd2004_sc_quintile', 'simd2004_sc_quintile', 'simd2004_sc_quintile',
                        'simd2006_sc_quintile', 'simd2006_sc_quintile', 'simd2006_sc_quintile',
                        'simd2009v2_sc_quintile', 'simd2009v2_sc_quintile','simd2009v2_sc_quintile',
                        'simd2012_sc_quintile', 'simd2012_sc_quintile', 'simd2012_sc_quintile',
                        'simd2012_sc_quintile', 'simd2016_sc_quintile', 'simd2016_sc_quintile',
                        'simd2016_sc_quintile',
                        'simd2016_sc_quintile')}
# a= c('0102', '0203', '0304', '0405', '0506', '0607','0708', '0809', '0910','1011', '1112', '1213', '1314',
#      '1415', '1516', '1617', '1718')
# b= c('simd2004_sc_quintile', 'simd2004_sc_quintile', 'simd2004_sc_quintile', 'simd2006_sc_quintile',
#      'simd2006_sc_quintile', 'simd2006_sc_quintile', 'simd2009v2_sc_quintile', 'simd2009v2_sc_quintile',
#      'simd2009v2_sc_quintile', 'simd2012_sc_quintile', 'simd2012_sc_quintile', 'simd2012_sc_quintile',
#      'simd2012_sc_quintile', 'simd2016_sc_quintile', 'simd2016_sc_quintile', 'simd2016_sc_quintile',
#      'simd2016_sc_quintile')
# bmiData < bmiData %>%
#   for (schlyr_exam in a) {
#     allsimdfinyear <- b}
mutate(simd = allsimdfinyear),
if is.na(allsimdfinyear) simd='U', #x of n records don't have a SIMD quintile.

# Councils - create the old CA with codes 01-32 ; the new codes are made unique by the last 2 chars so use them   .
bmiData < bmiData %>%
  for (CA in c("33", "34", "41", "35", "26", "05", "39", "06", "42", "08", "45", "10", "11", "36", "14", "47", "46", "17", "18", "19", "20", "21", "44", "23", "48", "38", "27", "28", "29", "30", "40", "13")) {
       CA <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "20", "21", "22", "23", "24", "25", "26", "27", "28", "29", "30", "31", "32")}, 
mutate(council_area_name <- sapply(council_area_name,rescale)), 
# CA_name = case_when(CA == 1 ~ "Aberdeen City",
#                     CA == 2 ~ "Aberdeenshire",
#                     CA == 3 ~ "Angus", 
#                     CA == 4 ~ "Argyll & Bute",
#                     CA == 5 ~ "Scottish Borders", 
#                     CA == 6 ~ "Clackmannanshire",
#                     CA == 7 ~ "West Dunbartonshire", 
#                     CA == 8 ~ "Dumfries & Galloway",
#                     CA == 9 ~ "Dundee City", 
#                     CA == 10 ~ "East Ayrshire",
#                     CA == 11 ~ "East Dunbartonshire", 
#                     CA == 12 ~ "East Lothian",
#                     CA == 13 ~ "East Renfrewshire", 
#                     CA == 14 ~ "City of Edinburgh",
#                     CA == 15 ~ "Falkirk",
#                     CA == 16 ~ "Fife",
#                     CA == 17 ~ "Glasgow City", 
#                     CA == 18 ~ "Highland",
#                     CA == 19 ~ "Inverclyde",
#                     CA == 20 ~ "Midlothian",
#                     CA == 21 ~ "Moray",
#                     CA == 22 ~ "North Ayrshire",
#                     CA == 23 ~ "North Lanarkshire", 
#                     CA == 24 ~ "Orkney Islands",
#                     CA == 25 ~ "Perth & Kinross",
#                     CA == 26 ~ "Renfrewshire",
#                     CA == 27 ~ "Shetland Islands",
#                     CA == 28 ~ "South Ayrshire",
#                     CA == 29 ~ "South Lanarkshire",
#                     CA == 30 ~ "Stirling",
#                     CA == 31 ~ "West Lothian",
#                     CA == 32 ~ "Comhairle nan Eilean Siar",
#                     TRUE ~ "Other"),
mutate carea=paste("CA",CA),


arrange(bmiData, chi)


bmiData <- bmiData %>%
  subset(select = c(chi id HB2018 CA2018 CA carea sex1 height weight daterec schlyr_exam schlgivn rev_num agemth nyob mob dob PC7
                    simd2016_sc_quintile simd2012_sc_quintile simd2009v2_sc_quintile simd2006_sc_quintile simd2004_sc_quintile simd sex h w hm BMI 
                    cent_grp1 cent_grp2 cent_grp3 cent_grp4 cent_grp5 tot clin_cent_grp1 clin_cent_grp2 clin_cent_grp3 clin_cent_grp4 clin_cent_grp5 clin_cent_grp6 clin_cent_grp7
                    AGELO LMLO_b MMLO_b SMLO_b LFLO_b MFLO_b SFLO_b AGEHI LMHI_b MMHI_b SMHI_b LFHI_b MFHI_b SFHI_b LINT_b MINT_b SINT_b SDS_b cent_b
                    LMLO_h MMLO_h SMLO_h LFLO_h MFLO_h SFLO_h LMHI_h MMHI_h SMHI_h LFHI_h MFHI_h SFHI_h LINT_h MINT_h SINT_h SDS_h cent_h
                    LMLO_w MMLO_w SMLO_w LFLO_w MFLO_w SFLO_w LMHI_w MMHI_w SMHI_w LFHI_w MFHI_w SFHI_w LINT_w MINT_w SINT_w SDS_w cent_w))
# This file contains data for school years 2001/02 to 2017/18 and should be used for information requests etc.
# so that any figures produced match those published in for financial year 2017/18.
saveRDS(bmiData, (hostFolder, "BMI_data_0102_1718.rds"))








### END OF SCRIPT ### ----