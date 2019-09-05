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
#Read in labels of full dictionary
dictdf <- haven::read_spss(paste(file.path(hostFolder, "ReferenceFiles", "Dictionary_School.sav")))
fulldatadic <- tibble(name = colnames(dictdf),
                 label = dictdf %>%
                   purrr::map(~ attr(.x, "label")) %>%
                   unlist())
#create list of labels used in bmiData
lookup_relevant <- semi_join(lookup, bmiData %>% colnames(.) %>% tibble::enframe(name = NULL),
                             by = c("name" = "value"))

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
bmiData <- mutate(bmiData, height_m = height/1000)


# Calculate BMI
bmiData <- mutate(bmiData, bmi = weight/(height_m*height_m))

# Child's age in months will lie between two ages in whole months in the lookup table.
# Line below calculates the next lowest whole month and converts to years
bmiData <- mutate(bmiData, ageyr = round(1000*trunc(agemth)/12)/1000)




# The look-up file is matched in using the lowest whole month converted to years.
# The corresponding L,M,S are the lowest (LO) values used in the interpolation.
bmiData <- arrange(bmiData, ageyr)

grd <- readRDS(file.path(hostFolder, "ReferenceFiles", "UK1990_BMI_Growth_Reference_Data.rds"))

# Merge data
bmiData <- merge(bmiData, grd, by = c("ageyr"), all.x = TRUE, all.y = TRUE)

###lines 432-486

case_when()
function sdscore()
scale()
#bmi rounded to 2 decimals.
bmiData <- bmiData %>%
  mutate((bm2nd = round(bmi, 2)),
        # Calculate SD (standardised) score using BMI to two decimal places.
        SDS2_b = (((bmi_2d/MINT_b)**LINT_b)-1)/(LINT_b*SINT_b),
        #compute SDS to 2 decimal places.
        SDS_b=round(SDS2_b,2),
        # Calculate centiles. pnorm is the R function for Cumulative Distribution Function (CDF)
        cent_b=100 * pnorm(SDS_b, mean = 0, sd = 1))

* select out those outwith the range deemed to be real.
bmiData <- subset(bmiData, sds_b >= -7 & sds_b <= 7)

*height rounded to 2 decimals.
compute height_2d=rnd(h,0.01).
* Calculate SD (standardised) score using height to two decimal places.
compute SDS2_h=(((height_2d/MINT_h)**LINT_h)-1)/(LINT_h*SINT_h).
execute.

*compute SDS to 2 decimal places.
compute SDS_h=rnd(SDS2_h,0.01).


* Calculate centiles.
numeric cent_h (f4.1).
compute cent_h=100*CDF.Normal(SDS_h,0,1).
execute.

*select out those outwith the range deemed to be real.
select if range(sds_h,-7,7).
execute.

*weight rounded to 2 decimals.
compute weight_2d=rnd(w,0.01).
*Calculate SD (standardised) score using BMI to two decimal places.
compute SDS2_w=(((weight_2d/MINT_w)**LINT_w)-1)/(LINT_w*SINT_w).
execute.

*compute SDS to 2 decimal places.
compute SDS_w=rnd(SDS2_w,0.01).


*Calculate centiles.
numeric cent_w (f4.1).
compute cent_w=100*CDF.Normal(SDS_w,0,1).
execute.

* select out those outwith the range deemed to be real.
select if range(sds_w,-7,7).
execute.


### END OF SCRIPT ### ----