#Need to create two separate files for Tableau, as coverage can't be broken down by SIMD and sex etc.


Define !SourceData ()
'/PHI_conf/ChildHealthSurveillance/Topics/Obesity/Publications/Primary1BMI/20181211/Analysis/MainAnalysis/BMI_data_0102_1718.sav'
!EndDefine.

Define !output_folder ()
'/PHI_conf/ChildHealthSurveillance/Topics/Obesity/Publications/Primary1BMI/20181211/Tableau/'
!EndDefine.

source("./Code/functions.R")

## File Locations
# Source Data
server_desktop <- "server"

if (server_desktop == "server") {
  host_folder <- "//PHI_conf/ChildHealthSurveillance/Topics/Obesity/Publications/Primary1BMI/20191210/RAP/"
  source_folder <- "//PHI_conf/ChildHealthSurveillance/Portfolio/Data"
  lookupFolder <- "/conf/linkage/output/lookups"
  output_folder <- "//PHI_conf/ChildHealthSurveillance/Topics/Obesity/Publications/Primary1BMI/20191210/RAP/Tableau/"
  
} else if (server_desktop == "desktop") {
  host_folder <- "//stats/ChildHealthSurveillance/Topics/Obesity/Publications/Primary1BMI/20191210/RAP/"
  source_folder <- "//stats/ChildHealthSurveillance/Portfolio/Data"
  lookupFolder <-"//Isdsf00d03/cl-out/lookups"
  output_folder <- "//stats/ChildHealthSurveillance/Topics/Obesity/Publications/Primary1BMI/20191210/RAP/Tableau/"
}

## Variables
# Current School/Financial Year (e.g. 1819)
currentYr <- 1718


tableau_file <- readRDS(paste0(host_folder, "BMI_data_0102_1718.rds")) %>%
  subset(schlyr_exam != "2000/01")  %>%
  #Recode school years
  mutate(school_year = paste0("20", substr(schlyr_exam,1,2), "/", substr(schlyr_exam,3,4)),
         #Year Ending variable for use in Tableau time trends
         year_ending = paste0("30/06/20", substr(school_year,6,7))) %>%
  apply_ca_desc() %>%
  apply_hb_desc() %>%
  dplyr::rename(HB_RESIDENCE_DESC = hbres_name, COUNCIL_AREA_DESC = council_area_desc, N_Valid_Height_Weight = tot, Epi_Underweight = cent_grp1, Epi_HealthyWeight = cent_grp2, Epi_Overweight = cent_grp3,
                Epi_Obese = cent_grp4, Epi_OverweightObese = cent_grp5, Clin_Underweight = clin_cent_grp1,
                Clin_HealthyWeight = clin_cent_grp2, Clin_Overweight = clin_cent_grp3, Clin_Obese = clin_cent_grp4,
                Clin_SeverelyObese = clin_cent_grp5, Clin_OverweightObeseSevObese = clin_cent_grp6, Clin_Obese_SevObese = clin_cent_grp7) %>%
  #Set up data so that donut charts can be created
  mutate(Epidemiological_Grouping = case_when(Epi_Underweight == 1 ~ "At risk of underweight",
                                              Epi_HealthyWeight == 1 ~ "Healthy Weight",
                                              Epi_Overweight == 1 ~ "At risk of overweight",
                                              Epi_Obese == 1 ~ "At risk of obesity"),
         Clinical_Grouping = case_when(Clin_Underweight == 1 ~ "Clinically underweight",
                                       Clin_HealthyWeight == 1 ~ "Clinically healthy weight", 
                                       Clin_Overweight == 1 ~ "Clinically overweight", 
                                       Clin_Obese == 1 ~ "Clinically obese", 
                                       Clin_SeverelyObese == 1 ~ "Clinically severely obese"))

haven::write_sav(tableau_file, paste0(output_folder, "P1BMIData.zsav"), compress = TRUE)

#Aggregated data to take into Tableau

#Aggregate HB res data
hb_res_file <- haven::read_sav(paste0(output_folder, "P1BMIData.zsav")) %>%
  group_by(school_year, year_ending, HB2019, HB_RESIDENCE_DESC, simd, sex) %>%
  summarise_at(vars(N_Valid_Height_Weight:Clin_Obese_SevObese), sum) %>%
  ungroup()

haven::write_sav(hb_res_file, paste0(output_folder, "P1BMI_board_agg.sav"), compress = FALSE)

#Aggregate LA data
la_file <- haven::read_sav(paste0(output_folder, "P1BMIData.zsav")) %>%
  group_by(school_year, year_ending, CA2019, COUNCIL_AREA_DESC, simd, sex) %>%
  summarise_at(vars(N_Valid_Height_Weight:Clin_Obese_SevObese), sum) %>%
  ungroup()
haven::write_sav(la_file, paste0(host_folder, "Temp", "P1BMI_LA_temp_agg.sav"), compress = FALSE)


## Select relevant years function
apply_la_year <- function(x, la, school_year_list) {
  
  x <- x %>%
    if ((CA2019 == la) && (school_year %in% "school_year_list")) {
      mutate(flag = 1)
    }
  return(x)
}

#exclude data for years when local authorities have less than 50 records
apply_la_year <- function(x) {
  x <- x %>%
    mutate(flag = case_when((CA2019 == "S12000036") && (school_year %in% c("2001/02","2002/03","2003/04")) ~ 1,
                            (CA2019 == "S12000045") && (school_year %in% c("2006/07","2007/08","2008/09","2010/11","2016/17")) ~ 1,
                            (CA2019 == "S12000010") && (school_year %in% c("2001/02","2003/04")) ~ 1,
                            (CA2019 == "S12000011") && (school_year %in% c("2006/07","2007/08","2008/09","2010/11")) ~ 1,
                            (CA2019 == "S12000049") && (school_year %in% c("2006/07","2007/08","2008/09","2010/11")) ~ 1,
                            (CA2019 == "S12000018") && (school_year %in% c("2006/07","2007/08","2008/09","2010/11")) ~ 1,
                            (CA2019 == "S12000019") && (school_year %in% c("2003/04")) ~ 1,
                            (CA2019 == "S12000013") && (school_year %in% c("2004/05")) ~ 1,
                            (CA2019 == "S12000039") && (school_year %in% c("2006/07","2008/09","2010/11")) ~ 1,
                            0))
  return(x)
}

la_file <- la_file %>%
  apply_la_year() %>%
  filter(flag != 1)

haven::write_sav(la_file, paste0(host_folder, "Temp", "P1BMI_LA_agg.sav"), compress = FALSE)

#Scotland level aggregates (called All participating boards) as someboards have no data in earlier years
all_part_hb_file <- haven::read_sav(paste0(output_folder, "P1BMIData.zsav")) %>%
  group_by(school_year, year_ending, simd, sex) %>%
  summarise_at(vars(N_Valid_Height_Weight:Clin_Obese_SevObese), sum) %>%
  mutate(HB2019 = "Total",
         HB_RESIDENCE_DESC = "All participating boards") %>%
  ungroup()

haven::write_sav(all_part_hb_file, paste0(host_folder, "Temp", "P1BMI_Scotland_agg.sav"), compress = FALSE)

p1_bmi_agg_file <- bind_rows(hb_res_file, la_file, all_part_hb_file) %>%
  mutate_all(~replace(., is.na(.), 0))

haven::write_sav(p1_bmi_agg_file, paste0(output_folder, "P1BMI_agg.sav"), compress = FALSE)

#HB donut

donut_hb_res_file <- tableau_file %>%
  group_by(school_year, year_ending, HB_RESIDENCE_DESC, Epidemiological_Grouping, Clinical_Grouping, simd, sex) %>%
  summarise(N_Records = n()) %>%
  ungroup()
haven::write_sav(donut_hb_res_file, paste0(host_folder, "Temp", "P1BMIData_donuts_Board.sav"), compress = FALSE)

#LA donut

donut_la_file <- tableau_file %>%
  apply_la_year() %>%
  filter(flag != 1) %>%
  group_by(school_year, year_ending, COUNCIL_AREA_DESC, Epidemiological_Grouping, Clinical_Grouping, simd, sex) %>%
  summarise(N_Records = n()) %>%
  ungroup()
haven::write_sav(donut_la_file, paste0(host_folder, "Temp", "P1BMIData_donuts_Board.sav"), compress = FALSE)


#Scotland level aggregates (called All participating boards) as someboards have no data in earlier years
donut_all_part_hb_file <- tableau_file %>%
  mutate(HB_RESIDENCE_DESC = "All participating boards",
         COUNCIL_AREA_DESC = "All participating boards") %>%
  group_by(school_year, year_ending, HB_RESIDENCE_DESC, COUNCIL_AREA_DESC, Epidemiological_Grouping, Clinical_Grouping, simd, sex) %>%
  summarise(N_Records = n()) %>%
  ungroup()
haven::write_sav(donut_la_file, paste0(host_folder, "Temp", "P1BMIData_donuts_Scotland.sav"), compress = FALSE)

p1_bmi_donut_file <- rbind(donut_hb_res_file, donut_la_file, donut_all_part_hb_file) %>%
  
  haven::write_sav(donut_la_file, paste0(output_folder, "P1BMI_donut_data.sav"), compress = FALSE)


# Local Authority coverage data
#Only Western Isles differs from P1_BMI_Stats.R script: Comhairle nan Eilean Siar -> Na h-Eileanan Siar
ca_completeness_data <- ca_completeness_data %>%
  subset(schlyr_exam != "2000/01")  %>%
  #Recode school years
  mutate(school_year = paste0("20", substr(schlyr_exam,1,2), "/", substr(schlyr_exam,3,4)),
         #Year Ending variable for use in Tableau time trends
         year_ending = paste0("30/06/20", substr(school_year,6,7))) %>%
  apply_ca_desc() %>%
  dplyr::rename(pop_estimate = pop,tot_revs = total_reviews, 
                valid_revs = valid_reviews, 
                COUNCIL_AREA_DESC = council_area_desc, SchoolYear = school_year, 
                YearEnding = year_ending)

haven::write_sav(ca_completeness_data, paste0(host_folder, "Temp", "P1_LA_coverage_data.sav"), compress = FALSE)


### -- Board & Scotland coverage data

hb_completeness_data <- hb_completeness_data %>%
  subset(schlyr_exam != "2000/01")  %>%
  #Recode school years
  mutate(school_year = paste0("20", substr(schlyr_exam,1,2), "/", substr(schlyr_exam,3,4)),
         #Year Ending variable for use in Tableau time trends
         year_ending = paste0("30/06/20", substr(school_year,6,7))) %>%
  apply_hb_desc() %>%
  dplyr::rename(pop_estimate = pop,tot_revs = total_reviews, 
                valid_revs = valid_reviews, 
                HB_RESIDENCE_DESC = hbres_name, SchoolYear = school_year, 
                YearEnding = year_ending)


haven::write_sav(hb_completeness_data, paste0(host_folder, "Temp", "P1_Board_coverage_data.sav"), compress = FALSE)


completeness_data <- bind_rows(hb_completeness_data, ca_completeness_data) %>%
  mutate(COUNCIL_AREA_DESC = if (HB_RESIDENCE_DESC == "All Participating Boards") ~ "All Participating Boards")

haven::write_sav(completeness_data, paste0(output_folder, "P1_Board_coverage_data.sav"), compress = FALSE)



### End of script