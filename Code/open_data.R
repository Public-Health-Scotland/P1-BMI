
source("./Code/functions.R")

## File Locations
# Source Data
server_desktop <- "server"

if (server_desktop == "server") {
  host_folder <- "//PHI_conf/ChildHealthSurveillance/Topics/Obesity/Publications/Primary1BMI/20191210/RAP/"
  source_folder <- "//PHI_conf/ChildHealthSurveillance/Portfolio/Data/"
  lookupFolder <- "/conf/linkage/output/lookup/s"
  output_folder <- "//PHI_conf/ChildHealthSurveillance/Topics/Obesity/Publications/Primary1BMI/20191210/RAP/Tableau/"
  open_data_folder <- "//PHI_conf/ChildHealthSurveillance/Topics/Obesity/Publications/Primary1BMI/20191210/RAP/OpenData/"
  
} else if (server_desktop == "desktop") {
  host_folder <- "//stats/ChildHealthSurveillance/Topics/Obesity/Publications/Primary1BMI/20191210/RAP/"
  source_folder <- "//stats/ChildHealthSurveillance/Portfolio/Data/"
  lookupFolder <-"//Isdsf00d03/cl-out/lookups/"
  output_folder <- "//stats/ChildHealthSurveillance/Topics/Obesity/Publications/Primary1BMI/20191210/RAP/Tableau/"
  open_data_folder <- "//stats/ChildHealthSurveillance/Topics/Obesity/Publications/Primary1BMI/20191210/RAP/OpenData/"
}

## Variables
# Current School/Financial Year (e.g. 1819)
currentYr <- 1718

open_data_coverage_file <- haven::read_sav(file.path(output_folder, "P1_coverage_data.sav")) %>%
  dplyr::rename(HBR2014 = HB2019, CA2011 = CA2019, TotalReviews = tot_revs, ValidReviews = valid_revs, PopulationEstAge5 = pop_estimate) %>%
  mutate(HBR2014 = case_when(HBR2014 == "A" ~ "S08000015",
                            HBR2014 == "B" ~ "S08000016",
                            HBR2014 == "Y" ~ "S08000017",
                            HBR2014 == "V" ~ "S08000019",
                            HBR2014 == "N" ~ "S08000020",
                            HBR2014 == "G" ~ "S08000021",
                            HBR2014 == "H" ~ "S08000022",
                            HBR2014 == "L" ~ "S08000023",
                            HBR2014 == "S" ~ "S08000024",
                            HBR2014 == "R" ~ "S08000025",
                            HBR2014 == "Z" ~ "S08000026",
                            HBR2014 == "W" ~ "S08000028",
                            HBR2014 == "F" ~ "S08000029",
                            HBR2014 == "T" ~ "S08000030"))

haven::write_sav(open_data_coverage_file, file.path(open_data_folder, "P1_coverage_data.sav"), compress = FALSE)

###  1 - Board coverage data ----
hb_open_data_coverage_file <- haven::read_sav(file.path(open_data_folder, "P1_coverage_data.sav")) %>%
  dplyr::filter(CA2011=="") %>%
  subset(select = c(HBR2014, SchoolYear, PopulationEstAge5, TotalReviews, ValidReviews))

write_csv(hb_open_data_coverage_file, file.path(open_data_folder, "OD_P1BMI_Coverage_Board.csv"))


### 2 - LA coverage data ----
la_open_data_coverage_file <- haven::read_sav(file.path(open_data_folder, "P1_coverage_data.sav")) %>%
  dplyr::filter(HBR2014=="") %>%
  subset(select = c(CA2011, SchoolYear, PopulationEstAge5, TotalReviews, ValidReviews))

write_csv(la_open_data_coverage_file, file.path(open_data_folder, "OD_P1BMI_Coverage_LA.csv"))




p1_bmi_agg_file <- haven::read_sav(file.path(output_folder, "P1BMI_agg.sav")) %>%
  dplyr::rename(HBR2014 = HB2019, CA2011 = CA2019, SIMD = simd, Sex = sex,
                SchoolYear = school_year, ValidReviews = N_Valid_Height_Weight, 
                EpiUnderweight = Epi_Underweight,
                EpiHealthyWeight = Epi_HealthyWeight, 
                EpiOverweight = Epi_Overweight, EpiObese = Epi_Obese, 
                EpiOverweightAndObese = Epi_OverweightObese,
                ClinUnderweight = Clin_Underweight, 
                ClinHealthyWeight = Clin_HealthyWeight, 
                ClinOverweight = Clin_Overweight, 
                ClinObese = Clin_Obese, ClinSeverelyObese = Clin_SeverelyObese, 
                ClinOverweightObeseAndSeverelyObese = Clin_OverweightObeseSevObese,
                ClinObeseAndSeverelyObese = Clin_Obese_SevObese) %>%
  mutate(SIMDVersion = case_when(
    SchoolYear %in% c("2001/02", "2002/03", "2003/04") ~ "SIMD2004",
    SchoolYear %in% c("2004/05", "2005/06", "2006/07") ~ "SIMD2006",
    SchoolYear %in% c("2007/08", "2008/09", "2009/10") ~ "SIMD2009",
    SchoolYear %in% c("2010/11", "2011/12", "2012/13") ~ "SIMD2010",
    SchoolYear %in% c("2014/15", "2015/16", "2016/17", "2017/18", "2018/19") ~ "SIMD2016")) %>% 
  filter(SIMD != 0) %>%
  subset(select = -c(year_ending, flag))

haven::write_sav(p1_bmi_agg_file, file.path(open_data_folder, "P1BMI_agg.sav"), compress = FALSE)

### 3 - Board by gender (epidemiological) ----
hb_gender_epi_file <- haven::read_sav(file.path(open_data_folder, "P1BMI_agg.sav")) %>% 
  subset(select = c(SchoolYear, HBR2014, Sex, ValidReviews, EpiUnderweight, 
                    EpiHealthyWeight, EpiOverweight, EpiObese, 
                    EpiOverweightAndObese)) %>%
  filter(!(HBR2014 %in% c("0", "Total"))) %>%
  group_by(SchoolYear, HBR2014, Sex) %>%
  summarise_all(sum) %>%
  ungroup()

haven::write_sav(hb_gender_epi_file, file.path(open_data_folder, "OD_P1BMI_HB_Gender_Epi.sav"), compress = FALSE)
write_csv(hb_gender_epi_file, file.path(open_data_folder, "OD_P1BMI_HB_Gender_Epi.csv"))

### 4 - LA by gender (epidemiological) ----
la_gender_epi_file <- haven::read_sav(file.path(open_data_folder, "P1BMI_agg.sav")) %>% 
  subset(select = c(SchoolYear, CA2011, Sex, ValidReviews, EpiUnderweight, 
                    EpiHealthyWeight, EpiOverweight, EpiObese, 
                    EpiOverweightAndObese)) %>%
  filter(!(CA2011 %in% c("0", "Total"))) %>%
  group_by(SchoolYear, CA2011, Sex) %>%
  summarise_all(sum) %>%
  ungroup()

haven::write_sav(la_gender_epi_file, file.path(open_data_folder, "OD_P1BMI_LA_Gender_Epi.sav"), compress = FALSE)
write_csv(la_gender_epi_file, file.path(open_data_folder, "OD_P1BMI_LA_Gender_Epi.csv"))


### 5 - Board by SIMD (epidemiological) ----
hb_simd_epi_file <- haven::read_sav(file.path(open_data_folder, "P1BMI_agg.sav")) %>% 
  subset(select = c(SchoolYear, HBR2014, SIMD, SIMDVersion, ValidReviews, EpiUnderweight, 
                    EpiHealthyWeight, EpiOverweight, EpiObese, 
                    EpiOverweightAndObese)) %>%
  filter(!(HBR2014 %in% c("0", "Total"))) %>%
  group_by(SchoolYear, HBR2014, SIMD, SIMDVersion) %>%
  summarise_all(sum) %>%
  ungroup()

haven::write_sav(hb_simd_epi_file, file.path(open_data_folder, "OD_P1BMI_HB_SIMD_Epi.sav"), compress = FALSE)
write_csv(hb_simd_epi_file, file.path(open_data_folder, "OD_P1BMI_HB_SIMD_Epi.csv"))


### 6 - Local Authority by SIMD (epidemiological) ----
la_simd_epi_file <- haven::read_sav(file.path(open_data_folder, "P1BMI_agg.sav")) %>% 
  subset(select = c(SchoolYear, CA2011, SIMD, SIMDVersion, ValidReviews, EpiUnderweight, 
                    EpiHealthyWeight, EpiOverweight, EpiObese, 
                    EpiOverweightAndObese)) %>%
  filter(!(CA2011 %in% c("0", "Total"))) %>%
  group_by(SchoolYear, CA2011, SIMD, SIMDVersion) %>%
  summarise_all(sum) %>%
  ungroup()

haven::write_sav(la_simd_epi_file, file.path(open_data_folder, "OD_P1BMI_LA_SIMD_Epi.sav"), compress = FALSE)
write_csv(la_simd_epi_file, file.path(open_data_folder, "OD_P1BMI_LA_SIMD_Epi.csv"))

### 7 - Board by gender (clinical) ----
hb_gender_clin_file <- haven::read_sav(file.path(open_data_folder, "P1BMI_agg.sav")) %>% 
  subset(select = c(SchoolYear, HBR2014, Sex, ValidReviews, ClinUnderweight, 
                    ClinHealthyWeight, ClinOverweight, ClinObese, 
                    ClinSeverelyObese, ClinOverweightObeseAndSeverelyObese, 
                    ClinObeseAndSeverelyObese)) %>%
  filter(!(HBR2014 %in% c("0", "Total"))) %>%
  group_by(SchoolYear, HBR2014, Sex) %>%
  summarise_all(sum) %>%
  ungroup()

haven::write_sav(hb_gender_clin_file, file.path(open_data_folder, "OD_P1BMI_HB_Gender_Clin.sav"), compress = FALSE)
write_csv(hb_gender_clin_file, file.path(open_data_folder, "OD_P1BMI_HB_Gender_Clin.csv"))

### 8 - Local Authority by gender (clinical) ----
la_gender_clin_file <- haven::read_sav(file.path(open_data_folder, "P1BMI_agg.sav")) %>% 
  subset(select = c(SchoolYear, CA2011, Sex, ValidReviews, ClinUnderweight, 
                    ClinHealthyWeight, ClinOverweight, ClinObese, 
                    ClinSeverelyObese, ClinOverweightObeseAndSeverelyObese, 
                    ClinObeseAndSeverelyObese)) %>%
  filter(!(CA2011 %in% c("0", "Total"))) %>%
  group_by(SchoolYear, CA2011, Sex) %>%
  summarise_all(sum) %>%
  ungroup()

haven::write_sav(la_gender_clin_file, file.path(open_data_folder, "OD_P1BMI_LA_Gender_Clin.sav"), compress = FALSE)
write_csv(la_gender_clin_file, file.path(open_data_folder, "OD_P1BMI_LA_Gender_Clin.csv"))

### 9 - Board by SIMD (clinical) ----
hb_simd_clin_file <- haven::read_sav(file.path(open_data_folder, "P1BMI_agg.sav")) %>% 
  subset(select = c(SchoolYear, HBR2014, SIMD, SIMDVersion, ValidReviews, 
                    ClinUnderweight, ClinHealthyWeight, ClinOverweight, 
                    ClinObese, ClinSeverelyObese, 
                    ClinOverweightObeseAndSeverelyObese, ClinObeseAndSeverelyObese)) %>%
  filter(!(HBR2014 %in% c("0", "Total"))) %>%
  group_by(SchoolYear, HBR2014, SIMD, SIMDVersion) %>%
  summarise_all(sum) %>%
  ungroup()

haven::write_sav(hb_simd_clin_file, file.path(open_data_folder, "OD_P1BMI_HB_SIMD_Clin.sav"), compress = FALSE)
write_csv(hb_simd_clin_file, file.path(open_data_folder, "OD_P1BMI_HB_SIMD_Clin.csv"))

### - 10 Local Authority by SIMD (clinical) ----
la_simd_clin_file <- haven::read_sav(file.path(open_data_folder, "P1BMI_agg.sav")) %>% 
  subset(select = c(SchoolYear, CA2011, SIMD, SIMDVersion, ValidReviews, 
                    ClinUnderweight, ClinHealthyWeight, ClinOverweight, 
                    ClinObese, ClinSeverelyObese, 
                    ClinOverweightObeseAndSeverelyObese, ClinObeseAndSeverelyObese)) %>%
  filter(!(CA2011 %in% c("0", "Total"))) %>%
  group_by(SchoolYear, CA2011, SIMD, SIMDVersion) %>%
  summarise_all(sum) %>%
  ungroup()

haven::write_sav(la_simd_clin_file, file.path(open_data_folder, "OD_P1BMI_LA_SIMD_Clin.sav"), compress = FALSE)
write_csv(la_simd_clin_file, file.path(open_data_folder, "OD_P1BMI_LA_SIMD_Clin.csv"))

### - 11 NHS Board level - epidemiological file with confidence intervals ----
hb_epi_file <- read_csv(file.path(host_folder, "Output", "hb_data.csv")) %>% 
  mutate(SchoolYear = paste0("20", substr(schlyr_exam,1,2), "/", substr(schlyr_exam,3,4))) %>%
  dplyr::rename(ValidReviews = tot, EpiUnderweight = cent_grp1, 
                LCIEpiUnderweight = undw_lci, UCIEpiUnderweight = undw_uci, 
                EpiHealthyWeight = cent_grp2, LCIEpiHealthyWeight = hw_lci, 
                UCIEpiHealthyWeight = hw_uci, EpiOverweight = cent_grp3, 
                LCIEpiOverweight = over_lci, UCIEpiOverweight = over_uci, 
                EpiObese = cent_grp4, LCIEpiObese = obe_lci, 
                UCIEpiObese = obe_uci, EpiOverweightAndObese = cent_grp5, 
                LCIEpiOverweightAndObese = overobe_lci, 
                UCIEpiOverweightAndObese = overobe_uci) %>%
  subset(select = c(SchoolYear, HBR2014, ValidReviews, EpiUnderweight, 
                    LCIEpiUnderweight, UCIEpiUnderweight, EpiHealthyWeight,
                    LCIEpiHealthyWeight, UCIEpiHealthyWeight,EpiOverweight, 
                    LCIEpiOverweight, UCIEpiOverweight, EpiObese, LCIEpiObese, 
                    UCIEpiObese, EpiOverweightAndObese, 
                    LCIEpiOverweightAndObese, UCIEpiOverweightAndObese)) %>%
  filter(!(HBR2014 %in% c("0", "Total"))) %>%
  
write_csv(hb_epi_file, file.path(open_data_folder, "OD_P1BMI_HB_Epi.csv"))


### - 12 NHS Board level - clinical file with confidence intervals ----
hb_clin_file <- read_csv(file.path(host_folder, "Output", "hb_data.csv")) %>% 
  mutate(SchoolYear = paste0("20", substr(schlyr_exam,1,2), "/", substr(schlyr_exam,3,4))) %>%
  dplyr::rename(ValidReviews = tot, 
                ClinUnderweight = clin_cent_grp1, 
                LCIClinUnderweight = clin_undw_lci, 
                UCIClinUnderweight = clin_undw_uci,
                ClinHealthyWeight = clin_cent_grp2, 
                LCIClinHealthyWeight = clin_hw_lci, 
                UCIClinHealthyWeight = clin_hw_uci,
                ClinOverweight = clin_cent_grp3, 
                LCIClinOverweight = clin_over_lci, 
                UCIClinOverweight = clin_over_uci,
                ClinObese = clin_cent_grp4, LCIClinObese = clin_obe_lci, 
                UCIClinObese = clin_obe_uci, ClinSeverelyObese = clin_cent_grp5, 
                LCIClinSeverelyObese = clin_sobe_lci, 
                UCIClinSeverelyObese = clin_sobe_uci, 
                ClinOverweightObeseAndSeverelyObese = clin_cent_grp6, 
                LCIClinOverweightObeseAndSeverelyObese = clin_overwplus_lci, 
                UCIClinOverweightObeseAndSeverelyObese = clin_overwplus_uci,
                ClinObeseAndSeverelyObese = clin_cent_grp7, 
                LCIClinObeseAndSeverelyObese = clin_obeplus_lci, 
                UCIClinObeseAndSeverelyObese = clin_obeplus_uci) %>%
  subset(select = c(SchoolYear, HBR2014, ValidReviews, 
                    ClinUnderweight, LCIClinUnderweight, UCIClinUnderweight,
                    ClinHealthyWeight, LCIClinHealthyWeight, UCIClinHealthyWeight,
                    ClinOverweight, LCIClinOverweight, UCIClinOverweight,
                    ClinObese, LCIClinObese, UCIClinObese,
                    ClinSeverelyObese, LCIClinSeverelyObese, UCIClinSeverelyObese,
                    ClinOverweightObeseAndSeverelyObese, LCIClinOverweightObeseAndSeverelyObese, UCIClinOverweightObeseAndSeverelyObese,
                    ClinObeseAndSeverelyObese, LCIClinObeseAndSeverelyObese, UCIClinObeseAndSeverelyObese)) %>%
  filter(!(HBR2014 %in% c("0", "Total"))) %>%
  
  write_csv(hb_epi_file, file.path(open_data_folder, "OD_P1BMI_HB_Clin.csv"))

### - 13 Local Authority level - epidemiological file with confidence intervals ----
la_epi_file <- read_csv(file.path(host_folder, "Output", "ca_data.csv")) %>% 
  mutate(SchoolYear = paste0("20", substr(schlyr_exam,1,2), "/", substr(schlyr_exam,3,4))) %>%
  dplyr::rename(CA2011 = CA2019, ValidReviews = tot, EpiUnderweight = cent_grp1, 
                LCIEpiUnderweight = undw_lci, UCIEpiUnderweight = undw_uci, 
                EpiHealthyWeight = cent_grp2, LCIEpiHealthyWeight = hw_lci, 
                UCIEpiHealthyWeight = hw_uci, EpiOverweight = cent_grp3, 
                LCIEpiOverweight = over_lci, UCIEpiOverweight = over_uci, 
                EpiObese = cent_grp4, LCIEpiObese = obe_lci, 
                UCIEpiObese = obe_uci, EpiOverweightAndObese = cent_grp5, 
                LCIEpiOverweightAndObese = overobe_lci, 
                UCIEpiOverweightAndObese = overobe_uci) %>%
  subset(select = c(SchoolYear, CA2011, ValidReviews, EpiUnderweight, 
                    LCIEpiUnderweight, UCIEpiUnderweight, EpiHealthyWeight,
                    LCIEpiHealthyWeight, UCIEpiHealthyWeight,EpiOverweight, 
                    LCIEpiOverweight, UCIEpiOverweight, EpiObese, LCIEpiObese, 
                    UCIEpiObese, EpiOverweightAndObese, 
                    LCIEpiOverweightAndObese, UCIEpiOverweightAndObese)) %>%
  filter(!(CA2011 %in% c("0", "Total"))) %>%
  
  write_csv(hb_epi_file, file.path(open_data_folder, "OD_P1BMI_LA_Epi.csv"))


### - 12 LA clinical file with confidence intervals ----
la_clin_file <- read_csv(file.path(host_folder, "Output", "ca_data.csv")) %>% 
  mutate(SchoolYear = paste0("20", substr(schlyr_exam,1,2), "/", substr(schlyr_exam,3,4))) %>%
  dplyr::rename(CA2011 = CA2019, ValidReviews = tot, 
                ClinUnderweight = clin_cent_grp1, 
                LCIClinUnderweight = clin_undw_lci, 
                UCIClinUnderweight = clin_undw_uci,
                ClinHealthyWeight = clin_cent_grp2, 
                LCIClinHealthyWeight = clin_hw_lci, 
                UCIClinHealthyWeight = clin_hw_uci,
                ClinOverweight = clin_cent_grp3, 
                LCIClinOverweight = clin_over_lci, 
                UCIClinOverweight = clin_over_uci,
                ClinObese = clin_cent_grp4, LCIClinObese = clin_obe_lci, 
                UCIClinObese = clin_obe_uci, ClinSeverelyObese = clin_cent_grp5, 
                LCIClinSeverelyObese = clin_sobe_lci, 
                UCIClinSeverelyObese = clin_sobe_uci, 
                ClinOverweightObeseAndSeverelyObese = clin_cent_grp6, 
                LCIClinOverweightObeseAndSeverelyObese = clin_overwplus_lci, 
                UCIClinOverweightObeseAndSeverelyObese = clin_overwplus_uci,
                ClinObeseAndSeverelyObese = clin_cent_grp7, 
                LCIClinObeseAndSeverelyObese = clin_obeplus_lci, 
                UCIClinObeseAndSeverelyObese = clin_obeplus_uci) %>%
  subset(select = c(SchoolYear, CA2011, ValidReviews, 
                    ClinUnderweight, LCIClinUnderweight, UCIClinUnderweight,
                    ClinHealthyWeight, LCIClinHealthyWeight, UCIClinHealthyWeight,
                    ClinOverweight, LCIClinOverweight, UCIClinOverweight,
                    ClinObese, LCIClinObese, UCIClinObese,
                    ClinSeverelyObese, LCIClinSeverelyObese, UCIClinSeverelyObese,
                    ClinOverweightObeseAndSeverelyObese, LCIClinOverweightObeseAndSeverelyObese, UCIClinOverweightObeseAndSeverelyObese,
                    ClinObeseAndSeverelyObese, LCIClinObeseAndSeverelyObese, UCIClinObeseAndSeverelyObese)) %>%
  filter(!(CA2011 %in% c("0", "Total"))) %>%
  
  write_csv(hb_epi_file, file.path(open_data_folder, "OD_P1BMI_LA_Clin.csv"))



### End of script
