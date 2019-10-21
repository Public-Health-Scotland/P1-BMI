### Select relevant years function
apply_hb_year <- function(df, HB, ey, cy = currentYr) {
  df <- df %>%
    subset(!(df$HB2019 == HB &
               as.numeric(schlyr_exam) <= ey &
               as.numeric(schlyr_exam) >= cy))
}

### Recode HB variable to single cypher function
apply_hb_cypher <- function(df) {
  df <- df %>%
    mutate(HB2019_cypher = case_when(df$HB2019 == "S08000015" ~ "A",
                                     df$HB2019 == "S08000016" ~ "B",
                                     df$HB2019 == "S08000017" ~ "Y",
                                     df$HB2019 == "S08000019" ~ "V",
                                     df$HB2019 == "S08000020" ~ "N",
                                     df$HB2019 == "S08000021" ~ "G",
                                     df$HB2019 == "S08000022" ~ "H",
                                     df$HB2019 == "S08000023" ~ "L",
                                     df$HB2019 == "S08000024" ~ "S",
                                     df$HB2019 == "S08000025" ~ "R",
                                     df$HB2019 == "S08000026" ~ "Z",
                                     df$HB2019 == "S08000028" ~ "W",
                                     df$HB2019 == "S08000029" ~ "F",
                                     df$HB2019 == "S08000030" ~ "T"))
  return(df)
}

calculate_ci <- function(df) {
  df <- df %>%
    mutate(pcf = (pop - tot)/(pop - 1),
           n_undw = cent_grp1,
           n_hw = cent_grp2,
           n_over = cent_grp3,
           n_obe = cent_grp4,
           n_overobe = cent_grp5,
           n_clin_undw = clin_cent_grp1,
           n_clin_hw = clin_cent_grp2,
           n_clin_over = clin_cent_grp3,
           n_clin_obe = clin_cent_grp4,
           n_clin_sobe = clin_cent_grp5,
           n_clin_overwplus = clin_cent_grp6,
           n_clin_obeplus = clin_cent_grp7)

  for (weight_group_epi in c("undw", "hw", "over", "obe", "overobe")) {
    df <-
      df %>% mutate(
        !!paste0("p_", weight_group_epi) := get(paste0("n_", weight_group_epi)) /
          tot,
        !!paste0("q_", weight_group_epi) := (1 - get(paste0(
          "p_", weight_group_epi
        ))),
        !!paste0(weight_group_epi, "_bmi") := get(paste0("n_", weight_group_epi)) /
          tot,
        !!paste0(weight_group_epi, "_lci") := (get(paste0(
          "p_", weight_group_epi
        )) - 1.96 * sqrt(((
          get(paste0("p_", weight_group_epi)) * get(paste0("q_", weight_group_epi))
        ) / tot) * (pcf))),
        !!paste0(weight_group_epi, "_uci") := (get(paste0(
          "p_", weight_group_epi
        )) + 1.96 * sqrt(((
          get(paste0("p_", weight_group_epi)) * get(paste0("q_", weight_group_epi))
        ) / tot) * (pcf)))
      )
  }
  return(df)
}

# Assign the appropriate SIMD value to a patient depending on the year they
# were admitted
apply_simd <- function(df) {
  df <- df %>%  mutate(simd = case_when(
    year >= 2014 ~ simd2016_sc_quintile,
    year >= 2010 & year <= 2013 ~ simd2012_sc_quintile,
    year >= 2007 & year <= 2009 ~ simd2009v2_sc_quintile,
    year >= 2004 & year <= 2006 ~ simd2006_sc_quintile,
    year <= 2003 ~ simd2004_sc_quintile
  ))
  return(df)
}

# Postcode lookups for SIMD 2016, 2012 and 2009
# These files will be combined, so create a year variable in each one, to allow
# them to be differentiated from one another
simd_2016 <- read_spss(paste0(plat_filepath,
                              "lookups/Unicode/Deprivation",
                              "/postcode_2019_1.5_simd2016.sav")) %>%
  select(pc7, simd2016_sc_quintile) %>%
  rename(postcode = pc7,
         simd = simd2016_sc_quintile) %>%
  mutate(year = "simd_2016")

simd_2012 <- read_spss(paste0(plat_filepath,
                              "lookups/Unicode/Deprivation/",
                              "postcode_2016_1_simd2012.sav")) %>%
  select(pc7, simd2012_sc_quintile) %>%
  rename(postcode = pc7,
         simd = simd2012_sc_quintile) %>%
  mutate(year = "simd_2012")

simd_2009 <- read_spss(paste0(plat_filepath,
                              "lookups/Unicode/Deprivation/",
                              "postcode_2012_2_simd2009v2.sav")) %>%
  select(PC7, simd2009v2_sc_quintile) %>%
  rename(postcode = PC7,
         simd = simd2009v2_sc_quintile) %>%
  mutate(year = "simd_2009")

### 7 - Council Area Recode ----
apply_ca_desc <- function(df) {
  df <- df %>%
    mutate(council_area_desc = case_when(CA2019 == "S12000033" ~ "Aberdeen City",
                                         CA2019 == "S12000034" ~ "Aberdeenshire",
                                         CA2019 == "S12000041" ~ "Angus",
                                         CA2019 == "S12000035" ~ "Argyll & Bute",
                                         CA2019 == "S12000026" ~ "Scottish Borders",
                                         CA2019 == "S12000005" ~ "Clackmannanshire",
                                         CA2019 == "S12000039" ~ "West Dunbartonshire",
                                         CA2019 == "S12000006" ~ "Dumfries & Galloway",
                                         CA2019 == "S12000042" ~ "Dundee City",
                                         CA2019 == "S12000008" ~ "East Ayrshire",
                                         CA2019 == "S12000045" ~ "East Dunbartonshire",
                                         CA2019 == "S12000010" ~ "East Lothian",
                                         CA2019 == "S12000011" ~ "East Renfrewshire",
                                         CA2019 == "S12000036" ~ "City of Edinburgh",
                                         CA2019 == "S12000014" ~ "Falkirk",
                                         CA2019 == "S12000047" ~ "Fife",
                                         CA2019 == "S12000046" ~ "Glasgow City",
                                         CA2019 == "S12000017" ~ "Highland",
                                         CA2019 == "S12000018" ~ "Inverclyde",
                                         CA2019 == "S12000019" ~ "Midlothian",
                                         CA2019 == "S12000020" ~ "Moray",
                                         CA2019 == "S12000021" ~ "North Ayrshire",
                                         CA2019 == "S12000044" ~ "North Lanarkshire",
                                         CA2019 == "S12000023" ~ "Orkney Islands",
                                         CA2019 == "S12000048" ~ "Perth & Kinross",
                                         CA2019 == "S12000038" ~ "Renfrewshire",
                                         CA2019 == "S12000027" ~ "Shetland Islands",
                                         CA2019 == "S12000028" ~ "South Ayrshire",
                                         CA2019 == "S12000029" ~ "South Lanarkshire",
                                         CA2019 == "S12000030" ~ "Stirling",
                                         CA2019 == "S12000040" ~ "West Lothian",
                                         CA2019 == "S12000013" ~ "Na h-Eileanan Siar",
                                         TRUE ~ "Other"))
  return(df)
}

### 7 - Council Area Recode ----
apply_ca_cypher_desc <- function(df) {
  df <- df %>%
    mutate(council_area_name = case_when(council_area == 1 ~ "Aberdeen City",
                                         council_area == 2 ~ "Aberdeenshire",
                                         council_area == 3 ~ "Angus", 
                                         council_area == 4 ~ "Argyll & Bute",
                                         council_area == 5 ~ "Scottish Borders", 
                                         council_area == 6 ~ "Clackmannanshire",
                                         council_area == 7 ~ "West Dunbartonshire", 
                                         council_area == 8 ~ "Dumfries & Galloway",
                                         council_area == 9 ~ "Dundee City", 
                                         council_area == 10 ~ "East Ayrshire",
                                         council_area == 11 ~ "East Dunbartonshire", 
                                         council_area == 12 ~ "East Lothian",
                                         council_area == 13 ~ "East Renfrewshire", 
                                         council_area == 14 ~ "City of Edinburgh",
                                         council_area == 15 ~ "Falkirk",
                                         council_area == 16 ~ "Fife",
                                         council_area == 17 ~ "Glasgow City", 
                                         council_area == 18 ~ "Highland",
                                         council_area == 19 ~ "Inverclyde",
                                         council_area == 20 ~ "Midlothian",
                                         council_area == 21 ~ "Moray",
                                         council_area == 22 ~ "North Ayrshire",
                                         council_area == 23 ~ "North Lanarkshire", 
                                         council_area == 24 ~ "Orkney Islands",
                                         council_area == 25 ~ "Perth & Kinross",
                                         council_area == 26 ~ "Renfrewshire",
                                         council_area == 27 ~ "Shetland Islands",
                                         council_area == 28 ~ "South Ayrshire",
                                         council_area == 29 ~ "South Lanarkshire",
                                         council_area == 30 ~ "Stirling",
                                         council_area == 31 ~ "West Lothian",
                                         council_area == 32 ~ "Comhairle nan Eilean Siar",
                                         TRUE ~ "Other"))
  return(df)
}

# hb res
apply_hb_desc <- function(df) {
  df <- df %>%
    mutate(hbres_name = case_when(HB2019 == "S08000015" ~ "NHS Ayrshire & Arran",
                                  HB2019 == "S08000016" ~ "NHS Borders", 
                                  HB2019 == "S08000017" ~ "NHS Dumfries & Galloway",
                                  HB2019 == "S08000018" ~ "NHS Fife",
                                  HB2019 == "S08000019" ~ "NHS Forth Valley", 
                                  HB2019 == "S08000020" ~ "NHS Grampian",
                                  HB2019 == "S08000021" ~ "NHS Greater Glasgow & Clyde",
                                  HB2019 == "S08000022" ~ "NHS Highland", 
                                  HB2019 == "S08000023" ~ "NHS Lanarkshire",
                                  HB2019 == "S08000024" ~ "NHS Lothian",
                                  HB2019 == "S08000025" ~ "NHS Orkney", 
                                  HB2019 == "S08000026" ~ "NHS Shetland", 
                                  HB2019 == "S08000027" ~ "NHS Tayside", 
                                  HB2019 == "S08000028" ~ "NHS Western Isles", 
                                  TRUE ~ "Other"))
  return(df)
}

# create a numeric var for hb res to make sorting easier
apply_hb_cypher <- function(df) {
  df <- df %>%
    mutate(hbres_num = case_when(hbres_name == "NHS Ayrshire & Arran" ~ 1,
                                 hbres_name == "NHS Borders" ~ 2, 
                                 hbres_name == "NHS Dumfries & Galloway" ~ 3, 
                                 hbres_name == "NHS Fife" ~ 4,
                                 hbres_name == "NHS Forth Valley" ~ 5,
                                 hbres_name == "NHS Grampian" ~ 6,
                                 hbres_name == "NHS Greater Glasgow & Clyde" ~ 7,
                                 hbres_name == "NHS Highland" ~ 8,
                                 hbres_name == "NHS Lanarkshire" ~ 9,
                                 hbres_name == "NHS Lothian" ~ 10,
                                 hbres_name == "NHS Orkney" ~ 11,       
                                 hbres_name == "NHS Shetland" ~ 12,       
                                 hbres_name == "NHS Tayside" ~ 13, 
                                 hbres_name == "NHS Western Isles" ~ 14,         
                                 TRUE ~ 99))
  return(df)
}
