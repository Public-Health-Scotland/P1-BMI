### functions for primary 1 bmi publication ----

## Select relevant years function
apply_hb_year <- function(x, HB, ey, cy) {
  
  x <- x %>%
    filter((hb2019_cypher == HB & as.numeric(schlyr_exam) >= ey & 
              as.numeric(schlyr_exam) <= cy) | hb2019_cypher != HB)
  
  return(x)
  
}


## Create single cypher HB variable
# create a new variable in order to keep the HB codes
apply_hb2019_cypher <- function(df) {
  df <- df %>%
    mutate(hb2019_cypher = case_when(df$HB2019 == "S08000015" ~ "A",
                                     df$HB2019 == "S08000016" ~ "B",
                                     df$HB2019 == "S08000017" ~ "Y",
                                     df$HB2019 == "S08000019" ~ "V",
                                     df$HB2019 == "S08000020" ~ "N",
                                     df$HB2019 == "S08000031" ~ "G",
                                     df$HB2019 == "S08000022" ~ "H",
                                     df$HB2019 == "S08000032" ~ "L",
                                     df$HB2019 == "S08000024" ~ "S",
                                     df$HB2019 == "S08000025" ~ "R",
                                     df$HB2019 == "S08000026" ~ "Z",
                                     df$HB2019 == "S08000028" ~ "W",
                                     df$HB2019 == "S08000029" ~ "F",
                                     df$HB2019 == "S08000030" ~ "T"))
  return(df)
}

# function to calculate confidence intervals
apply_ci_calculation <- function(df) {
  df <- df %>%
    mutate(pcf = (pop - tot)/(pop - 1),
           num_epi_undw = cent_grp1,
           num_epi_hw = cent_grp2,
           num_epi_over = cent_grp3,
           num_epi_obe = cent_grp4,
           num_epi_overobe = cent_grp5,
           num_clin_undw = clin_cent_grp1,
           num_clin_hw = clin_cent_grp2,
           num_clin_over = clin_cent_grp3,
           num_clin_obe = clin_cent_grp4,
           num_clin_sobe = clin_cent_grp5,
           num_clin_overwplus = clin_cent_grp6,
           num_clin_obeplus = clin_cent_grp7)

  for (weight_group in c("epi_undw", "epi_hw", "epi_over", "epi_obe", 
                         "epi_overobe", "clin_undw", "clin_hw", "clin_over",
                         "clin_obe", "clin_sobe", "clin_overwplus",
                         "clin_obeplus")) {
    df <-
      df %>% mutate(
        !!paste0("p_", weight_group) := get(paste0("num_", weight_group)) /
          tot,
        !!paste0("q_", weight_group) := (1 - get(paste0(
          "p_", weight_group
        ))),
        !!paste0(weight_group, "_bmi") := get(paste0("num_", weight_group)) /
          tot,
        !!paste0(weight_group, "_lci") := (get(paste0(
          "p_", weight_group
        )) - 1.96 * sqrt(((
          get(paste0("p_", weight_group)) * get(paste0("q_", weight_group))
        ) / tot) * (pcf))),
        !!paste0(weight_group, "_uci") := (get(paste0(
          "p_", weight_group
        )) + 1.96 * sqrt(((
          get(paste0("p_", weight_group)) * get(paste0("q_", weight_group))
        ) / tot) * (pcf)))
      )
  }
  return(df)
}

# function to calculate pecentages for each weight category
apply_percentage_calc <- function(df) {
  df <- df %>%
    mutate(per_epi_undw = num_epi_undw/total_reviews,
           per_epi_hw = num_epi_hw/total_reviews,
           per_epi_over = num_epi_over/total_reviews,
           per_epi_obe = num_epi_obe/total_reviews,
           per_epi_overobe = num_epi_overobe/total_reviews,
           per_clin_undw = num_clin_undw/total_reviews,
           per_clin_hw = num_clin_hw/total_reviews,
           per_clin_over = num_clin_over/total_reviews,
           per_clin_obe = num_clin_obe/total_reviews,
           per_clin_sobe = num_clin_sobe/total_reviews,
           per_clin_overwplus = num_clin_overwplus/total_reviews,
           per_clin_obeplus = num_clin_obeplus/total_reviews)
  return(df)
}


# function to filter population estimate data
apply_pop_est_filter <- function(df) {
  df <- df %>% 
    filter(age == 5) %>%
    filter(year >= 2001 & year <=2018) %>% 
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
  return(df)
}


### Open data functions ----

# function to change the format of school year e.g. "0102" to "2001/02"
apply_school_year_format <- function(df) {
  df <- df %>% 
    mutate(SchoolYear = case_when(SchoolYear == "0102" ~ "2001/02",
                                  SchoolYear == "0203" ~ "2002/03",
                                  SchoolYear == "0304" ~ "2003/04",
                                  SchoolYear == "0405" ~ "2004/05",
                                  SchoolYear == "0506" ~ "2005/06",
                                  SchoolYear == "0607" ~ "2006/07",
                                  SchoolYear == "0708" ~ "2007/08",
                                  SchoolYear == "0809" ~ "2008/09",
                                  SchoolYear == "0910" ~ "2009/10",
                                  SchoolYear == "1011" ~ "2010/11",
                                  SchoolYear == "1112" ~ "2011/12",
                                  SchoolYear == "1213" ~ "2012/13",
                                  SchoolYear == "1314" ~ "2013/14",
                                  SchoolYear == "1415" ~ "2014/15",
                                  SchoolYear == "1516" ~ "2015/16",
                                  SchoolYear == "1617" ~ "2016/17",
                                  SchoolYear == "1718" ~ "2017/18",
                                  SchoolYear == "1819" ~ "2018/19"))
  return(df)
}


# function to create SIMDVersion variable
apply_simd_version <- function(df) {
  df <- df %>%
    mutate(SIMDVersion = case_when(
      SchoolYear %in% c("2001/02", "2002/03", "2003/04") ~ "SIMD2004",
      SchoolYear %in% c("2004/05", "2005/06", "2006/07") ~ "SIMD2006",
      SchoolYear %in% c("2007/08", "2008/09", "2009/10") ~ "SIMD2009",
      SchoolYear %in% c("2010/11", "2011/12", "2012/13", "2013/14") ~ "SIMD2012",
      SchoolYear %in% c("2014/15", "2015/16", "2016/17", "2017/18",
                        "2018/19") ~ "SIMD2016"))
  return(df)
}


### Tableau data functions ----
apply_year_ending_variable <- function(df) {
  df <- df %>% 
    mutate(YearEnding = case_when(SchoolYear == "2001/02" ~ "30/06/2002",
                                  SchoolYear == "2002/03" ~ "30/06/2003",
                                  SchoolYear == "2003/04" ~ "30/06/2004",
                                  SchoolYear == "2004/05" ~ "30/06/2005",
                                  SchoolYear == "2005/06" ~ "30/06/2006",
                                  SchoolYear == "2006/07" ~ "30/06/2007",
                                  SchoolYear == "2007/08" ~ "30/06/2008",
                                  SchoolYear == "2008/09" ~ "30/06/2009",
                                  SchoolYear == "2009/10" ~ "30/06/2010",
                                  SchoolYear == "2010/11" ~ "30/06/2011",
                                  SchoolYear == "2011/12" ~ "30/06/2012",
                                  SchoolYear == "2012/13" ~ "30/06/2013",
                                  SchoolYear == "2013/14" ~ "30/06/2014",
                                  SchoolYear == "2014/15" ~ "30/06/2015",
                                  SchoolYear == "2015/16" ~ "30/06/2016",
                                  SchoolYear == "2016/17" ~ "30/06/2017",
                                  SchoolYear == "2017/18" ~ "30/06/2018",
                                  SchoolYear == "2018/19" ~ "30/06/2019"))
  return(df)
}

# add a new function to create flag for excluding ca's with less than 50 records
apply_ca_exclusion <- function(df) {
  df <- df %>% 
    mutate(flag = case_when(
      (CA2019 == "S12000036") &
        (SchoolYear %in% c("2001/02","2002/03","2003/04")) ~ 1,
      (CA2019 == "S12000045") &
        (SchoolYear %in% c("2006/07","2007/08","2008/09","2010/11",
                           "2016/17")) ~ 1,
      (CA2019 == "S12000010") &
        (SchoolYear %in% c("2001/02","2003/04")) ~ 1,
      (CA2019 == "S12000011") &
        (SchoolYear %in% c("2006/07","2007/08","2008/09","2010/11")) ~ 1,
      (CA2019 == "S12000049") &
        (SchoolYear %in% c("2006/07","2007/08","2008/09","2010/11")) ~ 1,
      (CA2019 == "S12000018") &
        (SchoolYear %in% c("2006/07","2007/08","2008/09","2009/10",
                           "2010/11")) ~ 1,
      (CA2019 == "S12000019") &
        (SchoolYear %in% c("2003/04")) ~ 1,
      (CA2019 == "S12000013") &
        (SchoolYear %in% c("2004/05")) ~ 1,
      (CA2019 == "S12000039") &
        (SchoolYear %in% c("2006/07","2008/09","2010/11")) ~ 1,
      TRUE ~ 0))
  return(df)
}


#### END OF SCRIPT ### ----