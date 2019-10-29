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

# function to calculate pecentages for each category
apply_percentage_calc <- function(df) {
  df <- df %>%
    mutate(per_epi_undw = num_epi_undw/tot,
           per_epi_hw = num_epi_hw/tot,
           per_epi_over = num_epi_over/tot,
           per_epi_obe = num_epi_obe/tot,
           per_epi_overobe = num_epi_overobe/tot,
           per_clin_undw = num_clin_undw/tot,
           per_clin_hw = num_clin_hw/tot,
           per_clin_over = num_clin_over/tot,
           per_clin_obe = num_clin_obe/tot,
           per_clin_sobe = num_clin_sobe/tot,
           per_clin_overwplus = num_clin_overwplus/tot,
           per_clin_obeplus = num_clin_obeplus/tot)
  return(df)
}


