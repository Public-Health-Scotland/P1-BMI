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

calculate_ci <- function(df, weight_group = c("undw", "hw", "over", "obe", "overobe")){
  mutate_if(weight_group == "undw")(n = cent_grp1, p = n/tot, q = (1-p))
  mutate_if(weight_group == "hw")(n = cent_grp1, p = n/tot, q = (1-p))
  mutate_if(weight_group == "over")(n = cent_grp1, p = n/tot, q = (1-p))
  mutate_if(weight_group == "obe")(n = cent_grp1, p = n/tot, q = (1-p))
  mutate_if(weight_group == "overobe")(n = cent_grp1, p = n/tot, q = (1-p))
  #does this work???
  mutate(assign(paste(weight_group, "_bmi",sep = ""))) = (n/tot)
  mutate(assign(paste(weight_group, "_lci",sep = ""))) = (p - 1.96 * sqrt(((p * q)/tot) * (pcf)))
  mutate(assign(paste(weight_group, "_uci",sep = ""))) = (p + 1.96 * sqrt(((p * q)/tot) * (pcf)))
}