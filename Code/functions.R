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
    mutate(HB2019_cypher = case_when(data$HB2019 == "S08000015" ~ "A",
                                     data$HB2019 == "S08000016" ~ "B",
                                     data$HB2019 == "S08000017" ~ "Y",
                                     data$HB2019 == "S08000019" ~ "V",
                                     data$HB2019 == "S08000020" ~ "N",
                                     data$HB2019 == "S08000021" ~ "G",
                                     data$HB2019 == "S08000022" ~ "H",
                                     data$HB2019 == "S08000023" ~ "L",
                                     data$HB2019 == "S08000024" ~ "S",
                                     data$HB2019 == "S08000025" ~ "R",
                                     data$HB2019 == "S08000026" ~ "Z",
                                     data$HB2019 == "S08000028" ~ "W",
                                     data$HB2019 == "S08000029" ~ "F",
                                     data$HB2019 == "S08000030" ~ "T"))
  return(data)
}