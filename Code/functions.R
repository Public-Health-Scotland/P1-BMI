#added this line


### ---- Council Area Recode ----
apply_ca_name <-function(council_area){
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
                                       TRUE ~ "Other"))}


### ---- hbres Recode ----
apply_hbres_name <- function(hbres) {
  mutate(hbres_name = case_when(hbres == "S08000015" ~ "NHS Ayrshire & Arran",
                                hbres == "S08000016" ~ "NHS Borders", 
                                hbres == "S08000017" ~ "NHS Dumfries & Galloway",
                                hbres == "S08000018" ~ "NHS Fife",
                                hbres == "S08000019" ~ "NHS Forth Valley", 
                                hbres == "S08000020" ~ "NHS Grampian",
                                hbres == "S08000021" ~ "NHS Greater Glasgow & Clyde",
                                hbres == "S08000022" ~ "NHS Highland", 
                                hbres == "S08000023" ~ "NHS Lanarkshire",
                                hbres == "S08000024" ~ "NHS Lothian",
                                hbres == "S08000025" ~ "NHS Orkney", 
                                hbres == "S08000026" ~ "NHS Shetland", 
                                hbres == "S08000027" ~ "NHS Tayside", 
                                hbres == "S08000028" ~ "NHS Western Isles", 
                                TRUE ~ "Other"))}

# create a numeric var for hbres to make sorting easier
apply_hbres_num <- function(hbres_name) {
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
                               TRUE ~ 99))}


# Lower confidence limits.
bmiData <- mutate(# Lower confidence limits.
                  lci_undw = (p_undw - 1.96 * sqrt(((p_undw * q_undw)/tot) * (pcf))) * 100,
                  lci_hw = (p_hw - 1.96 * sqrt(((p_hw * q_hw)/tot) * (pcf))) * 100,
                  lci_over = (p_over - 1.96 * sqrt(((p_over * q_over)/tot) * (pcf))) * 100, 
                  lci_obe = (p_obe - 1.96 * sqrt(((p_obe * q_obe)/tot) * (pcf))) * 100, 
                  lci_overobe = (p_overobe - 1.96 * sqrt(((p_overobe * q_overobe)/tot) * (pcf))) * 100, 
                  lci_clin_undw = (p_clin_undw - 1.96 * sqrt(((p_clin_undw * q_clin_undw)/tot) * (pcf))) * 100, 
                  lci_clin_hw = (p_clin_hw - 1.96 * sqrt(((p_clin_hw * q_clin_hw)/tot) * (pcf))) * 100, 
                  lci_clin_over = (p_clin_over - 1.96 * sqrt(((p_clin_over * q_clin_over)/tot) * (pcf))) * 100, 
                  lci_clin_obe = (p_clin_obe - 1.96 * sqrt(((p_clin_obe * q_clin_obe)/tot) * (pcf))) * 100, 
                  lci_clin_sobe = (p_clin_sobe - 1.96 * sqrt(((p_clin_sobe * q_clin_sobe)/tot) * (pcf))) * 100,
                  lci_clin_overwplus = (p_clin_overwplus - 1.96 * sqrt(((p_clin_overwplus * q_clin_overwplus)/tot) * (pcf))) * 100, 
                  lci_clin_obeplus = (p_clin_obeplus - 1.96 * sqrt(((p_clin_obeplus * q_clin_obeplus)/tot) * (pcf))) * 100, 
                  # Upper confidence limits.
                  uci_undw = (p_undw + 1.96 * sqrt(((p_undw * q_undw)/tot) * (pcf))) * 100, 
                  uci_hw = (p_hw + 1.96 * sqrt(((p_hw * q_hw)/tot) * (pcf))) * 100, 
                  uci_over = (p_over + 1.96 * sqrt(((p_over * q_over)/tot) * (pcf))) * 100, 
                  uci_obe = (p_obe + 1.96 * sqrt(((p_obe * q_obe)/tot) * (pcf))) * 100, 
                  uci_overobe = (p_overobe + 1.96 * sqrt(((p_overobe * q_overobe)/tot) * (pcf))) * 100, 
                  uci_clin_undw = (p_clin_undw + 1.96 * sqrt(((p_clin_undw * q_clin_undw)/tot) * (pcf))) * 100, 
                  uci_clin_hw = (p_clin_hw + 1.96 * sqrt(((p_clin_hw * q_clin_hw)/tot) * (pcf))) * 100, 
                  uci_clin_over = (p_clin_over + 1.96 * sqrt(((p_clin_over * q_clin_over)/tot) * (pcf))) * 100, 
                  uci_clin_obe = (p_clin_obe + 1.96 * sqrt(((p_clin_obe * q_clin_obe)/tot) * (pcf))) * 100, 
                  uci_clin_sobe = (p_clin_sobe + 1.96 * sqrt(((p_clin_sobe * q_clin_sobe)/tot) * (pcf))) * 100, 
                  uci_clin_overwplus = (p_clin_overwplus + 1.96 * sqrt(((p_clin_overwplus * q_clin_overwplus)/tot) * (pcf))) * 100, 
                  uci_clin_obeplus = (p_clin_obeplus + 1.96 * sqrt(((p_clin_obeplus * q_clin_obeplus)/tot) * (pcf))) * 100)


