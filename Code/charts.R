### -- Figure 2 - line chart 
#Percentage of Primary 1 children at risk of overweight and obesity, and
#underweight (epidemiological categories), by gender, school years 2001/02 to 2017/18,
#all participating NHS Boards/Scotland

## Install Packages
install.packages("readxl")
install.packages("here")
install.packages("readr")
install.packages("dplyr")
install.packages("tidyr")
install.packages("haven")
install.packages("lubridate")
install.packages("ggplot2")



library(haven)    # used for importing SPSS files
library(dplyr)
library(lubridate)
library(readxl)
library(here)
library(readr)
library(tidyr)
library(ggplot2)

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
figure_two_data <- readRDS(paste(file.path(host_folder, "BMI_data_0102_1718.rds"))) 

figure_two_data <- subset(figure_two_data, 
                          select = c(sex, schlyr_exam, tot, cent_grp1, cent_grp5)) %>%
  group_by(schlyr_exam,sex) %>%
  summarise(Epi_OverweightObese = sum(cent_grp5),Epi_Underweight=sum(cent_grp1),N_Valid_Height_Weight=sum(tot)) %>%
  ungroup() %>% 
  mutate(OvOb_Perc=Epi_OverweightObese/N_Valid_Height_Weight*100,Und_Perc=Epi_Underweight/N_Valid_Height_Weight*100) %>%
  gather(Epi_Category,Percentage,OvOb_Perc:Und_Perc) %>%
  subset(select=c(schlyr_exam,sex,Epi_Category,Percentage)) %>%
  Perc2=paste(sex,Epi_Category) %>%

figure_two <- ggplot(data = figure_two_data, aes(x = school_year, 
                                                y = Percentage)) +
  geom_bar(position = "dodge", stat = "identity", width = 0.5, show.legend = T) + 
  coord_flip() +
  theme(panel.background = element_blank(),
        panel.grid.major.x = element_blank(), 
        panel.grid.major.y = element_line(size = .1, color = "#C0C0C0"),
        axis.title.x = element_text(size = 12, face = "bold"),
        axis.title.y = element_text(size = 12, face = "bold"),
        axis.text = element_text(size = 12)) +
  
  xlab("Percentage") + ylab("School Year")


scale_x_discrete(limits = c("Other", "All participating boards", 
                            "NHS Ayrshire & Arran", "NHS Borders", 
                            "NHS Dumfries & Galloway", "NHS Fife", 
                            "NHS Forth Valley", "NHS Grampian", 
                            "NHS Greater Glasgow & Clyde", "NHS Highland", 
                            "NHS Lanarkshire", "NHS Lothian", "NHS Orkney", 
                            "NHS Shetland", "NHS Tayside", "NHS Western Isles")) +
  scale_fill_manual(values = c("#0391BF", "#0391BF", "#0391BF", "#0391BF", "#0391BF", 
                               "#0391BF", "#0391BF", "#0391BF", "#0391BF", 
                               "#0391BF", "#0391BF", "#0391BF", "#0391BF", 
                               "#092869", "#0391BF", "#0391BF", "#0391BF", "#0391BF")) +
ggsave("figure_two_ggplot.png", plot = figure_two, width = 17.79, height = 14.24, units = "cm", 
       device = "png", dpi = 600)
figure_two

