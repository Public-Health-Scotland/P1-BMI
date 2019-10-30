### -- Figure 2 - line chart 
#Percentage of Primary 1 children at risk of overweight and obesity, and
#underweight (epidemiological categories), by gender, school years 2001/02 to 2018/19,
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
figure_two_data <- readRDS(paste(file.path(host_folder, "BMI_data_0102_1718.rds"))) %>%
  subset(select = c(sex, schlyr_exam, tot, cent_grp1, cent_grp5)) %>%
  group_by(sex, schlyr_exam) %>%
  summarise(epi_overweightobese=sum(cent_grp5),epi_underweight=sum(cent_grp1),n_valid=sum(tot)) %>%
  ungroup() %>% 
  mutate(ovob_perc=epi_overweightobese/n_valid*100,und_perc=epi_underweight/n_valid*100) %>%
  gather(epi_category,percentage,ovob_perc:und_perc) %>%
  subset(select=c(schlyr_exam,sex,epi_category,percentage)) %>%
  mutate(category=paste(sex,epi_category))
         


trend_palette <- scale_color_manual(values = c("#004949", "#006dd1", "#49006a", 
                                               "#920000", "#924900", "#db6d00", 
                                               "#24ff24"))

shape_palette <- scale_shape_manual(values = c(16, 15, 17, 3, 4, 8, 18))


figure_two <- ggplot(data = figure_two_data, aes(x = schlyr_exam, 
                                                y = percentage)) +
geom_line(aes(colour = sex, linetype = epi_category, group = category), size = 1) +
  scale_color_manual(values = c("#004949", "#006dd1", "#49006a", 
                                "#920000", "#924900", "#db6d00", 
                                "#24ff24"),
                     labels = c("Female", "Male")) +
  scale_linetype_manual(labels = c("Overweight", "Underweight"),
                        values = c("dotdash", "solid")) +
  labs(x = "School Year", y = "Percentage") +
  theme(panel.background = element_blank(),
        panel.grid.minor.x = element_line(size = .15, color = "#C0C0C0"), 
        panel.grid.major.y = element_blank(),
        axis.line.x = element_line(size = .1, color = "#787878"),
        axis.line.y = element_line(size = .1, color = "#787878"),
        axis.title.x = element_text(size = 10, face = "bold"),
        axis.title.y = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(size = 9, angle = 90, hjust = 0, vjust = 0.5),
        legend.title = element_blank(),
        legend.position = "top",
        legend.key = element_rect(colour = NA, fill = NA)) +
  scale_y_continuous(labels = function(x) sprintf("%.1f", x),
                     limits = c(0, NA))


figure_four_data <- readRDS(paste(file.path(host_folder, "BMI_data_0102_1718.rds"))) %>%
  subset(select = c(simd, schlyr_exam, tot, cent_grp5)) %>%
  group_by(simd, schlyr_exam) %>%
  summarise(epi_overweightobese=sum(cent_grp5),n_valid=sum(tot)) %>%
  ungroup() %>% 
  mutate(ovob_perc=epi_overweightobese/n_valid*100)

figure_four <- ggplot(data = figure_four_data %>% filter(simd != "NA"), aes(x = schlyr_exam, 
                                                 y = ovob_perc)) +
  geom_line(aes(colour = as.factor(simd), group = as.factor(simd)), size = 1) +
  scale_color_manual(values = c("#004949", "#006dd1", "#49006a", 
                                "#920000", "#924900", "#db6d00", 
                                "#24ff24"),
                     labels = c("1 - Most deprived", "2", "3", "4", "5 - Least deprived")) +
  labs(x = "School Year", y = "Percentage") +
  theme(panel.background = element_blank(),
        panel.grid.minor.x = element_line(size = .15, color = "#C0C0C0"), 
        panel.grid.major.y = element_blank(),
        axis.line.x = element_line(size = .1, color = "#787878"),
        axis.line.y = element_line(size = .1, color = "#787878"),
        axis.title.x = element_text(size = 10, face = "bold"),
        axis.title.y = element_text(size = 10, face = "bold"),
        axis.text.x = element_text(size = 9, angle = 90, hjust = 0, vjust = 0.5),
        legend.title = element_blank(),
        legend.position = "top",
        legend.key = element_rect(colour = NA, fill = NA)) +
  scale_y_continuous(labels = function(x) sprintf("%.1f", x),
                     limits = c(0, 30))

figure_four


figure_one_data <- readRDS(paste(file.path(host_folder, "BMI_data_0102_1718.rds"))) %>%
  subset(select = c(schlyr_exam, tot, cent_grp1, cent_grp2, cent_grp3, cent_grp4)) %>%
  group_by(schlyr_exam) %>%
  summarise(epi_underweight=sum(cent_grp1), epi_healthyweight=sum(cent_grp2), 
            epi_overweight=sum(cent_grp3), epi_obese=sum(cent_grp4), n_valid=sum(tot)) %>%
  ungroup() %>% 
  mutate(underweight_perc=epi_underweight/n_valid*100, healthyweight_perc=epi_healthyweight/n_valid*100,
         overweight_perc=epi_overweight/n_valid*100, obese_perc=epi_obese/n_valid*100) %>%
  gather(epi_category,percentage,underweight_perc:obese_perc) %>%
  subset(select=c(schlyr_exam,epi_category,percentage))

#  figure_one_data$group <- factor(figure_one_data$group , levels=c("underweight_perc", "healthyweight_perc", "overweight_perc", "obese_perc") )

  figure_one <- ggplot(data = figure_one_data, aes(x = schlyr_exam, 
                                                 y = percentage, group=epi_category, fill=epi_category)) +
    geom_area(position="fill") +
  scale_fill_manual(values=c("#0072B2", "#F0E442", "#E69F00", "#D55E00"),
                      labels=c("At risk of underweight", "Healthy Weight", "At risk of overweight", "At risk of obesity")) +
                      scale_y_continuous(labels = scales::percent) +
                  labs(x = "School Year", y = "Percentage") +
          theme(panel.background = element_blank(),
                panel.grid.minor.x = element_line(size = .15, color = "#C0C0C0"), 
                panel.grid.major.y = element_blank(),
                axis.text.x = element_text(angle = 45, hjust = 1),
                legend.title = element_blank())

figure_one



