###########################################################
####FoRTE Soil Respiration Data Analysis
####2018-2021
####Kayla C. Mathes  
###########################################################

###Load Packages
library(ggplot2)
library(dplyr)
library(stringr)
library(plotrix)
library(reshape2)
library(ggpubr)
library(rstatix)
library(tidyverse)
library(googledrive)
library(lubridate)
library(scales)
library(rcartocolor)
library(gridExtra)
library(gvlma)  ##Checking linear regression assumptions




######Download Rs and Rh data for 2018-2021 from Google Drive####

# Direct Google Drive link to "FoRTE/data/soil_respiration/2021"
as_id("https://drive.google.com/drive/folders/1NVO0DUmR_JcYTzw2vq4D9TNItxqM_fp9") %>% 
  drive_ls ->
  gdfiles

# Create a new data directory for files, if necessary
data_dir <- "googledrive_data/"
if(!dir.exists(data_dir)) dir.create(data_dir)

#Download date
for(f in seq_len(nrow(gdfiles))) {
  cat(f, "/", nrow(gdfiles), " Downloading ", gdfiles$name[f], "...\n", sep = "")
  drive_download(gdfiles[f,], overwrite = TRUE, path = file.path(data_dir, gdfiles$name[f]))
}

## Import downloaded date from new data directory "googledrive_data"
all_2018 <- read.csv("googledrive_data/Rs_2018.csv", na.strings = c("NA", "na"))
all_2019 <- read.csv("googledrive_data/Rs_2019.csv", na.strings = c("NA","na"))
all_2020 <- read.csv("googledrive_data/Rs_2020.csv", na.strings = c("NA","na"))
all_2021 <- read.csv("googledrive_data/Rs_2021.csv", na.strings = c("NA","na"))
Rh_2019 <- read.csv("googledrive_data/Rh_2019.csv", na.strings = c("NA", "na"))
Rh_2020 <- read.csv("googledrive_data/Rh_2020.csv", na.strings = c("NA", "na", "#VALUE!"))
Rh_2021 <- read.csv("googledrive_data/Rh_2021.csv", na.strings = c("NA", "na", "#VALUE!"))


####Clean Dataframes#####
#leave out notes/comments column and get rid of NA efflux values 

#2018
all_2018_sub <- all_2018%>%
  select(!notes)%>%
  filter(!is.na(soilCO2Efflux))


#2019 (Also eliminate dates in July that only have data for Rep D)
all_2019_sub <- all_2019%>%
  select(!notes)%>%
  select(!HHMMSS)%>%
  filter(date != "2019-05-30") %>%
  filter(date != "2019-06-11")%>%
  filter(date != "2019-06-18")%>%
  filter(date != "2019-06-28")%>%
  filter(date != "2019-07-05")%>%
  filter(!is.na(soilCO2Efflux))

#2020
all_2020_sub <- all_2020%>%
  select(!notes)%>%
  select(!HH.MM.SS)%>%
  filter(!is.na(soilCO2Efflux))

#2021
all_2021_sub <- all_2021%>%
  select(!notes)%>%
  filter(!is.na(soilCO2Efflux))


#####Combine all years into one dataset 
all_years <- rbind(all_2021_sub,all_2020_sub, all_2019_sub, all_2018_sub)

##Convert date into POSIXct class and add just a year column 
all_years$date <- as.POSIXct(all_years$date,format="%Y-%m-%d")
all_years$year <- format(all_years$date, format = "%Y")

##Combine Rep_ID, Plot_ID and Subplot into one column to create "subplot_code" column 
all_years$Subplot_code <- str_c(all_years$Rep_ID, '',all_years$Plot_ID,'', all_years$Subplot)



##Add severity and treatment values to all dataset using Subplot_code ####
##add disturbance severity and treatment columns from subplot codes  
Plot_conversion <- function(Subplot_code) {
  if(str_detect(Subplot_code, "A1e")==TRUE) {
    Disturbance_severity = "85"
  } else if(str_detect(Subplot_code, "A1w")==TRUE) {
    Disturbance_severity = "85"
  }else if(str_detect(Subplot_code, "A2e")==TRUE) {
    Disturbance_severity = "45"
  }else if(str_detect(Subplot_code, "A2w")==TRUE) {
    Disturbance_severity = "45"
  }else if(str_detect(Subplot_code, "A3e")==TRUE) {
    Disturbance_severity = "65"
  }else if(str_detect(Subplot_code, "A3w")==TRUE) {
    Disturbance_severity = "65"
  }else if(str_detect(Subplot_code, "A4e")==TRUE) {
    Disturbance_severity = "0"
  }else if(str_detect(Subplot_code, "A4w")==TRUE) {
    Disturbance_severity = "0"
  }else if(str_detect(Subplot_code, "B1e")==TRUE) {
    Disturbance_severity = "0"
  } else if(str_detect(Subplot_code, "B1w")==TRUE) {
    Disturbance_severity = "0"
  } else if(str_detect(Subplot_code, "B2e")==TRUE) {
    Disturbance_severity = "45"
  } else if(str_detect(Subplot_code, "B2w")==TRUE) {
    Disturbance_severity = "45"
  } else if(str_detect(Subplot_code, "B3e")==TRUE) {
    Disturbance_severity = "85"
  } else if(str_detect(Subplot_code, "B3w")==TRUE) {
    Disturbance_severity = "85"
  } else if(str_detect(Subplot_code, "B4e")==TRUE) {
    Disturbance_severity = "65"
  } else if(str_detect(Subplot_code, "B4w")==TRUE) {
    Disturbance_severity = "65"
  } else if(str_detect(Subplot_code, "C1e")==TRUE) {
    Disturbance_severity = "0"
  } else if(str_detect(Subplot_code, "C1w")==TRUE) {
    Disturbance_severity = "0"
  } else if(str_detect(Subplot_code, "C2e")==TRUE) {
    Disturbance_severity = "65"
  } else if(str_detect(Subplot_code, "C2w")==TRUE) {
    Disturbance_severity = "65"
  } else if(str_detect(Subplot_code, "C3e")==TRUE) {
    Disturbance_severity = "85"
  } else if(str_detect(Subplot_code, "C3w")==TRUE) {
    Disturbance_severity = "85"
  } else if(str_detect(Subplot_code, "C4e")==TRUE) {
    Disturbance_severity = "45"
  } else if(str_detect(Subplot_code, "C4w")==TRUE) {
    Disturbance_severity = "45"
  } else if(str_detect(Subplot_code, "D1e")==TRUE) {
    Disturbance_severity = "0"
  } else if(str_detect(Subplot_code, "D1w")==TRUE) {
    Disturbance_severity = "0"
  } else if(str_detect(Subplot_code, "D2e")==TRUE) {
    Disturbance_severity = "85"
  } else if(str_detect(Subplot_code, "D2w")==TRUE) {
    Disturbance_severity = "85"
  } else if(str_detect(Subplot_code, "D3e")==TRUE) {
    Disturbance_severity = "45"
  } else if(str_detect(Subplot_code, "D3w")==TRUE) {
    Disturbance_severity = "45"
  } else if(str_detect(Subplot_code, "D4e")==TRUE) {
    Disturbance_severity = "65"
  } else if(str_detect(Subplot_code, "D4w")==TRUE) {
    Disturbance_severity = "65"
  }
  return(Disturbance_severity)
}

##Add disturbance severity column to dataset
all_years <- all_years %>%
  mutate(Severity = sapply(Subplot_code, Plot_conversion))

##Add Treatment variable 
Sub_plot_conversion <- function(Subplot_code) {
  if(str_detect(Subplot_code, "A1e")==TRUE) {
    Treatment = "bottom"
  } else if(str_detect(Subplot_code, "A1w")==TRUE) {
    Treatment = "top"
  }else if(str_detect(Subplot_code, "A2e")==TRUE) {
    Treatment = "top"
  }else if(str_detect(Subplot_code, "A2w")==TRUE) {
    Treatment = "bottom"
  }else if(str_detect(Subplot_code, "A3e")==TRUE) {
    Treatment = "bottom"
  }else if(str_detect(Subplot_code, "A3w")==TRUE) {
    Treatment = "top"
  }else if(str_detect(Subplot_code, "A4e")==TRUE) {
    Treatment = "bottom"
  }else if(str_detect(Subplot_code, "A4w")==TRUE) {
    Treatment = "top"
  }else if(str_detect(Subplot_code, "B1e")==TRUE) {
    Treatment = "bottom"
  } else if(str_detect(Subplot_code, "B1w")==TRUE) {
    Treatment = "top"
  } else if(str_detect(Subplot_code, "B2e")==TRUE) {
    Treatment = "top"
  } else if(str_detect(Subplot_code, "B2w")==TRUE) {
    Treatment = "bottom"
  } else if(str_detect(Subplot_code, "B3e")==TRUE) {
    Treatment = "bottom"
  } else if(str_detect(Subplot_code, "B3w")==TRUE) {
    Treatment = "top"
  } else if(str_detect(Subplot_code, "B4e")==TRUE) {
    Treatment = "top"
  } else if(str_detect(Subplot_code, "B4w")==TRUE) {
    Treatment = "bottom"
  } else if(str_detect(Subplot_code, "C1e")==TRUE) {
    Treatment = "top"
  } else if(str_detect(Subplot_code, "C1w")==TRUE) {
    Treatment = "bottom"
  } else if(str_detect(Subplot_code, "C2e")==TRUE) {
    Treatment = "bottom"
  } else if(str_detect(Subplot_code, "C2w")==TRUE) {
    Treatment = "top"
  } else if(str_detect(Subplot_code, "C3e")==TRUE) {
    Treatment = "bottom"
  } else if(str_detect(Subplot_code, "C3w")==TRUE) {
    Treatment = "top"
  } else if(str_detect(Subplot_code, "C4e")==TRUE) {
    Treatment = "top"
  } else if(str_detect(Subplot_code, "C4w")==TRUE) {
    Treatment = "bottom"
  } else if(str_detect(Subplot_code, "D1e")==TRUE) {
    Treatment = "bottom"
  } else if(str_detect(Subplot_code, "D1w")==TRUE) {
    Treatment = "top"
  } else if(str_detect(Subplot_code, "D2e")==TRUE) {
    Treatment = "bottom"
  } else if(str_detect(Subplot_code, "D2w")==TRUE) {
    Treatment = "top"
  } else if(str_detect(Subplot_code, "D3e")==TRUE) {
    Treatment = "bottom"
  } else if(str_detect(Subplot_code, "D3w")==TRUE) {
    Treatment = "top"
  } else if(str_detect(Subplot_code, "D4e")==TRUE) {
    Treatment = "top"
  } else if(str_detect(Subplot_code, "D4w")==TRUE) {
    Treatment = "bottom"
  }
  return(Treatment)
}

##Add treatment column to datasets
all_years <- all_years %>%
  mutate(Treatment = sapply(Subplot_code, Sub_plot_conversion))

######Create Full Rs, moisture and temperature timeseries####

##Create date groups (1 complete round of respiration) 
all_years_grouped <- all_years%>%
  mutate(week_group = case_when(date == "2018-07-27" | date == "2018-08-03" ~ "2018-07-07",
                                date == "2018-08-10" | date == "2018-08-14" ~ "2018-08-10",
                                date == "2018-11-15" | date == "2018-11-16" | date =="2018-11-17" ~ "2018-11-15",
                                date == "2019-05-14" | date == "2019-05-15" ~ "2019-05-14", 
                                date == "2019-05-21" | date == "2019-05-22" |date == "2019-05-23" | date == "2019-05-24" ~ "2019-05-21", 
                                date == "2019-06-05" | date == "2019-06-06" | date == "2019-06-07" ~ "2019-06-05", 
                                date == "2019-07-08" | date == "2019-07-09" | date =="2019-07-12" ~ "2019-07-08", 
                                date == "2019-07-16" | date == "2019-07-17" | date == "2019-07-19" ~ "2019-07-16", 
                                date == "2019-07-24" | date == "2019-07-25" | date == "2019-07-26" ~ "2019-07-24", 
                                date == "2019-08-01" |date == "2019-08-02" |date == "2019-08-03" ~ "2019-08-01", 
                                date == "2019-11-11" |date == "2019-11-12" ~ "2019-11-11", 
                                date == "2020-07-07" |date == "2020-07-08" | date == "2020-07-09" ~ "2020-07-07", 
                                date == "2020-07-24" | date == "2020-07-25" ~ "2020-07-24", 
                                date == "2020-08-05" | date == "2020-08-06" ~ "2020-08-05", 
                                date == "2020-11-16" |date == "2020-11-17" | date == "2020-11-18" ~ "2020-11-16", 
                                date == "2021-07-06" | date =="2021-07-09" |date == "2021-07-10" ~ "2021-07-06",
                                date == "2021-08-03" |  date == "2021-08-04" |  date == "2021-08-06" ~ "2021-08-03"))

##Make week group date class 
all_years_grouped$week_group <- as.Date(all_years_grouped$week_group,format="%Y-%m-%d")

##(Rs, soil temp and VWC were separated to get rid of NA values for each factor)
#####Rs Timeseries 
###Summarize by subplots (Collars are sudo-replicates and they variation should not be represented in the model (subplot is the smallest unit, we are interested in variation across replicates).
all_years_summary_timeseries <- all_years_grouped%>%
  group_by(Rep_ID, week_group, Severity, Treatment, Subplot_code)%>%
  summarize(soilCO2Efflux = mean(soilCO2Efflux))

##Summarize severity by replicate per round of respiration (week group)
all_years_timeseries_severity <- all_years_summary_timeseries%>%
  group_by(week_group, Severity)%>%
  summarize(ave_efflux = mean(soilCO2Efflux), std_error_efflux = std.error(soilCO2Efflux))

##Summarize treatment by replicates per round of respiration (week group)
all_years_timeseries_treatment <- all_years_summary_timeseries%>%
  group_by(week_group, Treatment)%>%
  summarize(ave_efflux = mean(soilCO2Efflux), std_error_efflux = std.error(soilCO2Efflux))

######Soil Temperature Timeseries 
###Summarize by subplots (Collars are sudo-replicates and they variation should not be represented in the model subplot is the smallest unit, we are interested in variation across replicates).
all_years_summary_timeseries_temp <- all_years_grouped%>%
  filter(!is.na(soilTemp))%>%
  group_by(Rep_ID, week_group, Severity, Treatment, Subplot_code)%>%
  summarize(soilTemp = mean(soilTemp))

##Summarize severity by replicate per round of respiration (week group)
all_years_timeseries_severity_temp <- all_years_summary_timeseries_temp%>%
  group_by(week_group, Severity)%>%
  summarize(ave_temp = mean(soilTemp), std_error_temp = std.error(soilTemp))

##Summarize treatment by replicates per round of respiration (week group)
all_years_timeseries_treatment_temp <- all_years_summary_timeseries_temp%>%
  group_by(week_group, Treatment)%>%
  summarize(ave_temp = mean(soilTemp), std_error_temp = std.error(soilTemp))


######Soil Moisture (VWC) Timeseries
###Summarize by subplots (Collars are sudo-replicates and they variation should not be represented in the model subplot is the smallest unit, we are interested in variation across replicates).
all_years_summary_timeseries_VWC <- all_years_grouped%>%
  filter(!is.na(VWC))%>%
  group_by(Rep_ID, week_group, Severity, Treatment, Subplot_code)%>%
  summarize(VWC =mean(VWC))

##Summarize severity by replicate per round of respiration (week group)
all_years_timeseries_severity_VWC <- all_years_summary_timeseries_VWC%>%
  group_by(week_group, Severity)%>%
  summarize(ave_VWC = mean(VWC), std_error_VWC = std.error(VWC))

##Summarize treatment by replicates per round of respiration (week group)
all_years_timeseries_treatment_VWC <- all_years_summary_timeseries_VWC%>%
  group_by(week_group, Treatment)%>%
  summarize(ave_VWC = mean(VWC), std_error_VWC = std.error(VWC))


######Figure 1: BIG Timeseries #####
##Rs Severity 
p1 <- ggplot(all_years_timeseries_severity, aes(x = week_group, y = ave_efflux, group = Severity, color = Severity)) +
  theme_classic() +
  labs(x = "Date", y=expression(paste("Rs (",mu*molCO[2],"  ",m^-2,"  ",sec^-1,")"))) +
  (scale_x_date(date_labels = "%b %Y", breaks = as.Date(c("2018-07-15", "2019-01-11", "2019-07-15", "2020-01-11", "2020-07-15", "2021-01-11", "2021-07-15")), sec.axis = sec_axis(~ .,labels = NULL, breaks = as.Date(c("2018-07-15", "2019-01-11", "2019-07-15", "2020-01-11", "2020-07-15", "2021-01-11", "2021-07-15")))))+
  geom_path(size = 0.5, alpha = 0.9) +
  geom_point(size = 2.5) +
  theme(axis.text.x = element_blank(),axis.title.x = element_blank(), legend.position = c(0.15,0.78), axis.text.y = element_text(size = 15), axis.title.y = element_text(size = 20),legend.text = element_text(size = 15), legend.title = element_blank(),legend.spacing.x = unit(0.1, 'cm'),legend.spacing.y = unit(0.1, 'cm'), plot.margin = margin(0,0,0,0.33,"cm")) +
  geom_errorbar(mapping=aes(x=week_group, ymin=ave_efflux - std_error_efflux, ymax=ave_efflux + std_error_efflux), size = 0.7, width = 15) +
  scale_color_manual(values=c("#000000", "#009E73", "#0072B2", "#D55E00")) +
  scale_y_continuous(position = "left",breaks = seq(from = 1, to = 10, by = 2),sec.axis = dup_axis(name = NULL, labels = NULL))+
  guides(color=guide_legend(nrow=2, byrow=TRUE))

##Rs Treatment 
p2 <- ggplot(all_years_timeseries_treatment, aes(x = week_group, y = ave_efflux, group = Treatment, color =Treatment)) +
  theme_classic() +
  labs(x = "Date", y=expression(paste("Rs (",mu*molCO[2],"  ",m^-2,"  ",sec^-1,")"))) +
  (scale_x_date(date_labels = "%b %Y", breaks = as.Date(c("2018-07-15", "2019-01-11", "2019-07-15", "2020-01-11", "2020-07-15", "2021-01-11", "2021-07-15")), sec.axis = sec_axis(~ .,labels = NULL, breaks = as.Date(c("2018-07-15", "2019-01-11", "2019-07-15", "2020-01-11", "2020-07-15", "2021-01-11", "2021-07-15")))))+
  geom_path(size = 0.5, alpha = 0.9) +
  geom_point(size = 2.5) +
  theme(axis.text.x = element_blank(),axis.title.x = element_blank(), legend.position = c(0.15,0.78), axis.text.y = element_text(size = 15), axis.title.y =  element_text(size = 20),legend.text = element_text(size = 15), legend.title = element_blank(),legend.spacing.x = unit(0.1, 'cm'),legend.spacing.y = unit(0.1, 'cm'), plot.margin = margin(0,0.20,0,0,"cm")) +
  geom_errorbar(mapping=aes(x=week_group, ymin=ave_efflux - std_error_efflux, ymax=ave_efflux + std_error_efflux), size = 0.7, width = 15) +
  scale_color_manual(values=c("#A6611A", "#018571")) +
  scale_y_continuous(position = "right",breaks = seq(from = 1, to = 10, by = 2),sec.axis = dup_axis(name = NULL, labels = NULL))+
  guides(color=guide_legend(nrow=2, byrow=TRUE))

##Temp Severity 
p3 <- ggplot(all_years_timeseries_severity_temp, aes(x = week_group, y = ave_temp, group = Severity, color = Severity)) +
  labs(x = "Date", y=expression(paste('Temp ('*~degree*C*')'))) +
  (scale_x_date(date_labels = "%b %Y", breaks = as.Date(c("2018-07-15", "2019-01-11", "2019-07-15", "2020-01-11", "2020-07-15", "2021-01-11", "2021-07-15")), sec.axis = sec_axis(~ .,labels = NULL, breaks = as.Date(c("2018-07-15", "2019-01-11", "2019-07-15", "2020-01-11", "2020-07-15", "2021-01-11", "2021-07-15")))))+
  geom_path(size = 0.5, alpha = 0.9) +
  geom_point(size = 2.5) +
  geom_errorbar(mapping=aes(x=week_group, ymin=ave_temp - std_error_temp, ymax=ave_temp + std_error_temp), size = 0.7, width = 15) +
  theme_classic() +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank(),legend.position = "none", axis.text.y = element_text(size = 15), axis.title.y = element_text(size = 20), plot.margin = margin(-0.23,0,0,0.21, "cm")) +
  scale_color_manual(values=c("#000000", "#009E73", "#0072B2", "#D55E00")) +
  scale_y_continuous(position = "left",breaks = seq(from = 1, to = 30, by = 5),sec.axis = dup_axis(name = NULL, labels = NULL))

##Temp Treatment 
p4 <- ggplot(all_years_timeseries_treatment_temp, aes(x = week_group, y = ave_temp, group = Treatment, color = Treatment)) +
  labs(x = "Date", y=expression(paste('Temp ('*~degree*C*')'))) +
  (scale_x_date(date_labels = "%b %Y", breaks = as.Date(c("2018-07-15", "2019-01-11", "2019-07-15", "2020-01-11", "2020-07-15", "2021-01-11", "2021-07-15")), sec.axis = sec_axis(~ .,labels = NULL, breaks = as.Date(c("2018-07-15", "2019-01-11", "2019-07-15", "2020-01-11", "2020-07-15", "2021-01-11", "2021-07-15")))))+
  geom_path(size = 0.5, alpha = 0.9) +
  geom_point(size = 2.5) +
  geom_errorbar(mapping=aes(x=week_group, ymin=ave_temp - std_error_temp, ymax=ave_temp + std_error_temp), size = 0.7, width = 15) +
  theme_classic() +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank(),legend.position = "none", axis.text.y  = element_text(size = 15), axis.title.y = element_text(size = 20), plot.margin = margin(-0.23,0,0,0.21, "cm")) +
  scale_color_manual(values=c("#A6611A", "#018571")) +
  scale_y_continuous(position = "right",breaks = seq(from = 1, to = 30, by = 5),sec.axis = dup_axis(name = NULL, labels = NULL))

##VWC Severity 
p5 <- ggplot(all_years_timeseries_severity_VWC, aes(x = week_group, y = ave_VWC, group = Severity, color = Severity)) +
  labs(x = "Date", y= "VWC (%)") +
  (scale_x_date(date_labels = "%b %Y", breaks = as.Date(c("2018-07-15", "2019-01-11", "2019-07-15", "2020-01-11", "2020-07-15", "2021-01-11", "2021-07-15")), sec.axis = sec_axis(~ .,labels = NULL, breaks = as.Date(c("2018-07-15", "2019-01-11", "2019-07-15", "2020-01-11", "2020-07-15", "2021-01-11", "2021-07-15")))))+
  geom_path(size = 0.5, alpha = 0.9) +
  geom_point(size = 2.5) +
  geom_errorbar(mapping=aes(x=week_group, ymin=ave_VWC - std_error_VWC, ymax=ave_VWC + std_error_VWC), size = 0.7, width = 15) +
  theme_classic() +
  scale_color_manual(values=c("#000000", "#009E73", "#0072B2", "#D55E00")) +
  scale_y_continuous(position = "left",breaks = seq(from = 1, to = 30, by = 5),sec.axis = dup_axis(name = NULL, labels = NULL)) +
  theme(axis.text.y = element_text(size = 15), axis.title.y = element_text(size = 20), legend.position = "none", axis.text.x = element_text(size = 15),  axis.title.x = element_text(size = 15), plot.margin = margin(-0.23,0,0,0.4, "cm")) +
  guides(color=guide_legend(nrow=2, byrow=TRUE))
  


##VWC Treatment 
p6 <- ggplot(all_years_timeseries_treatment_VWC, aes(x = week_group, y = ave_VWC, group = Treatment, color = Treatment)) +
  labs(x = "Date", y= "VWC (%)") +
  (scale_x_date(date_labels = "%b %Y", breaks = as.Date(c("2018-07-15", "2019-01-11", "2019-07-15", "2020-01-11", "2020-07-15", "2021-01-11", "2021-07-15")), sec.axis = sec_axis(~ .,labels = NULL, breaks = as.Date(c("2018-07-15", "2019-01-11", "2019-07-15", "2020-01-11", "2020-07-15", "2021-01-11", "2021-07-15")))))+
  geom_path(size = 0.5, alpha = 0.9) +
  geom_point(size = 2.5) +
  geom_errorbar(mapping=aes(x=week_group, ymin=ave_VWC - std_error_VWC, ymax=ave_VWC + std_error_VWC), size = 0.7, width = 15) +
  theme_classic() +
  scale_color_manual(values=c("#A6611A", "#018571")) +
  scale_y_continuous(position = "right",breaks = seq(from = 1, to = 30, by = 5),sec.axis = dup_axis(name = NULL, labels = NULL)) +
  theme(axis.text.y  = element_text(size = 15), axis.title.y  = element_text(size = 20), legend.position = "none", axis.text.x = element_text(size = 12.5),  axis.title.x = element_text(size = 15), plot.margin = margin(-0.23,0.20,0,0, "cm")) +
  guides(color=guide_legend(nrow=2, byrow=TRUE))

#Severity multipanel Figure 
p1_grob <- ggplotGrob(p1)
p3_grob <- ggplotGrob(p3)
p5_grob <- ggplotGrob(p5)
##Treatment multipanel figure 
p2_grob <- ggplotGrob(p2)
p4_grob <- ggplotGrob(p4)
p6_grob <- ggplotGrob(p6)

layout <- rbind(c(1,2),
                c(3,4), 
                c(5,6))
g_timeseries <- grid.arrange(p1_grob, p2_grob, p3_grob, p4_grob, p5_grob, p6_grob, layout_matrix=layout)

ggsave("Figure_1.png", height = 10, width = 15, units = "in", g_timeseries)





#######Summarize per year (growing season only)######
##Inlude only growing season dates from all years (July and August)
all_years_gs <- all_years%>%
  filter(date == "2019-07-08" | date == "2019-07-09" | date == "2019-07-12" | date == "2019-07-16" | date == "2019-07-17" | date == "2019-07-18" | date == "2019-07-19" | date == "2019-07-24" | date == "2019-07-25" | date == "2019-07-26" | date == "2019-08-01" | date == "2019-08-02" | date == "2019-08-03" | date == "2020-07-07" | date == "2020-07-08" |date == "2020-07-09" | date == "2020-07-24" | date == "2020-07-25" | date == "2020-08-05" | date == "2020-08-06" | date == "2018-07-27" | date == "2018-08-03" | date == "2018-08-10" | date == "2018-08-14" | date == "2021-07-06" |date == "2021-07-09" |date == "2021-07-10" | date == "2021-08-03" | date == "2021-08-04" | date == "2021-08-06") 

###Summarize by subplots (Collars are sudo-replicates and they variation should not be represented in the model subplot is the smallest unit, we are interested in variation across replicates).
all_years_summary <- all_years_gs%>%
  group_by(Rep_ID, year, Severity, Treatment)%>%
  summarize(soilCO2Efflux = mean(soilCO2Efflux), soilTemp = mean(soilTemp, na.rm=TRUE), VWC =mean(VWC))

####Summarize Data by severity (Growing season) ######

##Summarize severity by replications per year 
all_years_summary_severity <- all_years_gs%>%
  group_by(year, Rep_ID, Severity)%>%
  summarize(ave_efflux = mean(soilCO2Efflux), std_error_efflux = std.error(soilCO2Efflux))

all_years_summary_severity$year <- as.factor(all_years_summary_severity$year)

all_years_summary_ameriflux <- all_years_summary_severity%>%
  mutate(pre_post = case_when(year == "2018" ~ "Pre-Disturbance", 
                              year == "2019" | year == "2020" | year == "2021" ~ "Post-Disturbance")) 
                                                  
all_years_summary_ameriflux <- all_years_summary_ameriflux%>%
  group_by(pre_post, Rep_ID, Severity)%>%
  summarize(ave_efflux = mean(ave_efflux), std_error_efflux = std.error(std_error_efflux))
  
  
  

######Visualize Rs data by severity by Year####
##Boxplot of replicate averages across disturbance severity per year 
splot <- ggplot(all_years_summary_severity,aes(x = Severity, y = ave_efflux, fill = Severity)) +
  scale_fill_manual(values=c("#000000", "#009E73", "#0072B2", "#D55E00"))+
  theme_classic()+
  geom_boxplot()+
  theme(axis.text.x= element_text(size = 20), axis.text.y= element_text(size=20), axis.title.x = element_text(size = 25), axis.title.y  = element_text(size=25), legend.title = element_blank(),  strip.text.x =element_text(size = 25), legend.text = element_blank(), legend.position = "none",plot.margin = margin(1,1,1,1, "cm")) +
  scale_y_continuous(sec.axis = sec_axis(~ .,labels = NULL)) +
  facet_grid(.~year,scales="free")+ 
  guides(col = guide_legend(nrow = 2)) +
  labs(x = "Severity", y=expression(paste("Rs (",mu*molCO[2],"  ",m^-2,"  ",sec^-1,")"))) 
ggsave("severity_boxplot.png",height = 10, width = 15 , units = "in")

###Boxplot of replicate averagers across disturbance severity post and Post(mean of 2019-2021)
ggplot(all_years_summary_ameriflux, aes(x = Severity, y = ave_efflux, fill = Severity)) +
  scale_fill_manual(values=c("#000000", "#009E73", "#0072B2", "#D55E00"))+
  theme_classic()+
  geom_boxplot()+
  theme(axis.text.x= element_text(size = 20), axis.text.y= element_text(size=20), axis.title.x = element_text(size = 25), axis.title.y  = element_text(size=25), legend.title = element_blank(),  strip.text.x =element_text(size = 25), legend.text = element_blank(), legend.position = "none",plot.margin = margin(1,1,1,1, "cm")) +
  scale_y_continuous(sec.axis = sec_axis(~ .,labels = NULL)) +
  facet_grid(~factor(pre_post,levels=c("Pre-Disturbance","Post-Disturbance")))+ 
  guides(col = guide_legend(nrow = 2)) +
  labs(x = "Disturbance Severity:Gross defoliation (%)", y=expression(paste("Soil Respiration (",mu*molCO[2],"  ",m^-2,"  ",sec^-1,")"))) 
ggsave("severity_boxplot_ameriflux.png",height = 10, width = 15 , units = "in")

#####Summarize data by Treatment######
##Summarize treatment by replications per year 
all_years_summary_treatment <- all_years_gs%>%
  group_by(year, Rep_ID, Treatment)%>%
  summarize(ave_efflux = mean(soilCO2Efflux), std_error_efflux = std.error(soilCO2Efflux))

####Create a boxplot of Rs by treatment by year 
ggplot(all_years_summary_treatment,aes(x = Treatment, y = ave_efflux, fill = Treatment)) +
  scale_fill_manual(values=c("darkgoldenrod4", "forestgreen"))+
  theme_classic()+
  geom_boxplot()+
  theme(axis.text.x= element_text(size = 15), axis.text.y= element_text(size=15), axis.title.x = element_text(size = 15), axis.title.y  = element_text(size=15), legend.title = element_blank(),  strip.text.x =element_text(size = 15), legend.text = element_blank(), legend.position = "none") +
  facet_grid(.~year,scales="free")+ 
  guides(col = guide_legend(nrow = 2)) +
  labs(x = "Treatment", y=expression(paste("Soil Respiration (",mu*molCO[2],"  ",m^-2,"  ",sec^-1,")"))) 



#########Split plot Model for absolute data#####
library(agricolae)

##Transform variables into factors for model 
all_years_summary$Severity <- as.factor(all_years_summary$Severity)
all_years_summary$Treatment <- as.factor(all_years_summary$Treatment)
all_years_summary$year <- as.factor(all_years_summary$year)

####Testing Assumptions 
##Test for outliers test: no extreme outliers
all_years_summary %>% 
  group_by(Severity, Treatment, year) %>%
  identify_outliers(soilCO2Efflux)

##Equality of variance test for severity and treatment (Slightly unequal data using alpha = 0.05. Equal variance using alpha = 0.1)
leveneTest(soilCO2Efflux ~ year*Treatment*Severity, data = all_years_summary)

##Normality (Data are normal)
# Build the linear model
normality_test  <- lm(soilCO2Efflux ~ Severity*Treatment*year,
                      data = all_years_summary)

# Create a QQ plot of residuals
ggqqplot(residuals(normality_test))
# Shapiro test of normality 
shapiro_test(residuals(normality_test))


####WORKING SPLIT-SPLIT MODEL: Using aov(). Same results as the agricolae package. Ran an ANCOVA with VWC as a covariate.(However significance does not change with or without VWC). 

#Rs Model without covariates
rs_model <- aov(soilCO2Efflux  ~ Severity*Treatment*year + Error(Rep_ID/Severity/Treatment/year), data = all_years_summary)
summary(rs_model)

#Rs model with VWC covariate
rs_model_VWC <- aov(soilCO2Efflux  ~ Severity*Treatment*year +VWC + Error(Rep_ID/Severity/Treatment/year), data = all_years_summary)
summary(rs_model_VWC)

#Rs model with Temp covariate
rs_model_temp <- aov(soilCO2Efflux  ~ Severity*Treatment*year +soilTemp + Error(Rep_ID/Severity/Treatment/year), data = all_years_summary)
summary(rs_model_temp)

###Moisture model 
VWC_model <- aov(VWC  ~ Severity*Treatment*year + Error(Rep_ID/Severity/Treatment/year), data = all_years_summary)
summary(VWC_model)

#Temperature Model
temp_model <- aov(soilTemp  ~ Severity*Treatment*year + Error(Rep_ID/Severity/Treatment/year), data = all_years_summary)
summary(temp_model)

######Post hoc analysis: LSD test for Rs/VWC/temp models without covariates for all significant values in the model

#Rs model
out_year_severity_rs <- with(all_years_summary, LSD.test(soilCO2Efflux, year:Severity,72,0.703, console = TRUE))

out_year_rs <- with(all_years_summary,LSD.test(soilCO2Efflux,year,72,0.703,console=TRUE))

#VWC Model
out_year_VWC <- with(all_years_summary,LSD.test(VWC,year,72,0.76,console=TRUE))

#Temp Model
out_year_temp <- with(all_years_summary,LSD.test(soilTemp,year,72,0.75,console=TRUE))


#########Calculating Resistance ######
resistance_rs <- all_years_summary_severity


  resistance_rs <- resistance_rs%>%
    mutate(log_response = case_when(year == "2018" & Rep_ID == "A" ~ log(ave_efflux/resistance_rs$ave_efflux[1]),
                                  year == "2018" & Rep_ID == "B" ~ log(ave_efflux/resistance_rs$ave_efflux[5]),
                                  year == "2018" & Rep_ID == "C" ~ log(ave_efflux/resistance_rs$ave_efflux[9]),
                                  year == "2018" & Rep_ID == "D" ~ log(ave_efflux/resistance_rs$ave_efflux[13]),
                                  year == "2019" & Rep_ID == "A" ~ log(ave_efflux/resistance_rs$ave_efflux[17]),
                                  year == "2019" & Rep_ID == "B" ~ log(ave_efflux/resistance_rs$ave_efflux[21]),
                                  year == "2019" & Rep_ID == "C" ~ log(ave_efflux/resistance_rs$ave_efflux[25]),
                                  year == "2019" & Rep_ID == "D" ~ log(ave_efflux/resistance_rs$ave_efflux[29]),
                                  year =="2020" & Rep_ID == "A" ~ log(ave_efflux/resistance_rs$ave_efflux[33]),
                                  year =="2020" & Rep_ID == "B" ~ log(ave_efflux/resistance_rs$ave_efflux[37]),
                                  year =="2020" & Rep_ID == "C" ~ log(ave_efflux/resistance_rs$ave_efflux[41]),
                                  year =="2020" & Rep_ID == "D" ~ log(ave_efflux/resistance_rs$ave_efflux[45]),
                                  year =="2021" & Rep_ID == "A" ~ log(ave_efflux/resistance_rs$ave_efflux[49]),
                                  year =="2021" & Rep_ID == "B" ~ log(ave_efflux/resistance_rs$ave_efflux[53]),
                                  year =="2021" & Rep_ID == "C" ~ log(ave_efflux/resistance_rs$ave_efflux[57]),
                                  year =="2021" & Rep_ID == "D" ~ log(ave_efflux/resistance_rs$ave_efflux[61])))%>%
  group_by(year, Severity)%>%
  summarize(ave_log_response = mean(log_response), std_err = std.error(log_response))
  
  
resistance_rs$Severity <- as.numeric(resistance_rs$Severity)

##Run regression analysis for Rs resistance by severity per year: 2019,2020, 2021 all have significant linear decline with increasing disturbance severity. The slopes for each year post disturbance are not statistically different (overlapping 95% CI), but they are all significantly different than 2018. 

#2018
regression_2018 <- resistance_rs%>%
  filter(year == 2018)

regression_2018_model <- lm(ave_log_response ~ Severity, data = regression_2018)
summary(regression_2018_model)

confint(regression_2018_model, 'Severity', level=0.95)

#2019
regression_2019 <- resistance_rs%>%
  filter(year == 2019)

regression_2019_model <- lm(ave_log_response ~ Severity, data = regression_2019)
summary(regression_2019_model)

confint(regression_2019_model, 'Severity', level=0.95)

#2020
regression_2020 <- resistance_rs%>%
  filter(year == 2020)

regression_2020_model <- lm(ave_log_response ~ Severity, data = regression_2020)
summary(regression_2020_model)

confint(regression_2020_model, 'Severity', level=0.95)

#2021 yeah baby
regression_2021 <- resistance_rs%>%
  filter(year == 2021)

regression_2021_model <- lm(ave_log_response ~ Severity, data = regression_2021)
summary(regression_2021_model)

confint(regression_2021_model, 'Severity', level=0.95)

resistance_rs <- resistance_rs%>%
  mutate(pre_post = case_when(year == "2018" ~ "pre", 
                              year == "2019" | year == "2020" | year == "2021" ~ "post"))

###Since there are overlapping 95% CI for 2019-2021, we aggregated all years together into the average slope across all years post disturbance and ran a regression analysis as a function of severity 

##Make model dataframe
pre_post_model_df <- resistance_rs%>%
  filter(pre_post == "post")

#linear regression
pre_post_model <- lm(ave_log_response ~ Severity, data = pre_post_model_df)
summary(pre_post_model)

##graph model diagnostics
par(mfrow = c(2, 2))
plot(pre_post_model)

##Test model assumptions (all are met)
gvlma::gvlma(pre_post_model)


###Plot Rs resistance by severity per year with regression line representing average slope post disturbance years 
ggplot(resistance_rs, aes(x = Severity, y = ave_log_response)) +
  theme_classic()+
  geom_point(aes(colour = year), size = 3)+
  scale_color_manual(values=c("#000000", "#1B9E77", "#D95F02", "#7570B3")) +
  geom_smooth(method = "lm", se = FALSE, aes(linetype = pre_post), color = "black", size = 0.5, show.legend = FALSE, alpha = 0.2)+
  theme(axis.text= element_text(size = 20), axis.title = element_text(size = 25), legend.text = element_text(size = 15), legend.title = element_text(size = 15), legend.position = c(.2,.25)) +
  scale_y_continuous(sec.axis = sec_axis(~ .,labels = NULL)) +
  scale_x_continuous(sec.axis = sec_axis(~ .,labels = NULL)) +
  geom_errorbar(aes(ymin=ave_log_response - std_err, ymax=ave_log_response + std_err, colour = year), width = 3)+
  labs(y = "Rs Resistance") 
 
ggsave("Output/log_response.png",height = 10, width = 10, units = "in")

g <- ggarrange(splot, lr_plot, labels = c("A", "B"), font.label = list(size = 25))
ggsave("Output/combined_fig_chris.png",height = 10, width = 15, units = "in",g)


#####Heterotrophic Respiration Analysis 2019-2021#######
##Clean Dataframe 
#2019
Rh_2019_sub <- Rh_2019%>%
  select(Rep_ID, Plot_ID, Subplot, soilCO2Efflux, soilTemp, date_measured,oven.air.weight.ratio)%>%
  filter(!is.na(soilCO2Efflux))

#2020
Rh_2020_sub <- Rh_2020%>%
  select(Rep_ID, Plot_ID, Subplot, soilCO2Efflux, soilTemp, date_measured, oven.air.weight.ratio)%>%
  filter(!is.na(soilCO2Efflux))

#2021
Rh_2021_sub <- Rh_2021%>%
  select(Rep_ID, Plot_ID, Subplot, soilCO2Efflux, soilTemp, date_measured, oven.air.weight.ratio)%>%
  filter(!is.na(soilCO2Efflux))
Rh_2021 <- Rh_2021[-c(249),]

#####Combine all years into one dataset 
all_years_Rh <- rbind(Rh_2019_sub, Rh_2020_sub, Rh_2021_sub)

##Convert date into POSIXct class and add just a year column 
all_years_Rh$date_measured <- as.POSIXct(all_years_Rh$date_measured,format="%Y-%m-%d")
all_years_Rh$year <- format(all_years_Rh$date, format = "%Y")


##Combine Rep_ID, Plot_ID and Subplot into one column to create "subplot_code" column 
all_years_Rh$Subplot_code <- str_c(all_years_Rh$Rep_ID, '',all_years_Rh$Plot_ID,'', all_years_Rh$Subplot)


##Add disturbance severity column to dataset
all_years_Rh <- all_years_Rh%>%
  mutate(Severity = sapply(Subplot_code, Plot_conversion))


##Add treatment column to datasets
all_years_Rh <- all_years_Rh %>%
  mutate(Treatment = sapply(Subplot_code, Sub_plot_conversion))

all_years_Rh$soilCO2Efflux <- as.numeric(all_years_Rh$soilCO2Efflux)
all_years_Rh$Severity <- as.factor(all_years_Rh$Severity)
all_years_Rh_severity <- all_years_Rh%>%
  filter(!is.na(soilCO2Efflux))%>%
  group_by(Rep_ID, year, Severity)%>%
  summarize(ave_soilCO2Efflux = mean(soilCO2Efflux))

ggplot(all_years_Rh_severity, aes(x = Severity, y = ave_soilCO2Efflux, fill = Severity)) +
  scale_fill_manual(values=c("#000000", "#009E73", "#0072B2", "#D55E00"))+
  theme_classic()+
  geom_boxplot()+
  theme(axis.text.x= element_text(size = 20), axis.text.y= element_text(size=20), axis.title.x = element_text(size = 25), axis.title.y  = element_text(size=25), legend.title = element_blank(),  strip.text.x =element_text(size = 25), legend.text = element_blank(), legend.position = "none",plot.margin = margin(1,1,1,1, "cm")) +
  scale_y_continuous(sec.axis = sec_axis(~ .,labels = NULL)) +
  guides(col = guide_legend(nrow = 2)) +
  facet_grid(.~year,scales="free")+ 
  labs(x = "Disturbance Severity:Gross defoliation (%)", y=expression(paste("Soil Respiration (",mu*molCO[2],"  ",m^-2,"  ",sec^-1,")"))) 

##Testing water content with Rh
all_years_Rh <- all_years_Rh%>%
  mutate(water_content_percent = (1-oven.air.weight.ratio)*100)

VWC_Rh_model <- lm(soilCO2Efflux ~ water_content_percent, data = all_years_Rh)
summary(VWC_Rh_model)

ggplot(all_years_Rh, aes(x = water_content_percent, y =soilCO2Efflux )) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_point() +
  theme_classic() 

#Rh Model 
####Testing Assumptions 
##Test for outliers test: no extreme outliers
outliers <- all_years_Rh %>% 
  group_by(Severity, Treatment) %>%
  identify_outliers(soilCO2Efflux)

all_years_Rh_no_outlier <- all_years_Rh[-c(34,35,49,50,69,70,77),]

hist(all_years_Rh_no_outlier$soilCO2Efflux)

##Equality of variance test for severity and treatment (Slightly unequal data using alpha = 0.05. Equal variance using alpha = 0.1)
library(car)
leveneTest(soilCO2Efflux ~ Severity, data = all_years_Rh_no_outlier)

##Normality (Data are normal)
# Build the linear model
normality_test  <- lm(soilCO2Efflux ~ Severity*Treatment*year,
                      data = all_years_Rh)

# Create a QQ plot of residuals
ggqqplot(residuals(normality_test))
# Shapiro test of normality 
shapiro_test(residuals(normality_test))


rh_model <- aov(soilCO2Efflux  ~ Severity*Treatment*year + water_content_percent +Error(Rep_ID/Severity/Treatment/year), data = all_years_Rh)
summary(rh_model)

water_model <- aov(water_content_percent  ~ Severity*Treatment*year  +Error(Rep_ID/Severity/Treatment/year), data = all_years_Rh)
summary(water_model)

out_year_water <- with(all_years_Rh, LSD.test(water_content_percent, year,48,16.86, console = TRUE))


#######Cross-year soil micrometerology comparison: Compare soil temperature, moisture and Rs in the control across years######

##Filter out disturbed plots (Only Control)
all_years_enviro <- all_years%>%
  filter(Severity == "0")

##Filter out in NA values and summarize Rs, moisture, temperature by subplot 
all_years_enviro_2 <- all_years_enviro%>%
  filter(!is.na(Temp_C_7cm))%>%
  filter(!is.na(VWC_20cm))%>%
  group_by(Subplot_code, Year)%>%
  summarize(ave_VWC = mean(VWC_20cm), ave_temp = mean(Temp_C_7cm), ave_efflux = mean(Efflux_umol_m2_s), std_error_VWC = std.error(VWC_20cm), std_error_temp = std.error(Temp_C_7cm), std_error_efflux = std.error(Efflux_umol_m2_s))

##Make Year a factor for analysis 
all_years_enviro_2$Year <- as.factor(all_years_enviro_2$Year)

##?????
my_comparisons <- list(c("2018", "2019"), c("2018", "2020"))

##Run a MANOVA for differences across the control efflux, moisture and temperature 
##Clean dataset 
enviro_test <- all_years_enviro%>%
  ungroup()%>%
  select(Year, Efflux_umol_m2_s, VWC_20cm, Temp_C_7cm)
factor <- cbind(enviro_test$Efflux_umol_m2_s, enviro_test$Temp_C_7cm, enviro_test$VWC_20cm)

##Check for outliers within year (none) for Efflux, moisture and temperature
efflux_outliers <- enviro_test %>% 
  group_by(Year) %>%
  identify_outliers(Efflux_umol_m2_s)


enviro_test %>% 
  group_by(Year) %>%
  identify_outliers(VWC_20cm)

enviro_test %>% 
  group_by(Year) %>%
  identify_outliers(Temp_C_7cm)

##Check for normality within year (normal) for efflux temp and moisture
#Efflux normal
enviro_test %>%
  group_by(Year) %>%
  shapiro_test(Efflux_umol_m2_s)

ggqqplot(enviro_test, "Efflux_umol_m2_s", facet.by = "Year")

##moisture (slightly non normal for 2018)
enviro_test %>%
  group_by(Year) %>%
  shapiro_test(Moisture)

ggqqplot(enviro_test, "Moisture", facet.by = "Year")

##Temp normal
enviro_test %>%
  group_by(Year) %>%
  shapiro_test(Temp)

ggqqplot(enviro_test, "Temp", facet.by = "Year")


##Check for equal variance (equal) for all three 
##Efflux (equal)
enviro_test %>% 
  levene_test(Efflux ~ Year)

##moisture (equal)
enviro_test %>% 
  levene_test(Moisture ~ Year)

##Temperature (equal)
enviro_test %>% 
  levene_test(Temp ~ Year)

##Run MANOVA 
enviro_model <- manova(factor ~ Year, data=enviro_test)  
summary(enviro_model, intercept=TRUE)
summary.aov(enviro_model)

##data are normal across groups too 
shapiro_test(residuals(enviro_model))
ggqqplot(residuals(enviro_model))

##Post hoc 
enviro_test$Year <- as.factor(enviro_test$Year)
## Efflux (2018 is significantly different from 2019 and 2020)
pwc <- enviro_test %>% 
  tukey_hsd(Efflux ~ Year)
pwc

##Moisture (2018 is significantly different from 2019 and 2020)
pwc2 <- enviro_test %>% 
  tukey_hsd(Moisture ~ Year)
pwc2

##Temperature not significant 
pwc3 <- enviro_test %>% 
  tukey_hsd(Temp ~ Year)
pwc3

p1 <- ggplot(enviro_test, aes(x = Year, y = Efflux)) +
  geom_boxplot(fill = "khaki3") +
  theme_classic()+
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01), limits = c(3.6, 11.5))+
  theme(axis.text.y = element_text(size = 10), axis.title.y = element_text(size = 15), axis.title.x = element_blank(), axis.text.x = element_blank()) +
  stat_compare_means(comparisons = my_comparisons, label = "p.signif", label.y = c(9.7, 10.7), size = 8) + ylab("soil respiration")

p2 <- ggplot(enviro_test, aes(x = Year, y = Moisture)) +
  geom_boxplot(fill = "lightsteelblue2") +
  theme_classic()+
  theme(axis.text.y = element_text(size = 10), axis.title.y = element_text(size = 15), axis.title.x = element_blank(), axis.text.x = element_blank()) +
  scale_y_continuous(labels = scales::number_format(accuracy = 0.01), limits = c(3.5, 13))+
  stat_compare_means(comparisons = my_comparisons, label = "p.signif", label.y = c(11.7, 12.5), size = 8) +
  ylab("volumetric soil moisture (20cm)") 

p3 <- ggplot(enviro_test, aes(x = Year, y = Temp))+
  geom_boxplot(fill = "palegreen") +
  theme_classic()+
  scale_y_continuous(breaks = c(17.5,18.5, 19.5, 20.5), labels = scales::number_format(accuracy = 0.1), limits = c(17.2, 20.5)) +
  theme(axis.text = element_text(size = 10), axis.title = element_text(size = 15)) +
  annotate("text", x = 0.7, y = 20.4, label = "n.s.", size = 6) +
  ylab("Soil temperature (C)") 


library(gridExtra)
timeseries <- grid.arrange(p1, p2, p3, ncol=1)
g <- arrangeGrob(p1, p2, p3,ncol=1)
plot(timeseries)

ggsave("Output/control_temp_moist_efflux.png",height = 15, width = 7, units = "in", g)
