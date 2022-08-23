###########################################################
####FoRTE Soil Respiration Data Analysis
####2018-2021
####Kayla C. Mathes  
###########################################################

###Load Packages
library(ggplot2) #all plots/visualization 
library(dplyr) #all dataframe organization, cleaning and summarizing 
library(stringr)
library(plotrix)
library(reshape2)
library(ggpubr)
library(rstatix)
library(tidyverse)
library(googledrive)#upload data from googledrive 
library(lubridate)
library(scales)
library(rcartocolor)
library(gridExtra)
library(gvlma)
library(car)
library(ggprism) ##Checking linear regression assumptions
library(emmeans) ##Running estimated means analysis 
library(agricolae) ##runs the split-split plot ANOVA and LSD post-hoc tests
library(RDocumentation) ##Runs AIC model selection 



######Download Rs and Rh data for 2018-2021 from Google Drive####

# Direct Google Drive link to "FoRTE/data/soil_respiration/2021"
as_id("https://drive.google.com/drive/folders/1HHnDpTj32O-aaFavUzugzIxojf_BGEei") %>% 
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
all_2022 <- read.csv("googledrive_data/Rs_2022.csv", na.strings = c("NA","na"))
Rh_2019 <- read.csv("googledrive_data/Rh_2019.csv", na.strings = c("NA", "na"))
Rh_2020 <- read.csv("googledrive_data/Rh_2020.csv", na.strings = c("NA", "na", "#VALUE!"))
Rh_2021 <- read.csv("googledrive_data/Rh_2021.csv", na.strings = c("NA", "na", "#VALUE!"))


####Clean Dataframes#####
#leave out notes/comments column and get rid of NA efflux values 

#2018: This deletes 4 rows
all_2018_sub <- all_2018%>%
  select(!notes)%>%
  filter(!is.na(soilCO2Efflux))


#2019 (Also eliminate dates in July that only have data for Rep D): This deletes 404 rows 
all_2019_sub <- all_2019%>%
  select(!notes)%>%
  select(!HHMMSS)%>%
  filter(date != "2019-05-30") %>%
  filter(date != "2019-06-11")%>%
  filter(date != "2019-06-18")%>%
  filter(date != "2019-06-28")%>%
  filter(date != "2019-07-05")%>%
  filter(!is.na(soilCO2Efflux))

#2020: This deletes 0 rows 
all_2020_sub <- all_2020%>%
  select(!notes)%>%
  select(!HH.MM.SS)%>%
  filter(!is.na(soilCO2Efflux))

#2021: This deletes 4 rows 
all_2021_sub <- all_2021%>%
  select(!notes)%>%
  filter(!is.na(soilCO2Efflux))

#2022: 

all_2022_sub <- all_2022%>%
  select(!notes)%>%
  filter(!is.na(soilCO2Efflux))%>%
  rename(nestedSubplot = nestSubplot)


  


#####Combine all years into one dataset 
all_years <- rbind(all_2021_sub,all_2020_sub, all_2019_sub, all_2018_sub, all_2022_sub)

##Convert date into POSIXct class and add just a year column 
all_years$date <- as.POSIXct(all_years$date,format="%Y-%m-%d")
all_years$year <- format(all_years$date, format = "%Y")

##Combine Rep_ID, Plot_ID and Subplot into one column to create "subplot_code" column (This creates uniquely indentifiable code to add Disturbance Severity and Treatment columns)
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

##Create weeks groups (Groups measurements into 1 complete round of respiration) 
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
                                date == "2021-08-03" |  date == "2021-08-04" |  date == "2021-08-06" ~ "2021-08-03", 
                                date == "2021-11-12" | date == "2021-11-13" | date == "2021-11-15" ~ "2021-11-12"))

##Convert "week group" to as.Date class 
all_years_grouped$week_group <- as.Date(all_years_grouped$week_group)





######Create Full Rs, moisture and temperature timeseries####

##(Rs, soil temp and VWC were separated to get rid of NA values for each factor)
#####Rs Timeseries 
###Summarize by subplots (Collars are sudo-replicates and that variation should not be represented in the model (subplot is the smallest unit, we are interested in variation across replicates).
all_years_summary_timeseries <- all_years_grouped%>%
  group_by(Rep_ID, week_group, Severity, Treatment, Subplot_code)%>%
  summarize(soilCO2Efflux = mean(soilCO2Efflux))

##Summarize by severity by replication per round of respiration 
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


#Create date variable for x axis for timeseries 
week_group <- all_years_timeseries_severity$week_group
week_group_treatment <- all_years_timeseries_treatment$week_group



###Create an overall summary dataframe for results section (Filter through the different combinations)
all_years_summary_time_VWC <- all_years_grouped%>%
  filter(!is.na(VWC))%>%
  group_by( week_group, year, Rep_ID, Treatment, Severity)%>%
  summarize(VWC = mean(VWC))%>%
  filter(year == 2021)

 

######Figure 1: BIG Timeseries #####
##Rs Severity 
p1 <- ggplot(all_years_timeseries_severity, aes(x = week_group, y = ave_efflux, group = Severity, color = Severity)) +
  theme_classic() +
  scale_x_date(date_breaks = "3 months", date_minor_breaks = "1 month", labels = function(x) if_else(is.na(lag(x)) | !year(lag(x)) == year(x),paste(month(x, label = TRUE), "\n", year(x)),paste(month(x, label = TRUE))))+
  geom_path(size = 2, alpha = 0.9, linetype = 2) +
  geom_point(size = 10) +
  theme(axis.text.x = element_blank(),axis.title.x = element_blank(), legend.position = c(0.15,0.78), axis.text.y = element_text(size = 30), axis.title.y = element_text(size = 30),legend.text = element_text(size = 30), legend.title = element_blank(),legend.spacing.x = unit(0.1, 'cm'),legend.spacing.y = unit(0.1, 'cm'), plot.margin = margin(0,-0.4,-0.3,0.42,"cm"),panel.border = element_rect(colour = "black", fill=NA, size=1),axis.ticks.length=unit(.35, "cm")) +
  geom_errorbar(mapping=aes(x=week_group, ymin=ave_efflux - std_error_efflux, ymax=ave_efflux + std_error_efflux), size = 2, width = 10) +
  scale_color_manual(values=c("#000000", "#009E73", "#0072B2", "#D55E00")) +
  scale_y_continuous(position = "left",breaks = seq(from = 1, to = 10, by = 2),sec.axis = dup_axis(name = NULL, labels = NULL))+
  guides(color=guide_legend(nrow=2, byrow=TRUE))+
  annotate("text", x = as.Date("2018-07-07"), y = 9.5, label = "A", size = 11) +
geom_vline(xintercept = as.Date("2019-05-20"), linetype="dashed", size=1.5,color = "red3")+
  labs(x = "Date", y=expression(paste(" ",R[s]," (",mu*molCO[2]," ",m^-2," ",sec^-1,")"))) 

##Rs Treatment 
p2 <- ggplot(all_years_timeseries_treatment, aes(x = week_group, y = ave_efflux, group = Treatment, color =Treatment)) +
  theme_classic() +
  labs(x = "Date", y=expression(paste(" ",Rs," (",mu*molCO[2],"  ",m^-2,"  ",sec^-1,")"))) +
  scale_x_date(date_breaks = "3 months", date_minor_breaks = "1 month", labels = function(x) if_else(is.na(lag(x)) | !year(lag(x)) == year(x), 
                                                                      paste(month(x, label = TRUE), "\n", year(x)), 
                                                                      paste(month(x, label = TRUE))))  +
  geom_path(size = 2, alpha = 0.9, linetype = 2) +
  geom_point(size = 10) +
  theme(axis.text.x = element_blank(),axis.title.x = element_blank(), legend.position = c(0.15,0.78), axis.text.y = element_text(size = 30), axis.title.y =  element_text(size = 30),legend.text = element_text(size = 30), legend.title = element_blank(),legend.spacing.x = unit(0.1, 'cm'),legend.spacing.y = unit(0.1, 'cm'), plot.margin = margin(0,0.24,-0.3, -0.2,"cm"),panel.border = element_rect(colour = "black", fill=NA, size=1),axis.ticks.length=unit(.35, "cm")) +
  geom_errorbar(mapping=aes(x=week_group, ymin=ave_efflux - std_error_efflux, ymax=ave_efflux + std_error_efflux), size = 2, width = 10) +
  scale_color_manual(values=c("#A6611A", "#018571")) +
  scale_y_continuous(position = "right",breaks = seq(from = 1, to = 10, by = 2),sec.axis = dup_axis(name = NULL, labels = NULL))+
  guides(color=guide_legend(nrow=2, byrow=TRUE)) +
  annotate("text", x = as.Date("2018-07-07"), y = 8, label = "B", size = 11) +
  geom_vline(xintercept = as.Date("2019-05-20"), linetype="dashed", size=1.5,color = "red3")

##Temp Severity 
p3 <- ggplot(all_years_timeseries_severity_temp, aes(x = week_group, y = ave_temp, group = Severity, color = Severity)) +
  labs(x = "Date", y=expression(paste('Temp ('*~degree*C*')'))) +
  scale_x_date(date_breaks = "3 months", date_minor_breaks = "1 month", labels = function(x) if_else(is.na(lag(x)) | !year(lag(x)) == year(x), 
                                                                      paste(month(x, label = TRUE), "\n", year(x)), 
                                                                      paste(month(x, label = TRUE))))  +
  geom_path(size = 2, alpha = 0.9, linetype = 2) +
  geom_point(size = 10) +
  geom_errorbar(mapping=aes(x=week_group, ymin=ave_temp - std_error_temp, ymax=ave_temp + std_error_temp), size = 2,width = 10) +
  theme_classic() +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank(),legend.position = "none", axis.text.y = element_text(size = 30), axis.title.y = element_text(size = 30), plot.margin = margin(0,-.4,-0.3,0.17, "cm"),panel.border = element_rect(colour = "black", fill=NA, size=1),axis.ticks.length=unit(.35, "cm")) +
  scale_color_manual(values=c("#000000", "#009E73", "#0072B2", "#D55E00")) +
  scale_y_continuous(position = "left",breaks = seq(from = 1, to = 30, by = 5),sec.axis = dup_axis(name = NULL, labels = NULL)) +
  annotate("text", x = as.Date("2018-07-07"), y = 21.5, label = "C", size = 11) +
  geom_vline(xintercept = as.Date("2019-05-20"), linetype="dashed", size=1.5,color = "red3")

##Temp Treatment 
p4 <- ggplot(all_years_timeseries_treatment_temp, aes(x = week_group, y = ave_temp, group = Treatment, color = Treatment)) +
  labs(x = "Date", y=expression(paste('Temp ('*~degree*C*')'))) +
  scale_x_date(date_breaks = "3 months", date_minor_breaks = "1 month", labels = function(x) if_else(is.na(lag(x)) | !year(lag(x)) == year(x), 
                                                                      paste(month(x, label = TRUE), "\n", year(x)), 
                                                                      paste(month(x, label = TRUE)))) +
  geom_path(size = 2, alpha = 0.9, linetype = 2) +
  geom_point(size = 10) +
  geom_errorbar(mapping=aes(x=week_group, ymin=ave_temp - std_error_temp, ymax=ave_temp + std_error_temp), size = 2,width = 10) +
  theme_classic() +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank(),legend.position = "none", axis.text.y  = element_text(size = 30), axis.title.y = element_text(size = 30), plot.margin = margin(0,0,-0.3,-0.2, "cm"),panel.border = element_rect(colour = "black", fill=NA, size=1),axis.ticks.length=unit(.35, "cm")) +
  scale_color_manual(values=c("#A6611A", "#018571")) +
  scale_y_continuous(position = "right",breaks = seq(from = 1, to = 30, by = 5),sec.axis = dup_axis(name = NULL, labels = NULL)) +
  annotate("text", x = as.Date("2018-07-07"), y = 21, label = "D", size = 11) +
 geom_vline(xintercept = as.Date("2019-05-20"), linetype="dashed", size=1.5, color = "red3")

##VWC Severity 
p5 <- ggplot(all_years_timeseries_severity_VWC, aes(x = week_group, y = ave_VWC, group = Severity, color = Severity)) +
  labs(x = "Date", y= "VWC (%)") +
  scale_x_date(date_breaks = "3 months", date_minor_breaks = "1 month", labels = function(x) if_else(is.na(lag(x)) | !year(lag(x)) == year(x), 
                                                                      paste(month(x, label = TRUE), "\n", year(x)), 
                                                                      paste(month(x, label = TRUE)))) +
  geom_path(size = 2, alpha = 0.9, linetype = 2) +
  geom_point(size = 10) +
  geom_errorbar(mapping=aes(x=week_group, ymin=ave_VWC - std_error_VWC, ymax=ave_VWC + std_error_VWC), size =2, width = 10) +
  theme_classic() +
  scale_color_manual(values=c("#000000", "#009E73", "#0072B2", "#D55E00")) +
  scale_y_continuous(position = "left",breaks = seq(from = 1, to = 30, by = 5),sec.axis = dup_axis(name = NULL, labels = NULL)) +
  theme(axis.text.y = element_text(size = 30), axis.title.y = element_text(size = 30), legend.position = "none", axis.text.x = element_text(size = 30),  axis.title.x = element_blank(), plot.margin = margin(0,-.40,0,0.37, "cm"),panel.border = element_rect(colour = "black", fill=NA, size=1),axis.ticks.length=unit(.35, "cm")) +
  guides(color=guide_legend(nrow=2, byrow=TRUE)) +
  annotate("text", x = as.Date("2018-07-07"), y = 19, label = "E", size = 11) +
  geom_vline(xintercept = as.Date("2019-05-20"), linetype="dashed", size=1.5, color = "red3")
  
all_years_timeseries_treatment_VWC$week_group <- as.Date(all_years_timeseries_treatment_VWC$week_group)

##VWC Treatment 
p6 <- ggplot(all_years_timeseries_treatment_VWC, aes(x = week_group, y = ave_VWC, group = Treatment, color = Treatment)) +
  labs(x = "Date", y= "VWC (%)") +
  geom_path(size = 2, alpha = 0.9, linetype = 2) +
  geom_point(size = 10) +
  geom_errorbar(mapping=aes(x=week_group, ymin=ave_VWC - std_error_VWC, ymax=ave_VWC + std_error_VWC), size = 2, width = 10) +
  theme_classic() +
  scale_color_manual(values=c("#A6611A", "#018571")) +
  scale_y_continuous(position = "right",breaks = seq(from = 1, to = 30, by = 5),sec.axis = dup_axis(name = NULL, labels = NULL)) +
  theme(axis.text.y  = element_text(size = 30), axis.title.y  = element_text(size = 30), legend.position = "none", axis.text.x = element_text(size = 30),  axis.title.x = element_blank(), plot.margin = margin(0,.20,0,-0.2, "cm"), strip.placement = "outside",panel.border = element_rect(colour = "black", fill=NA, size=1),axis.ticks.length=unit(.35, "cm")) +
  guides(color=guide_legend(nrow=2, byrow=TRUE)) +
  scale_x_date(date_breaks = "3 months", date_minor_breaks = "1 month", labels = function(x) if_else(is.na(lag(x)) | !year(lag(x)) == year(x), 
                                                                       paste(month(x, label = TRUE), "\n", year(x)), 
                                                                       paste(month(x, label = TRUE)))) +
  annotate("text", x = as.Date("2018-07-07"), y = 18, label = "F", size = 11) +
  geom_vline(xintercept = as.Date("2019-05-20"), linetype="dashed", size=1.5, color = "red3")

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

ggsave(path = "Manuscript_figures", filename = "Figure_1.png", height = 20, width = 30, units = "in", g_timeseries)



#######Summarize per year######
##Inlude only months that were analyzed across all years (ie.Not May and June in 2019)
all_years_gs_nov <- all_years%>%
  filter(date == "2018-11-15" |date == "2018-11-16"|date == "2018-11-17"|  date == "2019-07-08" | date == "2019-07-09" | date == "2019-07-12" | date == "2019-07-16" | date == "2019-07-17" | date == "2019-07-18" | date == "2019-07-19" | date == "2019-07-24" | date == "2019-07-25" | date == "2019-07-26" | date == "2019-08-01" | date == "2019-08-02" | date == "2019-08-03" | date == "2019-11-11" |date == "2019-11-12" | date == "2020-07-07" | date == "2020-07-08" |date == "2020-07-09" | date == "2020-07-24" | date == "2020-07-25" | date == "2020-08-05" | date == "2020-08-06" | date == "2020-11-16" |date == "2020-11-17"|date == "2020-11-18"| date == "2018-07-27" | date == "2018-08-03" | date == "2018-08-10" | date == "2018-08-14" | date == "2021-07-06" |date == "2021-07-09" |date == "2021-07-10" | date == "2021-08-03" | date == "2021-08-04" | date == "2021-08-06" | date == "2021-11-12" |date == "2021-11-13"|date == "2021-11-15" | date == "2022-06-28" | date == "2022-07-03"| date == "2022-06-27" | date == "2022-07-18" | date == "2022-07-19")


###Summarize by subplots (Collars are sudo-replicates and they variation should not be represented in the model subplot is the smallest unit, we are interested in variation across replicates).
all_years_summary <- all_years_gs_nov%>%
  group_by(Rep_ID, year, Severity, Treatment)%>%
  summarize(soilCO2Efflux = mean(soilCO2Efflux), soilTemp = mean(soilTemp, na.rm=TRUE), VWC =mean(VWC,na.rm=TRUE))

####Summarize and visualize Data by severity ######

##Summarize severity by replications per year 
all_years_summary_severity <- all_years_gs_nov%>%
  group_by(year, Rep_ID, Severity)%>%
  summarize(ave_efflux = mean(soilCO2Efflux), std_error_efflux = std.error(soilCO2Efflux))

all_years_summary_severity$year <- as.factor(all_years_summary_severity$year)


######Visualize Rs data by severity by Year##
###Figure 2
##Boxplot of replicate averages across disturbance severity per year 
ggplot(all_years_summary_severity,aes(x = Severity, y = ave_efflux, fill = Severity)) +
  scale_fill_manual(values=c("#000000", "#009E73", "#0072B2", "#D55E00"))+
  theme_classic2()+
  geom_boxplot()+
  theme(axis.text.x= element_text(size = 30), axis.text.y= element_text(size=30), axis.title.x = element_text(size = 35), axis.title.y  = element_text(size=35), legend.title = element_blank(),  strip.text.x =element_text(size = 30), legend.text = element_blank(), legend.position = "none",axis.ticks.length=unit(.35, "cm"),prism.ticks.length = unit(0.25, "cm"), panel.background = element_rect(fill = NA, color = "black")) +
  scale_y_continuous(breaks=pretty_breaks(n=7),sec.axis = sec_axis(~ .,labels = NULL, breaks=pretty_breaks(n=7))) +
  facet_grid(.~year,scales="free")+ 
  guides(col = guide_legend(nrow = 2)) +
  labs(x = "Severity (% Gross Defoliation)", y=expression(paste(" ",R[s]," (",mu*molCO[2],"  ",m^-2,"  ",sec^-1,")"))) +
  geom_rect(data=data.frame(year='2018'), inherit.aes=FALSE,
            xmin = 0, xmax = 5, ymin= 0, ymax= Inf,
            fill = 'grey20', alpha = 0.2)
ggsave(path ="Manuscript_figures", filename = "Figure_2.png",height = 10, width = 15 , units = "in")


########Create dataframe and boxplot of only control Rs by year to show the control Rs has been increasing####
# all_years_summary_severity_control <- all_years_summary_severity%>%
#   filter(Severity == 0)

# ggplot(all_years_summary_severity_control,aes(x = Severity, y = ave_efflux, fill = Severity)) +
#   scale_fill_manual(values=c("#000000"))+
#   theme_classic()+
#   geom_boxplot(width=0.3)+
#   theme(axis.text.x= element_text(size = 30), axis.text.y= element_text(size=30), axis.title.x = element_text(size = 35), axis.title.y  = element_text(size=35), legend.title = element_blank(),  strip.text.x =element_text(size = 30), legend.text = element_blank(), legend.position = "none",axis.ticks.length=unit(.35, "cm"),prism.ticks.length = unit(0.25, "cm")) +
#   scale_y_continuous(breaks=pretty_breaks(n=7),sec.axis = sec_axis(~ .,labels = NULL, breaks=pretty_breaks(n=7))) +
#   facet_grid(.~year,scales="free")+ 
#   guides(col = guide_legend(nrow = 2)) +
#   labs(x = "Severity (% Gross Defoliation)", y=expression(paste(" ",R[s]," (",mu*molCO[2],"  ",m^-2,"  ",sec^-1,")"))) 
# ggsave("severity_boxplot_control.png",height = 7, width = 10 , units = "in")




#####Summarize data by Treatment######
##Summarize treatment by replications per year 
all_years_summary_treatment <- all_years_gs_nov%>%
  group_by(year, Rep_ID, Treatment)%>%
  summarize(ave_efflux = mean(soilCO2Efflux), std_error_efflux = std.error(soilCO2Efflux))

####Figure 3
####Create a boxplot of Rs by treatment by year 
ggplot(all_years_summary_treatment,aes(x = Treatment, y = ave_efflux, fill = Treatment)) +
  scale_fill_manual(values=c("#A6611A", "#018571"))+
  theme_classic()+
  geom_boxplot()+
  theme(axis.text.x= element_text(size = 30), axis.text.y= element_text(size=30), axis.title.x = element_text(size = 35), axis.title.y  = element_text(size=35), legend.title = element_blank(),  strip.text.x =element_text(size = 30), legend.text = element_blank(), legend.position = "none", axis.ticks.length=unit(.35, "cm"),panel.background = element_rect(fill = NA, color = "black")) +
  facet_grid(.~year,scales="free")+ 
  guides(col = guide_legend(nrow = 2)) +
  scale_y_continuous(breaks = c(4,5,6,7,8),sec.axis = sec_axis(~ .,labels = NULL,breaks = c(4,5,6,7,8))) +
  labs(x = "Treatment", y=expression(paste(" ",R[s]," (",mu*molCO[2],"  ",m^-2,"  ",sec^-1,")"))) +
  geom_rect(data=data.frame(year='2018'), inherit.aes=FALSE,
            xmin = 0, xmax = 5, ymin= 0, ymax= Inf,
            fill = 'grey20', alpha = 0.2)
ggsave(path = "Manuscript_figures", filename = "Figure_3.png",height = 10, width = 15 , units = "in")


#########Split plot Model for absolute data#####

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

##Trying to export model summary 
rs_model_out <- capture.output(summary(rs_model))
cat("Rs_ANOVA", rs_model_out, file = "Rs_ANOVA")

#Rs model with VWC covariate 
rs_model_VWC <- aov(soilCO2Efflux  ~ Severity*Treatment*year +VWC + Error(Rep_ID/Severity/Treatment/year), data = all_years_summary)
summary(rs_model_VWC)

#Rs model with Temp covariate
rs_model_temp <- aov(soilCO2Efflux  ~ Severity*Treatment*year +soilTemp + Error(Rep_ID/Severity/Treatment/year), data = all_years_summary)
summary(rs_model_temp)

###Comparing AIC Values for Rs models: Lowest AIC with model with VWC as covariate 
rs_lm <- lm(soilCO2Efflux  ~ Severity*Treatment*year, data = all_years_summary)
summary(rs_lm)
rs_lm_temp <- lm(soilCO2Efflux  ~ Severity*Treatment*year +soilTemp, data = all_years_summary)
summary(rs_lm_temp)
rs_lm_VWC <- lm(soilCO2Efflux  ~ Severity*Treatment*year +VWC, data = all_years_summary)
summary(rs_lm_VWC)

AIC(rs_lm,rs_lm_temp,rs_lm_VWC)

######Post hoc analysis: LSD test for Rs/VWC/temp models without covariates for all significant values in the model

#Rs model
out_year_severity_rs <- with(all_years_summary, LSD.test(soilCO2Efflux, Severity:year,72, 0.45, console = TRUE))

out_year_severity_rs_VWC <- with(all_years_summary, LSD.test(soilCO2Efflux, Severity:year,71, 0.441, console = TRUE))

out_year_rs <- with(all_years_summary,LSD.test(soilCO2Efflux,year,72,0.703,console=TRUE))

#VWC Model
out_year_VWC <- with(all_years_summary,LSD.test(VWC,year,72,0.76,console=TRUE))

#Temp Model
out_year_temp <- with(all_years_summary,LSD.test(soilTemp,year,72,0.75,console=TRUE))



###Model with VWC as dependent Variable 
VWC_model <- aov(VWC  ~ Severity*Treatment*year + Error(Rep_ID/Severity/Treatment/year), data = all_years_summary)
summary(VWC_model)

#Model with soil temperature as dependent Variable 
temp_model <- aov(soilTemp  ~ Severity*Treatment*year + Error(Rep_ID/Severity/Treatment/year), data = all_years_summary)
summary(temp_model)


#####Heterotrophic Respiration Analysis 2019-2021#######
##Clean Dataframe 
#2019
Rh_2019_sub <- Rh_2019%>%
  select(Rep_ID, Plot_ID, Subplot, soilCO2Efflux, soilTemp, date_measured,oven.air.weight.ratio, dry_weight)%>%
  filter(!is.na(soilCO2Efflux))

#2020
Rh_2020_sub <- Rh_2020%>%
  select(Rep_ID, Plot_ID, Subplot, soilCO2Efflux, soilTemp, date_measured, oven.air.weight.ratio,dry_weight)%>%
  filter(!is.na(soilCO2Efflux))

#2021
Rh_2021_sub <- Rh_2021%>%
  select(Rep_ID, Plot_ID, Subplot, soilCO2Efflux, soilTemp, date_measured, oven.air.weight.ratio,dry_weight)%>%
  filter(!is.na(soilCO2Efflux))


#####Combine all years into one dataset 
all_years_Rh <- rbind(Rh_2019_sub, Rh_2020_sub, Rh_2021_sub)

##Convert date into POSIXct class and add a year only column 
all_years_Rh$date_measured <- as.POSIXct(all_years_Rh$date_measured,format="%Y-%m-%d")
all_years_Rh$year <- format(all_years_Rh$date, format = "%Y")


##Combine Rep_ID, Plot_ID and Subplot into one column to create "subplot_code" column. This will be used to add Severity and treatment values
all_years_Rh$Subplot_code <- str_c(all_years_Rh$Rep_ID, '',all_years_Rh$Plot_ID,'', all_years_Rh$Subplot)


##Add disturbance severity column to dataset.Function created previously 
all_years_Rh <- all_years_Rh%>%
  mutate(Severity = sapply(Subplot_code, Plot_conversion))


##Add treatment column to datasets. Function created previously 
all_years_Rh <- all_years_Rh %>%
  mutate(Treatment = sapply(Subplot_code, Sub_plot_conversion))

all_years_Rh$soilCO2Efflux <- as.numeric(all_years_Rh$soilCO2Efflux)
all_years_Rh$Severity <- as.factor(all_years_Rh$Severity)


###Convert Rh to umol/gram*sec. Adjusting the surface by 9cm/6cm to account for mouth of mason jar, adjusting 486cm^3/991cm^3 to adjust from soil chamber headspace volume to mason jar headspace volume and dividing by dry weight of each mason jar in grams. 

all_years_Rh <- all_years_Rh%>%
  mutate(soilCO2Efflux_umolg = soilCO2Efflux*(9/6)*(486)*(1/991)/dry_weight)


###Create water content variable from oven to air dry weight ratio
all_years_Rh <- all_years_Rh%>%
  mutate(water_content_percent = (1-oven.air.weight.ratio)*100)

##Summarize Rh across Rep, treatment, severity and year
all_years_Rh_summary <- all_years_Rh%>%
  filter(!is.na(soilCO2Efflux_umolg))%>%
  group_by(Rep_ID, year, Severity, Treatment)%>%
  summarize(ave_soilCO2Efflux_umolg = mean(soilCO2Efflux_umolg),ave_water_content_percent = mean(water_content_percent), ave_soilTemp = mean(soilTemp))

##Summarize Rh across Rep, severity and year only  
all_years_Rh_severity <- all_years_Rh%>%
  filter(!is.na(soilCO2Efflux_umolg))%>%
  group_by(Rep_ID, year, Severity)%>%
  summarize(ave_soilCO2Efflux_umolg = mean(soilCO2Efflux_umolg))

##Summarize Rh across Rep, treatment and year only  
all_years_Rh_treatment <- all_years_Rh%>%
  filter(!is.na(soilCO2Efflux_umolg))%>%
  group_by(Rep_ID, year, Treatment)%>%
  summarize(ave_soilCO2Efflux_umolg = mean(soilCO2Efflux_umolg))


###Plot boxplots of absolute Rh values 
ggplot(all_years_Rh_severity, aes(x = Severity, y = ave_soilCO2Efflux_umolg, fill = Severity)) +
  scale_fill_manual(values=c("#000000", "#009E73", "#0072B2", "#D55E00"))+
  theme_classic()+
  geom_boxplot()+
  theme(axis.text.x= element_text(size = 30), axis.text.y= element_text(size=30), axis.title.x = element_text(size = 35), axis.title.y  = element_text(size=35), legend.title = element_blank(),  strip.text.x =element_text(size = 25), legend.text = element_blank(), legend.position = "none",panel.background = element_rect(fill = NA, color = "black")) +
  scale_y_continuous(sec.axis = sec_axis(~ .,labels = NULL)) +
  guides(col = guide_legend(nrow = 2)) +
  facet_grid(.~year,scales="free")+ 
  labs(x = "Severity (% Gross defoliation)", y=expression(paste(" ",R[h]," (",mu*molCO[2],"  ",g^-1,"  ",sec^-1,")"))) 


##Plot Boxplot of absolute Rh value 
ggplot(all_years_Rh_treatment, aes(x = Treatment, y = ave_soilCO2Efflux_umolg, fill = Treatment)) +
  theme_classic()+
  geom_boxplot()+
  theme(axis.text.x= element_text(size = 30), axis.text.y= element_text(size=30), axis.title.x = element_text(size = 35), axis.title.y  = element_text(size=35), legend.title = element_blank(),  strip.text.x =element_text(size = 25), legend.text = element_blank(), legend.position = "none",panel.background = element_rect(fill = NA, color = "black")) +
  scale_y_continuous(sec.axis = sec_axis(~ .,labels = NULL)) +
  guides(col = guide_legend(nrow = 2)) +
  facet_grid(.~year,scales="free")+ 
  labs(x = "Treatment", y=expression(paste(" ",R[h]," (",mu*molCO[2],"  ",g^-1,"  ",sec^-1,")"))) 

##Testing water content with Rh
VWC_Rh_model <- lm(soilCO2Efflux_umolg ~ water_content_percent, data = all_years_Rh)
summary(VWC_Rh_model)

ggplot(all_years_Rh, aes(x = water_content_percent, y =soilCO2Efflux_umolg )) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_point() +
  theme_classic() 


#Rh Model 
####Testing Assumptions 
##Test for outliers test: one extreme outlier
 all_years_Rh_summary %>% 
  group_by(Severity, Treatment) %>%
  identify_outliers(ave_soilCO2Efflux_umolg)


##Equality of variance test for severity and treatment = Variances are equal 
leveneTest(ave_soilCO2Efflux_umolg ~ Severity, data = all_years_Rh_summary)

##Normality (Data not normal) But sample size over 30?
# Build the linear model
normality_test  <- lm(ave_soilCO2Efflux_umolg ~ Severity*Treatment*year,
                      data = all_years_Rh_summary)
# Create a QQ plot of residuals
ggqqplot(residuals(normality_test))
# Shapiro test of normality 
shapiro_test(residuals(normality_test))

###Log Transform Data 
all_years_Rh_summary_transformed <- all_years_Rh_summary%>%
  mutate(ave_soilCO2Efflux_umolg_transformed = log(ave_soilCO2Efflux_umolg))

normality_test_log  <- lm(ave_soilCO2Efflux_umolg_transformed ~ Severity*Treatment*year,
                          data = all_years_Rh_summary_transformed)
# Create a QQ plot of residuals
ggqqplot(residuals(normality_test_log))
# Shapiro test of normality = Data are normal with alpha <0.01
shapiro_test(residuals(normality_test_log))

#####Run Split-split model for transformed Rh Data
##Only Year is significant 
rh_model <- aov(ave_soilCO2Efflux_umolg_transformed  ~ Severity*Treatment*year + Error(Rep_ID/Severity/Treatment/year), data = all_years_Rh_summary_transformed)
summary(rh_model)


#Rh model with VWC as covariate 
rh_model_VWC <- aov(ave_soilCO2Efflux_umolg_transformed  ~ Severity*Treatment*year +ave_water_content_percent + Error(Rep_ID/Severity/Treatment/year), data = all_years_Rh_summary_transformed)
summary(rh_model_VWC)

##AIC Values for Rh Models 
rh_lm_VWC <- lm(ave_soilCO2Efflux_umolg_transformed  ~ Severity*Treatment*year +ave_water_content_percent, data = all_years_Rh_summary_transformed)
summary(rh_lm_VWC)

rh_lm <- lm(ave_soilCO2Efflux_umolg_transformed  ~ Severity*Treatment*year, data = all_years_Rh_summary_transformed)
AIC(rh_lm_VWC,rh_lm )

###Created Adjusted Rh values to 15% VWC for figures 
all_years_Rh_adjusted <- all_years_Rh%>%
  mutate(adjusted_15 = (VWC_Rh_model$coefficients[2]*15+VWC_Rh_model$coefficients[1])+ (soilCO2Efflux_umolg-(VWC_Rh_model$coefficients[2]*water_content_percent+VWC_Rh_model$coefficients[1])))

##Create a treatment ONLY summary (Because Treatment main effect was significant, not interaction term)
all_years_Rh_treatment_ONLY <-  all_years_Rh%>%
  filter(!is.na(soilCO2Efflux_umolg))%>%
  group_by(Rep_ID,Treatment)%>%
  summarize(ave_soilCO2Efflux_umolg = mean(soilCO2Efflux_umolg))

Rh_treatment_averages <- all_years_Rh_treatment_ONLY%>%
  group_by(Treatment)%>%
  summarize(ave_Rh = mean(ave_soilCO2Efflux_umolg))


##Create a severity ONLY summary (Because severity main effect was significant, not interaction term)
all_years_Rh_severity_ONLY <-  all_years_Rh%>%
  filter(!is.na(soilCO2Efflux_umolg))%>%
  group_by(Rep_ID,Severity)%>%
  summarize(ave_soilCO2Efflux_umolg = mean(soilCO2Efflux_umolg))

##Plot Rh treatment all years
Rh1 <- ggplot(all_years_Rh_treatment_ONLY, aes(x = Treatment, y = ave_soilCO2Efflux_umolg, fill = Treatment)) +
  theme_classic()+
  scale_fill_manual(values=c("#A6611A", "#018571"))+
  geom_boxplot(width = 0.5)+
  theme(axis.text.x= element_text(size = 30), axis.text.y= element_text(size=30), axis.title.x = element_text(size = 35), axis.title.y  = element_text(size=35), legend.title = element_blank(),  strip.text.x =element_text(size = 30), legend.text = element_blank(), legend.position = "none",panel.background = element_rect(fill = NA, color = "black")) +
  scale_y_continuous(sec.axis = sec_axis(~ .,labels = NULL),position="right") +
  guides(col = guide_legend(nrow = 2)) +
  labs(x = "Treatment", y=expression(paste(" ",R[h]," (",mu*molCO[2],"  ",g^-1,"  ",sec^-1,")"))) +annotate("text", x = 0.6, y = 0.0063, label = "B", size = 11)

  

##Plot Rh Severity all years
Rh2 <- ggplot(all_years_Rh_severity_ONLY, aes(x = Severity, y = ave_soilCO2Efflux_umolg, fill = Severity)) +
  theme_classic()+
  scale_fill_manual(values=c("#000000", "#009E73", "#0072B2", "#D55E00"))+
  geom_boxplot()+
  theme(axis.text.x= element_text(size = 30), axis.text.y= element_text(size=30), axis.title.x = element_text(size = 35), axis.title.y  = element_text(size=35), legend.title = element_blank(),  strip.text.x =element_text(size = 30), legend.text = element_blank(), legend.position = "none",panel.background = element_rect(fill = NA, color = "black")) +
  scale_y_continuous(sec.axis = sec_axis(~ .,labels = NULL)) +
  guides(col = guide_legend(nrow = 2)) +
  labs(x = "Severity", y=expression(paste(" ",R[h]," (",mu*molCO[2],"  ",g^-1,"  ",sec^-1,")"))) +
  annotate("text", x = 0.6, y = 0.0076, label = "A", size = 11)


#Rh  Multipanel Figure 
Rh_1_grob <- ggplotGrob(Rh1)
Rh_2_grob <- ggplotGrob(Rh2)


layout <- rbind(c(1,2))


Rh <- grid.arrange(Rh_2_grob,Rh_1_grob, layout_matrix=layout)

ggsave(path = "Manuscript_figures",filename = "Figure_4.png",height = 10, width = 20, units = "in", Rh)

#Post Hoc
out_severity_Rh <- with(all_years_Rh_summary_transformed, LSD.test(ave_soilCO2Efflux_umolg_transformed,Severity,8,0.0391, console = TRUE))

out_severity_Rh <- with(all_years_Rh_summary_transformed, LSD.test(ave_soilCO2Efflux_umolg_transformed,Treatment,11,0.0329, console = TRUE))


####Q10 Calculations####

##Creating a dataframe with average Rs across Rep, severity, treatment and round of measurement (date)
all_years_Q10 <- all_years_gs_nov%>%
  filter(!is.na(soilTemp))%>%
  filter(year!=2018)%>%
  group_by(Rep_ID, Severity, Treatment, date,year)%>%
  summarize(ave_soilCO2Efflux = mean(soilCO2Efflux), ave_soilTemp = mean(soilTemp))

#Scatterplots 
##All datapoints by day of measurement 
ggplot(all_years_Q10, aes(x = ave_soilTemp, y = ave_soilCO2Efflux)) +
  geom_point()

##All Data points by day of measurement faceted by severity
Q10_1 <- ggplot(all_years_Q10, aes(x = ave_soilTemp, y = ave_soilCO2Efflux, group = Severity)) +
  geom_point(aes(color = Severity), size = 3)+
  scale_color_manual(values=c("#000000", "#009E73", "#0072B2", "#D55E00")) +
  geom_smooth(method = "nls",
              method.args = list(formula = y ~ a*exp(b*x),
                                 start = list(a = 0.8, b = 0.1)),
              data = all_years_Q10,
              se = FALSE,
              aes(color = Severity)) +
  theme(axis.text.x = element_text(size = 30), axis.text.y= element_text(size=35), axis.title.x = element_text(size = 30),   axis.title.y  = element_text(size=35), legend.title = element_text(size = 25),  strip.text.x =element_text(size = 25), legend.text = element_text(size = 20), panel.background = element_rect(fill = NA, color = "black"),legend.position = c(0.13, 0.67)) +
  labs(x = expression("Soil Temperature ("*~degree*C*")"),y=expression(paste(" ",R[s]," (",mu*molCO[2],"  ",m^-2,"  ",sec^-1,")")))+
annotate("text", x = 0.6, y = 14.5, label = "A", size = 12)

##All Data points by day of measurement faceted by treatment
Q10_2 <- ggplot(all_years_Q10, aes(x = ave_soilTemp, y = ave_soilCO2Efflux, group = Treatment)) +
  geom_point(aes(color = Treatment), size = 3)+
  scale_color_manual(values=c("#A6611A", "#018571")) +
  geom_smooth(method = "nls",
              method.args = list(formula = y ~ a*exp(b*x),
                                 start = list(a = 0.8, b = 0.1)),
              data = all_years_Q10,
              se = FALSE,
              aes(color = Treatment)) +
  theme(axis.text.x = element_text(size = 30), axis.text.y= element_text(size=35), axis.title.x = element_text(size = 30),   axis.title.y  = element_text(size=35), legend.title = element_text(size = 25),  strip.text.x =element_text(size = 25), legend.text = element_text(size = 20), panel.background = element_rect(fill = NA, color = "black"),legend.position = c(0.13, 0.67)) +
  labs(x = expression("Soil Temperature ("*~degree*C*")"), y=expression(paste(" ",R[s]," (",mu*molCO[2],"  ",m^-2,"  ",sec^-1,")")))+
  scale_y_continuous(sec.axis = sec_axis(~ .,labels = NULL), position = "right")+
  annotate("text", x = 0.6, y = 14.5, label = "B", size = 12)



###Severity, treatment, Replicate Exponential model 
##Exponential Model 
model_Q10 <- all_years_Q10%>%
  group_by(Rep_ID, Severity,Treatment)%>%
  do(model = nls(ave_soilCO2Efflux ~ a * exp(b * ave_soilTemp), start = list(a = 0.8,b = 0.1),data = .))%>%
  ungroup()

##Extracting parameters 
param_model_Q10 <-  model_Q10 %>%
  mutate(param_efflux = lapply(model, broom::tidy)) %>%
  unnest(param_efflux) %>%
  select(Rep_ID, Severity,Treatment, term, estimate, std.error) %>%
  pivot_wider(names_from = term, values_from = estimate) 

##Q10 value dataframe
param_model_Q10_b <- param_model_Q10%>%
  select(Rep_ID, Severity,Treatment,  b)%>%
  filter(!is.na(b))%>%
  mutate(Q10 = exp(10*b))

##Intercept value dataframe
param_model_Q10_a<- param_model_Q10%>%
  select(Rep_ID, Severity,Treatment, a)%>%
  filter(!is.na(a))%>%
  rename(intercept = a)




param_model_Q10_10 <-  merge(param_model_Q10_a,param_model_Q10_b,by= c("Rep_ID", "Severity", "Treatment"))%>%
mutate(BR = (ave_soilCO2Efflux = intercept * exp(b * 10)))

##Boxplots of Q10 Values and intercept values 
##Severity Q10
Q10_3 <- ggplot(param_model_Q10_b, aes(x = Severity, y = Q10, fill = Severity)) +
  theme_classic()+
  scale_fill_manual(values=c("#000000", "#009E73", "#0072B2", "#D55E00"))+
  geom_boxplot()+
  theme(axis.text.x= element_blank(), axis.text.y= element_text(size=30), axis.title.x = element_blank(), axis.title.y  = element_text(size=35), legend.title = element_blank(),  strip.text.x =element_text(size = 25), legend.text = element_blank(), legend.position = "none",panel.background = element_rect(fill = NA, color = "black")) +
  scale_y_continuous(sec.axis = sec_axis(~ .,labels = NULL))+
  labs(x = "Severity (% Gross Defoliation)", y = "Q10")+
  annotate("text", x = 0.6, y = 3, label = "C", size = 12) +
  annotate("text", x = 4.2, y = 3, label = "ns", size = 10)

##Severity Intercept
Q10_5 <- ggplot(param_model_Q10_10, aes(x = Severity, y = BR, fill = Severity)) +
  theme_classic()+
  scale_fill_manual(values=c("#000000", "#009E73", "#0072B2", "#D55E00"))+
  geom_boxplot()+
  theme(axis.text.x= element_text(size = 30), axis.text.y= element_text(size=30), axis.title.x = element_text(size = 35), axis.title.y  = element_text(size=35), legend.title = element_blank(),  strip.text.x =element_text(size = 25), legend.text = element_blank(), legend.position = "none",panel.background = element_rect(fill = NA, color = "black")) +
  scale_y_continuous(sec.axis = sec_axis(~ .,labels = NULL))+
  labs(x = "Severity (% Gross Defoliation)", y=expression(paste(" ",BR," (",mu*molCO[2],"  ",m^-2,"  ",sec^-1,")")))+
  annotate("text", x = 0.6, y = 5, label = "D", size = 12) +
  annotate("text", x = 1.2, y = 4.8, label = "a", size = 11) +
  annotate("text", x = 2, y = 4.2, label = "ab", size = 11) +
  annotate("text", x = 3, y = 3.8, label = "b", size = 11) +
  annotate("text", x = 4, y = 3.2, label = "b", size = 11)
  

##Treatment Q10
##Boxplots of Q10 Values and intercept values 
Q10_4 <- ggplot(param_model_Q10_b, aes(x = Treatment, y = Q10, fill = Treatment)) +
  theme_classic()+
  scale_fill_manual(values=c("#A6611A", "#018571"))+
  geom_boxplot(width = 0.5)+
  theme(axis.text.x= element_blank(), axis.text.y= element_text(size=30), axis.title.x = element_blank(), axis.title.y  = element_text(size=35), legend.title = element_blank(),  strip.text.x =element_text(size = 25), legend.text = element_blank(), legend.position = "none",panel.background = element_rect(fill = NA, color = "black")) +
  scale_y_continuous(sec.axis = sec_axis(~ .,labels = NULL),position = "right")+
  labs(x = "Treatment", y = "Q10")+
  annotate("text", x = 0.5, y = 3, label = "E", size = 12) +
  annotate("text", x = 2.4, y = 3, label = "ns", size = 11)

##Treatment BR
Q10_6 <- ggplot(param_model_Q10_10, aes(x = Treatment, y = BR, fill = Treatment)) +
  theme_classic()+
  scale_fill_manual(values=c("#A6611A", "#018571"))+
  geom_boxplot(width = 0.5)+
  theme(axis.text.x= element_text(size = 30), axis.text.y= element_text(size=30), axis.title.x = element_text(size = 35), axis.title.y  = element_text(size=35), legend.title = element_blank(),  strip.text.x =element_text(size = 25), legend.text = element_blank(), legend.position = "none",panel.background = element_rect(fill = NA, color = "black")) +
  scale_y_continuous(sec.axis = sec_axis(~ .,labels = NULL), position = "right")+
  labs(x = "Treatment",  y=expression(paste(" ",BR," (",mu*molCO[2],"  ",m^-2,"  ",sec^-1,")")))+
  annotate("text", x = 0.5, y = 4.8, label = "F", size = 12)+
  annotate("text", x = 2.4, y =4.8, label = "ns", size = 11)

##Q10 and Intercept Multipanel Figure
Q10_1_grob <- ggplotGrob(Q10_1)
Q10_2_grob <- ggplotGrob(Q10_2)
Q10_3_grob <- ggplotGrob(Q10_3)
Q10_4_grob <- ggplotGrob(Q10_4)
Q10_5_grob <- ggplotGrob(Q10_5)
Q10_6_grob <- ggplotGrob(Q10_6)


layout_Q10 <- rbind(c(1,2),
                    c(3,4),
                    c(5,6))
g_Q10 <- grid.arrange(Q10_1_grob, Q10_2_grob, Q10_3_grob, Q10_4_grob,Q10_5_grob,Q10_6_grob,layout_matrix=layout_Q10)

ggsave(path = "Manuscript_figures", filename = "Q10_summary.png", height = 25, width = 25, units = "in",g_Q10 )


###Build split-plot statistical model for Q10 and Intercept values 

##Q10 model 

####Testing Assumptions 
##Test for outliers test: No extreme outliers
param_model_Q10_b %>% 
  group_by(Severity, Treatment) %>%
  identify_outliers(Q10)


##Equality of variance test for severity and treatment: Equal
leveneTest(Q10 ~ Severity*Treatment, data = param_model_Q10_b)

##Normality
# Build the linear model: Normal 
normality_test_Q10  <- lm(Q10 ~ Severity*Treatment,
                          data = param_model_Q10_b)
# Create a QQ plot of residuals
ggqqplot(residuals(normality_test_Q10))
# Shapiro test of normality 
shapiro_test(residuals(normality_test_Q10))


#####Run Split-plot model for Q10 

Q10_model <- aov(Q10  ~ Severity*Treatment + Error(Rep_ID/Severity/Treatment), data = param_model_Q10_b)
summary(Q10_model)

out_severity_Q10 <- with(param_model_Q10_b, LSD.test(Q10,Severity,9,0.05465, console = TRUE))

##Intercept model 

####Testing Assumptions 
##Test for outliers test: No extreme outliers
param_model_Q10_10 %>% 
  group_by(Severity, Treatment) %>%
  identify_outliers(BR)


##Equality of variance test for severity and treatment: Equal
leveneTest(intercept ~ Severity*Treatment, data = param_model_Q10_10)

##Normality
# Build the linear model: Normal 
normality_test_intercept  <- lm(intercept ~ Severity*Treatment,
                                data = param_model_Q10_10)
# Create a QQ plot of residuals
ggqqplot(residuals(normality_test_intercept))
# Shapiro test of normality 
shapiro_test(residuals(normality_test_intercept))


#####Run Split-plot model for Intercept 

intercept_model <- aov(BR  ~ Severity*Treatment + Error(Rep_ID/Severity/Treatment), data = param_model_Q10_10)
summary(intercept_model)

out_severity_intercept <- with(param_model_Q10_10, LSD.test(BR,Severity,9,0.4937, console = TRUE))



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
  summarize(ave_log_response = mean(log_response), std_err = std.error(log_response), .groups = "drop")
  
resistance_rs$Severity <- as.numeric(resistance_rs$Severity)


##Run regression analysis for Rs resistance by severity with year*severity interaction. 2019,2020, 2021 all have significant linear decline with increasing disturbance severity. The slopes for each year post disturbance are not statistically different (overlapping 95% CI), but they are all significantly different than 2018. 

##Regression Model
multiple_regression_model <- lm(ave_log_response ~ Severity + Severity*year, data = resistance_rs)
summary(multiple_regression_model)

##Post Hoc 

post_hoc_regression <- emmeans(multiple_regression_model, pairwise ~ year*Severity)


# displaying the result table with summary()
summary(post_hoc_regression)

##Simple plot
ggplot(resistance_rs, aes(x = Severity, y = ave_log_response, group = year, color = year)) +
  theme_classic()+
  geom_point()+
  geom_smooth(method = "lm", se = FALSE)


###Since there is no statistically significant difference between 2019-2021 slopes but there IS significance between those years and 2018, we aggregated all years together into the average slope across all years post disturbance and ran a regression analysis as a function of severity.

##Create a pre_post variable
resistance_rs <- resistance_rs%>%
  mutate(pre_post = case_when(year == "2018" ~ "pre", 
                              year == "2019" | year == "2020" | year == "2021" ~ "post"))

##Make model dataframe with only post years 
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



#######Calculation Rh resistance Values 
resistance_rh <- all_years_Rh_severity


resistance_rh <- resistance_rh%>%
  mutate(log_response = case_when(year == "2019" & Rep_ID == "A" ~ log(ave_soilCO2Efflux_umolg/resistance_rh$ave_soilCO2Efflux_umolg[1]),
                                  year == "2019" & Rep_ID == "B" ~ log(ave_soilCO2Efflux_umolg/resistance_rh$ave_soilCO2Efflux_umolg[13]),
                                  year == "2019" & Rep_ID == "C" ~ log(ave_soilCO2Efflux_umolg/resistance_rh$ave_soilCO2Efflux_umolg[25]),
                                  year == "2019" & Rep_ID == "D" ~ log(ave_soilCO2Efflux_umolg/resistance_rh$ave_soilCO2Efflux_umolg[37]),
                                  year =="2020" & Rep_ID == "A" ~ log(ave_soilCO2Efflux_umolg/resistance_rh$ave_soilCO2Efflux_umolg[5]),
                                  year =="2020" & Rep_ID == "B" ~ log(ave_soilCO2Efflux_umolg/resistance_rh$ave_soilCO2Efflux_umolg[17]),
                                  year =="2020" & Rep_ID == "C" ~ log(ave_soilCO2Efflux_umolg/resistance_rh$ave_soilCO2Efflux_umolg[29]),
                                  year =="2020" & Rep_ID == "D" ~ log(ave_soilCO2Efflux_umolg/resistance_rh$ave_soilCO2Efflux_umolg[41]),
                                  year =="2021" & Rep_ID == "A" ~ log(ave_soilCO2Efflux_umolg/resistance_rh$ave_soilCO2Efflux_umolg[9]),
                                  year =="2021" & Rep_ID == "B" ~ log(ave_soilCO2Efflux_umolg/resistance_rh$ave_soilCO2Efflux_umolg[21]),
                                  year =="2021" & Rep_ID == "C" ~ log(ave_soilCO2Efflux_umolg/resistance_rh$ave_soilCO2Efflux_umolg[33]),
                                  year =="2021" & Rep_ID == "D" ~ log(ave_soilCO2Efflux_umolg/resistance_rh$ave_soilCO2Efflux_umolg[45])))%>%
  group_by(year, Severity)%>%
  summarize(ave_log_response = mean(log_response), std_err = std.error(log_response))

resistance_rh$Severity <- as.character(resistance_rh$Severity)
resistance_rh$Severity <- as.numeric(resistance_rh$Severity)

##Run regression analysis for Rh resistance by severity per year: 2019,2020, 2021 

##Regression Model (Nothing is significant)
multiple_regression_model_Rh <- lm(ave_log_response ~ Severity + Severity*year, data = resistance_rh)
summary(multiple_regression_model_Rh)

##Post Hoc 
post_hoc_regression_Rh <- emmeans(multiple_regression_model_Rh, pairwise ~ year*Severity)


# displaying the result table with summary()
summary(post_hoc_regression_Rh)


###Plot Rs and Rh resistance by severity per year with regression line representing average slope post disturbance years 
####Figure 5

##Rs
Rs_rt <- ggplot(resistance_rs, aes(x = Severity, y = ave_log_response, color = year)) +
  theme_classic()+
  geom_point(aes(colour = year), size = 10)+
  scale_color_manual(values=c("#072F5F", "#1261A0", "#3895D3", "#56CCED")) +
  geom_smooth(method = "lm", se = FALSE, data=subset(resistance_rs,pre_post=post), color = "darkgrey", size = 3, show.legend = FALSE, alpha = 0.5)+
  theme(axis.text= element_text(size = 35), axis.title = element_text(size = 40),  legend.text = element_text(size = 30), legend.title = element_text(size = 35), legend.position = c(.2,.25),plot.margin = margin(0,0,0,0, "cm"),axis.ticks.length=unit(.35, "cm")) +
  scale_y_continuous(sec.axis = sec_axis(~ .,labels = NULL)) +
  scale_x_continuous(sec.axis = sec_axis(~ .,labels = NULL)) +
  geom_errorbar(aes(ymin=ave_log_response - std_err, ymax=ave_log_response + std_err, colour = year), size = 1, width = 3)+
  geom_hline(yintercept = 0, linetype = "dashed", size = 2) +
  labs(y=expression(paste(" ",R[s]," Resistance ")), x = "Severity (% Gross Defoliation)") +
  guides(color = guide_legend(title = "Year")) +
  annotate("text", x = 0, y = 0.19, label = "A", size = 12)


Rh_rt <- ggplot(resistance_rh, aes(x = Severity, y = ave_log_response, color = year)) +
  theme_classic()+
  geom_point(aes(colour = year), size = 10)+
  scale_color_manual(values=c("#1261A0", "#3895D3", "#56CCED")) +
  theme(axis.text= element_text(size = 35), axis.title = element_text(size = 40), legend.text = element_text(size = 30), legend.title = element_text(size = 35), legend.position = c(.2,.25),plot.margin = margin(0,0,0,0, "cm"),axis.ticks.length=unit(.35, "cm")) +
  scale_y_continuous(sec.axis = sec_axis(~ .,labels = NULL), position = "right") +
  scale_x_continuous(sec.axis = sec_axis(~ .,labels = NULL)) +
  geom_errorbar(aes(ymin=ave_log_response - std_err, ymax=ave_log_response + std_err, colour = year), size = 1, width = 3)+
  geom_hline(yintercept = 0, linetype = "dashed", size = 2) +
  labs(y=expression(paste(" ",R[h]," Resistance ")), x = "Severity (% Gross Defoliation)") +
  guides(color = guide_legend(title = "Year")) +
  annotate("text", x = 0, y = 0.14, label = "B", size = 12)

#Resistance Multipanel Figure 
Rs_rt_grob <- ggplotGrob(Rs_rt)
Rh_rt_grob <- ggplotGrob(Rh_rt)


layout <- rbind(c(1,2))
            
               
resistance <- grid.arrange(Rs_rt_grob, Rh_rt_grob, layout_matrix=layout)
ggsave("Figure_5.png",height = 10, width = 20, units = "in", resistance)



   
