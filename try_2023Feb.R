rm(list = ls())
# install.packages("calendR")
#devtools::install_github("R-CoderDotCom/calendR")
# install.packages("lubridate")


library(calendR)
library(tidyverse)
library(readxl)
library(openxlsx)
library(lubridate)
# 1. download trial meta information from CassavaBase using wizard
#    there might be R programming way to download, ask Luis Fernando

# 2. add one column, "harvestDays" for harvesting

# 3. read the meta data
folder = "D:\\OneDrive - CGIAR\\03_Git_Git\\04_harvestCalendar\\harvestCalendar\\"
file = "metadata.csv"

meta = read.csv(paste(folder, file, sep=""),
                stringsAsFactors = FALSE,
                skip = 3) %>%
  select(studyName, locationName, plantingDate)
# convert the data into date format
meta$plantingDate = ymd(meta$plantingDate)
# trials without planting date
meta %>%
  filter(is.na(plantingDate))%>%
  select(studyName)

# 4. select locations
allLoc = unique(meta$locationName)
SandraLoc = "CIAT. Valle, Colombia" 
CamiloLoc = c( "Sahagun. Cordoba, Colombia" ,
               "Cerete. Cordoba, Colombia",
               "Momil. Cordoba, Colombia")
JorgeIvanLoc = c( "Santo Tomas. Atlantico, Colombia",
                  "Repelon. Atlantico, Colombia",
                  "Aremasain. Guajira, Colombia")
ThuyLoc = c("Dong Nai",
            "Lao Ngam",
            "Tay Ninh"  )  

# 4.1 Camilo trials
camilo = meta %>%
  filter(locationName %in% CamiloLoc)

# change date into days
camilo$plantingDay = yday(camilo$plantingDate)

# select col
camilo_sel = camilo %>%
  select(studyName, plantingDay)


# camilo planning harvest calendar
camilo_plan = read_excel(paste(folder,
                               "Harvest Calendar Humid Car.V4 23_Camilo.xlsx",
                               sep="")) %>%
  select(`Trail name`, Time_harvest, Planting_Time)
names(camilo_plan) = c("studyName", "harvestDays", "plantingDate")

setdiff(camilo_plan$studyName, camilo_sel$studyName)
setdiff(camilo_sel$studyName, camilo_plan$studyName)

camilo_sel = camilo_plan %>%
  filter(studyName %in% c(camilo_sel$studyName,
                          "202257DVGST_momi",
                          "2022105DVPRC_cere" ) )
camilo_sel$plantingDay = yday(camilo_sel$plantingDate)




# fill the harvest day 1, 2, 3, 4, 5, 6... 10 ---- maxmum 10 days
max_harv_days = max(camilo_sel$harvestDays)
camilo_sel[, c(paste("harvestDay", 1:max_harv_days, sep="") ) ] = NA

for(i in 1:nrow(camilo_sel)) {
  
  if (camilo_sel[i,]$harvestDays == 1) {
    camilo_sel[i,]$harvestDay1 = camilo_sel[i,]$plantingDay + 1
  } 
  
  
  if (camilo_sel[i,]$harvestDays == 2) {
    camilo_sel[i,]$harvestDay1 = camilo_sel[i,]$plantingDay + 1
    camilo_sel[i,]$harvestDay2 = camilo_sel[i,]$plantingDay + 2
  }   
  
  if (camilo_sel[i,]$harvestDays == 3) {
    camilo_sel[i,]$harvestDay1 = camilo_sel[i,]$plantingDay + 1
    camilo_sel[i,]$harvestDay2 = camilo_sel[i,]$plantingDay + 2
    camilo_sel[i,]$harvestDay3 = camilo_sel[i,]$plantingDay + 3
  }  
  
  if (camilo_sel[i,]$harvestDays == 4) {
    camilo_sel[i,]$harvestDay1 = camilo_sel[i,]$plantingDay + 1
    camilo_sel[i,]$harvestDay2 = camilo_sel[i,]$plantingDay + 2
    camilo_sel[i,]$harvestDay3 = camilo_sel[i,]$plantingDay + 3
    camilo_sel[i,]$harvestDay4 = camilo_sel[i,]$plantingDay + 4
    
  } 
  
  if (camilo_sel[i,]$harvestDays == 5) {
    camilo_sel[i,]$harvestDay1 = camilo_sel[i,]$plantingDay + 1
    camilo_sel[i,]$harvestDay2 = camilo_sel[i,]$plantingDay + 2
    camilo_sel[i,]$harvestDay3 = camilo_sel[i,]$plantingDay + 3
    camilo_sel[i,]$harvestDay4 = camilo_sel[i,]$plantingDay + 4
    camilo_sel[i,]$harvestDay5 = camilo_sel[i,]$plantingDay + 5
  } 
  
  if (camilo_sel[i,]$harvestDays == 6) {
    camilo_sel[i,]$harvestDay1 = camilo_sel[i,]$plantingDay + 1
    camilo_sel[i,]$harvestDay2 = camilo_sel[i,]$plantingDay + 2
    camilo_sel[i,]$harvestDay3 = camilo_sel[i,]$plantingDay + 3
    camilo_sel[i,]$harvestDay4 = camilo_sel[i,]$plantingDay + 4
    camilo_sel[i,]$harvestDay5 = camilo_sel[i,]$plantingDay + 5
    camilo_sel[i,]$harvestDay6 = camilo_sel[i,]$plantingDay + 6
    
  } 
  
  if (camilo_sel[i,]$harvestDays == 7) {
    camilo_sel[i,]$harvestDay1 = camilo_sel[i,]$plantingDay + 1
    camilo_sel[i,]$harvestDay2 = camilo_sel[i,]$plantingDay + 2
    camilo_sel[i,]$harvestDay3 = camilo_sel[i,]$plantingDay + 3
    camilo_sel[i,]$harvestDay4 = camilo_sel[i,]$plantingDay + 4
    camilo_sel[i,]$harvestDay5 = camilo_sel[i,]$plantingDay + 5
    camilo_sel[i,]$harvestDay6 = camilo_sel[i,]$plantingDay + 6
    camilo_sel[i,]$harvestDay7 = camilo_sel[i,]$plantingDay + 7
  } 
  
  if (camilo_sel[i,]$harvestDays == 8) {
    camilo_sel[i,]$harvestDay1 = camilo_sel[i,]$plantingDay + 1
    camilo_sel[i,]$harvestDay2 = camilo_sel[i,]$plantingDay + 2
    camilo_sel[i,]$harvestDay3 = camilo_sel[i,]$plantingDay + 3
    camilo_sel[i,]$harvestDay4 = camilo_sel[i,]$plantingDay + 4
    camilo_sel[i,]$harvestDay5 = camilo_sel[i,]$plantingDay + 5
    camilo_sel[i,]$harvestDay6 = camilo_sel[i,]$plantingDay + 6
    camilo_sel[i,]$harvestDay7 = camilo_sel[i,]$plantingDay + 7
    camilo_sel[i,]$harvestDay8 = camilo_sel[i,]$plantingDay + 8
    
  } 
  
  if (camilo_sel[i,]$harvestDays == 9) {
    camilo_sel[i,]$harvestDay1 = camilo_sel[i,]$plantingDay + 1
    camilo_sel[i,]$harvestDay2 = camilo_sel[i,]$plantingDay + 2
    camilo_sel[i,]$harvestDay3 = camilo_sel[i,]$plantingDay + 3
    camilo_sel[i,]$harvestDay4 = camilo_sel[i,]$plantingDay + 4
    camilo_sel[i,]$harvestDay5 = camilo_sel[i,]$plantingDay + 5
    camilo_sel[i,]$harvestDay6 = camilo_sel[i,]$plantingDay + 6
    camilo_sel[i,]$harvestDay7 = camilo_sel[i,]$plantingDay + 7
    camilo_sel[i,]$harvestDay8 = camilo_sel[i,]$plantingDay + 8
    camilo_sel[i,]$harvestDay9 = camilo_sel[i,]$plantingDay + 9
  } 
  
  if (camilo_sel[i,]$harvestDays == 10) {
    camilo_sel[i,]$harvestDay1 = camilo_sel[i,]$plantingDay + 1
    camilo_sel[i,]$harvestDay2 = camilo_sel[i,]$plantingDay + 2
    camilo_sel[i,]$harvestDay3 = camilo_sel[i,]$plantingDay + 3
    camilo_sel[i,]$harvestDay4 = camilo_sel[i,]$plantingDay + 4
    camilo_sel[i,]$harvestDay5 = camilo_sel[i,]$plantingDay + 5
    camilo_sel[i,]$harvestDay6 = camilo_sel[i,]$plantingDay + 6
    camilo_sel[i,]$harvestDay7 = camilo_sel[i,]$plantingDay + 7
    camilo_sel[i,]$harvestDay8 = camilo_sel[i,]$plantingDay + 8
    camilo_sel[i,]$harvestDay9 = camilo_sel[i,]$plantingDay + 9
    camilo_sel[i,]$harvestDay10 = camilo_sel[i,]$plantingDay + 10
  } 
  
}

# convert wide into long

camilo_long = camilo_sel %>%
  select(-plantingDay, -harvestDays) %>%
  pivot_longer(cols= starts_with("harvest"),
               names_to = "harvestDayNum",
               values_to = "plantDates") %>%
  filter(!is.na(plantDates)) %>%
  arrange(plantDates)


#nrow = nrow(camilo_long)
#camilo_long[nrow+1,]$studyName = "2022300LAF1C_saha"
#camilo_long[nrow+1,]$ harvestDayNum = "harvestDay1"
#camilo_long[nrow+1,]$plantDates = 265

same_harvest = camilo_long %>%
  dplyr::group_by(plantDates, harvestDayNum) %>%
  dplyr::summarise(n = dplyr::n(), .groups = "drop") %>%
  dplyr::filter(n > 1L) 
same_harvest
nrow(same_harvest) # if not 0, there are duplicates date+plantday


# add the harvesting day before the trial name
unique_trial = camilo_long %>%
  distinct(studyName, .keep_all = TRUE)
unique_trial$harvest = unique_trial$plantDates + 275 - 365
unique_trial$day_trial = paste(unique_trial$harvest,
                               unique_trial$studyName,
                               sep="_") 
unique_trial_kp = unique_trial %>%
  select(day_trial,studyName)
camilo_long = camilo_long %>%
  left_join(unique_trial_kp, by="studyName")


# long to wide based on days
camilo_wide = camilo_long %>%
  distinct(plantDates, harvestDayNum, .keep_all=TRUE) %>%
  pivot_wider (names_from = harvestDayNum,
               values_from = day_trial )%>%
  arrange(plantDates)
# View(camilo_wide)

camilo_wide$harvestDays = camilo_wide$plantDates + 275-365

full_camilo_wide = data.frame(days = 0:364) %>%
  left_join(camilo_wide, by = c("days" = "harvestDays"))

full_camilo_wide$dates = as.Date(full_camilo_wide$days, origin = "2023-01-01")
full_camilo_wide$weekday <- weekdays(full_camilo_wide$dates)  
full_camilo_wide$plant_date <- as.Date(full_camilo_wide$plantDates, origin = "2022-01-01")   

#str(full_camilo_wide)
#names(full_camilo_wide)
#View(full_camilo_wide)


write.xlsx(full_camilo_wide, 
           paste(folder, 
                 "01_Camilo_harvest_calendar", 
                 "_", 
                 Sys.Date(), 
                 ".xlsx", 
                 sep="") )



camilo_comb <- full_camilo_wide %>%
  filter(!is.na(studyName)) %>%
  select( -dates, -weekday) %>%
  unite("trials", harvestDay1:harvestDay4,  na.rm = TRUE)
# camilo_comb$trial_day = paste(camilo_comb$trials, camilo_comb$harvestDays, sep="_")



length(camilo_comb$days)
myfills <- rep(NA, 365) # 366 or 365 depends on year; 2023, so 365

# Add the events to the desired days
for (i in 1:length(camilo_comb$days) ) {
  myfills[camilo_comb$days[i] ] <- camilo_comb$trials[i]
}

calendR(year = 2023,
        special.days = myfills,
        special.col = 1:length(unique(na.omit(myfills))),     # Add as many colors as events
        legend.pos = "right")  # Add a legend if desired





##########################################
#### after modify the harvesing data #####
##########################################

rm(list = ls())
# install.packages("calendR")
#devtools::install_github("R-CoderDotCom/calendR")
# install.packages("lubridate")
library(calendR)
library(tidyverse)
library(readxl)
library(openxlsx)
library(lubridate)

# check harvest calendar
folder = "D:\\OneDrive - CGIAR\\03_Git_Git\\04_harvestCalendar\\harvestCalendar\\"

plan_harvest = read_excel(paste(folder, 
                                "01_Camilo_harvest_calendar", 
                                "_", 
                                Sys.Date(), 
                                "_v1",
                                ".xlsx", 
                                sep=""),
                          sheet = "Camilo", # show camilo sheet
                          # sheet = "Helper" , # show the helper sheet,
                          skip=1) %>%
  filter(!is.na(studyName)) %>%
  select(days,
         weekday,
         dates,
         studyName,	
         plantingDate)


simple_trial = plan_harvest %>%
  select(days, studyName) %>%
  distinct(studyName, .keep_all = TRUE)
simple_trial$day_trial = paste(simple_trial$days, 
                               simple_trial$studyName,
                               sep="_") 
simple_trial = simple_trial %>%
  select (-days)

plan_harvest = plan_harvest %>%
  left_join(simple_trial, by="studyName")



length(plan_harvest$day_trial)
myfills <- rep(NA, 365) # 366 or 365 depends on year; 2023, so 365

# Add the events to the desired days
for (i in 1:length(plan_harvest$days) ) {
  myfills[plan_harvest$days[i] ] <- plan_harvest$day_trial[i]
}

calendR(year = 2023,
        special.days = myfills,
        special.col = 1:length(unique(na.omit(myfills))),     # Add as many colors as events
        legend.pos = "right")  # Add a legend if desired


#############################################
### check the planting calendar

plan_planting = read_excel(paste(folder, 
                                "01_Camilo_harvest_calendar", 
                                "_", 
                                Sys.Date(), 
                                "_v1",
                                ".xlsx", 
                                sep=""),
                          sheet = "Camilo", # show camilo sheet
                          # sheet = "Helper" , # show the helper sheet,
                          skip=1) %>%
  filter(!is.na(planting_plan1)) %>%
  select(days,
         dates,
         planting_plan1,
         planting_plan2,
         planting_plan3,
         planting_plan4,
         planting_plan5 )

planting_comb <- plan_planting %>%
  unite("trials", planting_plan1:planting_plan5,  na.rm = TRUE)



simple_plant = planting_comb %>%
  select(days, trials) %>%
  distinct(trials, .keep_all = TRUE)
simple_plant$day_trial = paste(simple_plant$days, 
                               simple_plant$trials,
                               sep="_") 
simple_plant = simple_plant %>%
  select (-days)

planting_comb = planting_comb %>%
  left_join(simple_plant, by="trials")

length(planting_comb$day_trial)
myfills <- rep(NA, 365) # 366 or 365 depends on year; 2023, so 365

# Add the events to the desired days
for (i in 1:length(planting_comb$days) ) {
  myfills[planting_comb$days[i] ] <- planting_comb$day_trial[i]
}

calendR(year = 2023,
        special.days = myfills,
        special.col = 1:length(unique(na.omit(myfills))),     # Add as many colors as events
        legend.pos = "right")  # Add a legend if desired

