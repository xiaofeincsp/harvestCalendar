

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

