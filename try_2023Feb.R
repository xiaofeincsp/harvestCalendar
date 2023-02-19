rm(list = ls())
# install.packages("calendR")
# devtools::install_github("R-CoderDotCom/calendR")
library(calendR)
library(tidyverse)
library(readxl)
library(openxlsx)

# 1. download trial meta information from CassavaBase using wizard
#    there might be R programming way to download, ask Luis Fernando

# 2. add one column, "harvestDays" for harvesting

# 3. read the meta data
folder = "D:\\OneDrive - CGIAR\\03_Git_Git\\04_harvestCalendar\\harvestCalendar\\"
file = "metadata.csv"

meta = read.csv(paste(folder, file, sep=""),
                stringsAsFactors = FALSE,
                skip = 3) %>%
  select(studyDescription, locationName, plantingDate, harvestDays)

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




