# Cleanup and merge MAT (mediction assisted treatment) data with panel dataframe

rm(list=ls())
setwd("C:/...")

library(dplyr)
library(tidyr)
library(plyr)
library(tidyverse)
library(tm)
library(stringr)
library(readxl)

foster_acs_df <- readRDS("code/foster_acs_df.rds")

# Read in MAT (mediction assisted treatment) data
matdata <- read.table("OpioidDB/countydata.csv", header = TRUE, sep = ",")

# Keep desired variables/values
indic_list <- c("SMAT_fac","AMAT_fac","drugdeaths","drugdeathrate")
matdata <- subset(matdata, INDICATOR %in% indic_list)

# Split county column (to remove "County" from name) -- tm package
stopwords = "County"
col = as.character(matdata$COUNTY)
col = removeWords(col, stopwords)
# Remove extra space at end of county name (stringr package)
matdata$COUNTY_FNAME <- str_trim(col)

# Remove unneeded columns
matdata <- subset(matdata, select=-c(1,2,6,8)) 

# Reformat data table
matdata <- matdata %>%
  group_by(COUNTY_FNAME, STATE, YEAR) %>%
  dplyr::mutate(i = row_number()) %>%
  # Turn INDICATOR/VALUE from long to wide format
  spread(INDICATOR, VALUE) %>%
  # Collapse rows by county and year, replace NA's with first non-na value
  summarise_each(funs(first(.[!is.na(.)])))

# Merge foster/census dataframe with matdata
df <- merge(foster_acs_df, matdata, 
                            by.x=c("state_name","county","datayear"),
                            by.y=c("STATE","COUNTY_FNAME","YEAR"))
                                                        
saveRDS(df, "county/foster_acs_matdata_df.rds")
