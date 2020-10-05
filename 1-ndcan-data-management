# Retrieve NDCAN data from desktop, compile each year into single dataframe, cleanup of columns

rm(list=ls())
setwd("C:/...")

library(dplyr)
library(tidyr)
library(plyr)
library(haven)
library(sjlabelled)

# list of NDCAN data folders
foldernames <- c('167 FC2011v6', '176 FC2012v7', '187 FC2013v7', '192 FC2014v7',
                 '200 FC2015v5', '215 Fc2016v3', '225 FC2017v2')

df_2018 <- read_dta("NDCAND_data/DS235 FC2018v1/DATA/Stata Files/FC2018v1.dta")
names(df_2018)[names(df_2018) == "fy"] <- "datayear"
df_2018$fipscode <- as.character(df_2018$fipscode)

# get path to each file
get.path <- function(folder){
  # keep only second string in folder name
  file <- strsplit(folder, " ")[[1]][2]
  # If file name ends in 'a', remove from string (not included in stata file name)
  file <- sub("a$","",file)
  # paste to get path
  filepath <- paste("NDCAND_data/DS", folder, "/Data/Stata Files/", file,
                    ".dta", sep="")
  filepath
}
vars = c("datayear", "fipscode", "entered", "state", "st")

# create empty dataframe
df <- subset(df_2018, select=vars)
# loop through foldernames
for(i in foldernames){
  # get path for file
  filepath <- get.path(i)
  # create df for file data
  new_df <- read_dta(filepath)
  # change columns to lowercase for consistency
  names(new_df) <- tolower(names(new_df))
  # replace "fy" with "datayear"
  names(new_df)[1] <- "datayear"
  # save fipscode as character instead of double
  new_df$fipscode <- as.character(new_df$fipscode)
  # filter out desired variables
  new_df <- subset(new_df, select=vars)
  # append to entire df 
  df <- rbind(df, new_df)
}

saveRDS(df, "code/ncands.rds")
