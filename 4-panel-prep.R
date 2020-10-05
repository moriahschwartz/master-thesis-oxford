# Clean up panel dataframe, export to STATA .dta file for regression analysis

rm(list=ls())
setwd("C:/...")

library(dplyr)
library(tidyr)
library(plyr)
library(tidyverse)
library(tm)
library(stringr)

# Prepare df for panel analysis
df <- readRDS("county/foster_acs_matdata_df.rds")

# Create new variables as needed
df$pov_rate <- df$income_below_pov / df$pov_track
df$unemp_rate <- df$unemployed_total / df$labor_force_total
df$no_HS_rate <- df$less_HS_25_total / df$pop_25_plus
df$perc_white <- df$white_pop / df$tot_pop
df$perc_married <- df$married_not_sep / df$marital_pop

# Fill NA's that come after a non-NA value (for MAT facility variable)
df <- df %>%
  group_by(fullname) %>%
  fill(AMAT_fac)
  
# Create binary comprehensive MAT variable for each county-year: 0 = no comprehensive MAT, 1= comprehensive MAT access
df$MAT_acc <- as.integer(!is.na(df$AMAT_fac))
# Binary incomplete MAT variable: 0 = no MAT access, 1 = incomplete MAT access
df$some_MAT <- as.integer(!is.na(df$SMAT_fac))

# Remove unneeded variables
df2 <- subset(df, select=-c(county,fipscode,
                             disreasn,pov_track,
                             labor_force_total,
                             pop_25_plus,less_HS_25_total,i,
                             AMAT_fac,SMAT_fac,daparent))
                             
# Keep years >= 2011
df2 <- df2[which(df2$datayear >= 2011),]
                             
# Listwise deletion
df2 <- df2[complete.cases(df2),]

# Get list of states included in complete cases
df2$state <- df2$state_name


# Create variable for region: U.S. Census Bureau delineates 4 statistical regions, 9 divisions

# Northeast: 2 divisions
new_england <- c("Connecticut", "Maine", "Massachusetts", "New Hampshire", "Rhode Island", "Vermont")
mid_atlantic <- c("New Jersey", "New York", "Pennsylvania")
northeast <- c(new_england, mid_atlantic)

# Midwest: 2 divisions
east_north_central <- c("Illinois", "Indiana", "Michigan", "Ohio","Wisconsin")
west_north_central <- c("Iowa", "Kansas", "Minnesota", "Missouri", 
                        "Nebraska", "North Dakota", "South Dakota")
midwest <- c(east_north_central,west_north_central)

# South: 3 divisions
south_atlantic <- c("Delaware", "Florida", "Georgia","Maryland","North Carolina",
                    "South Carolina", "Virginia", "District of Columbia", "West Virginia")
east_south_central <- c("Alabama", "Kentucky", "Mississippi", "Tennessee")
west_south_central <- c("Arkansas", "Louisiana", "Oklahoma", "Texas")
south <- c(south_atlantic,east_south_central,west_south_central)

# West: 2 divisions
mountain <- c("Arizona","Colorado", "Idaho", "Montana", "Nevada","New Mexico",
              "Utah","Wyoming")
pacific <- c("Alaska","California", "Hawaii", "Oregon","Washington")
west <- c(mountain,pacific)


# Create region variable based on state name
df2$region <- ifelse(df2$state %in% northeast, "northeast",
              ifelse(df2$state %in% midwest, "midwest",
              ifelse(df2$state %in% south, "south",
              ifelse(df2$state %in% west, "west",
                     NA))))

# Create division variable based on state name
df2$division <- ifelse(df2$state %in% new_england, "new_england",
                ifelse(df2$state %in% mid_atlantic, "mid_atlantic",
                ifelse(df2$state %in% east_north_central, "east_north_central",
                ifelse(df2$state %in% west_north_central, "west_north_central",
                ifelse(df2$state %in% south_atlantic, "south_atlantic",
                ifelse(df2$state %in% east_south_central, "east_south_central",
                ifelse(df2$state %in% west_south_central, "west_south_central",
                ifelse(df2$state %in% mountain, "mountain",
                ifelse(df2$state %in% pacific, "pacific",
                       NA )))))))))


# Save final panel dataset
saveRDS(df2, "code/panel_data.rds")

# Create dta file for STATA
write.dta(df2,"code/panel_data.dta")



# Collect summary info: find number of control/treatment counties and county-years

# Summarize variables by county name
county_info <- df2 %>%
  group_by(fullname) %>%
  summarize_at(vars(-datayear,-state,-region,-division), funs(sum))

control_names <- county_info$fullname[county_info$MAT_acc == 0]
length(unique(control_names)) # 48 unique counties in control group

treat_names <- county_info$fullname[county_info$MAT_acc > 0]
length(unique(treat_names)) # 76 unique counties in treatment group

length(unique(df2$fullname))
# 124 unique counties total

# Total county-years
length(df2$fullname[df2$MAT_acc == 0])
# 428 county-years in control group
length(df2$fullname[df2$MAT_acc == 1])
# 214 county-years in treatment group
nrow(df2)
# 642 unique county-years total
