# Create panel dataset from ncands dataframe
# Retrieve variables of interest from ACS 1-Year Estimates for each county-year in dataset (tidycensus)

rm(list=ls())
setwd("C:/...")

library(tidyr)
library(plyr)
library(dplyr)
library(tidycensus)
library(tm)
library(stringr)

# Append fipscodes & create panel dataframe from ncands df

df <- readRDS("code/ncands.rds")
# Remove "8" and "9" county codes -- missing data
df <- subset(df, fipscode != 8 & fipscode != 9)
# Add "0" to beginning of fips code where applicable
df$fipscode <- sprintf("%05d", df$fipscode)

# API key
census_api_key("...", install = TRUE)

# Download fipscodes from tidycensus
fipsdf <- data.frame(fips_codes)
# get full fips code by appending state + county fips codes
fipsdf$code <- paste(fipsdf$state_code,fipsdf$county_code,sep="")

# merge fips and ncands (county-year) datasets
countydf <- merge(df, fipsdf, by.x = "fipscode", by.y = "code")
# Remove "County" from county name -- tm package
stopwords = "County"
col = as.character(countydf$county)
col = removeWords(col, stopwords)
# Remove extra space at end of county name (stringr package)
countydf$county <- str_trim(col)

# Remove unneeded columns
countydf_sums <- subset(countydf, select=-c(state, state_code, county_code))

# Sum up variables by county-year
countydf_sums <- countydf_sums %>%
  group_by(fipscode,datayear,state_name,county) %>%
  summarise_each(funs(sum))

# save panel data
saveRDS(countydf_sums, "code/countydf_sums.rds")




# Retrieve ACS variables


# Function for appending codes to variable names (for ACS variables split into subcategories)
# Pass in codes for variables, variable family name
append.code.names <- function(codes, name){
  # Create empty vector
  v <- c()
  # Iterate through codes
  for(i in codes){
    # Add codes to variable family name
    x <- paste(name, i, sep="")
    # Add new variable name to vector
    v <- append(v, x)
  }
  # Return vector
  v
}


# Compile variables -- original dataset split by value range, sex, and age; want to retrieve totals regardless of category
# Labor force variable codes (# in labor force)
lf_codes <- c("004","011","018","025","032","039","046","053","060","067",
              "074","079","084","090","097",104,111,118,125,132,139,146,153,160,165,170)
lf <- append.code.names(lf_codes,"B23001_")

# Unemployed variable codes (# unemployed in labor force)
unemp_codes <-c("008","015","022","029","036","043","050","057","064","071","076",
                "081","086","094",101,108,115,122,129,136,143,150,157,162,167,172)
unemp <- append.code.names(unemp_codes, "B23001_")

# Education variables (# of individuals over age 25 with no HS diploma)
edu_codes <- c("003","004","005","006","007","008","009","010",
               "020","021","022","023","024","025","026","027")
edu <- append.code.names(edu_codes, "B15002_")

# List of variables to retrieve using tidycensus
vars <- c(
  tot_pop = "B01003_001",
  pov_track = "B17001_001", # people for whom poverty status determined
  income_below_pov = "B17001_002", # total people living in poverty
  child_pop = "B09001_001", # pop under 18 years
  labor_force = lf, # size of labor force
  unemployed = unemp, # unemployed in labor force
  white_pop = "B02001_002", # white population
  pop_25_plus = "B15002_001", # population over 25 years fo age
  less_HS_25 = edu, # number of individuals over age 25 without a HS diploma
  marital_pop = "B06008_001", # population with marital status recorded
  married_not_sep = "B06008_003" # married (and not separated) population
)

fips <- as.character(unique(countydf_sums$fipscode))
years <- as.numeric(unique(countydf_sums$datayear))

# Create empty dataframe
results <- data.frame()
# Get ACS values for desired variables & years
for(i in years){
  res <- get_acs(
    geography = "county",
    variables = vars,
    year = i,
    survey = "acs1",
    county = fips
  )
  # Record  year
  res$year = i
  # Bind results to original dataframe
  results <- rbind(res, results)
}


# Format ACS variables dataframe
results_df <- subset(results, select=-c(GEOID, moe)) %>% # remove un-needed variables
  group_by(NAME, year) %>%
  dplyr::mutate(id = row_number()) %>%
  # Turn variable/estimate from long to wide
  spread(variable, estimate) %>%
  # Collapse rows by state and year, replace NA's with first non-na value
  summarise_each(funs(first(.[!is.na(.)])))

# Function that sums columns that contain a particular string to find totals for each county-year
sum.cols <- function(data, string){
  cols <- data[,grepl(string, colnames(data))]
  rowSums(cols)
}

# Sum labor force columns
results_df$labor_force_total <- sum.cols(results_df, "labor_force")
# Sum unemployment columns
results_df$unemployed_total <- sum.cols(results_df, "unemployed")
# Sum education columns
results_df$less_HS_25_total <- sum.cols(results_df, "less_HS_25")

# Remove old un-summed columns
final_acs <- subset(results_df, select = c("NAME", "year", "child_pop", "income_below_pov",
                                       "pov_track", "tot_pop", "labor_force_total", 
                                       "unemployed_total", "white_pop", "less_HS_25_total",
                                       "pop_25_plus","marital_pop","married_not_sep"))

# save
saveRDS(final_acs,"code/county/acs_results.rds")



# Merge with county-level sums of foster youth data: final df includes fipscodes, ncands panel data, ACS variables
# Trim any whitespace
countydf_sums$county <- trimws(countydf_sums$county)
countydf_sums <- countydf_sums %>% 
  # Get full name ("X County, State") unless Alaska/Louisiana (Municipality/Parish)
  mutate(fullname = ifelse(state_name != "Alaska" & state_name != "Louisiana", 
                       paste(county, " County, ", state_name, sep=""), 
                       paste(county, ", ", state_name, sep="")))

# Merge ACS data with county-level foster youth data
foster_acs_df <- merge(countydf_sums, final_acs, by.x=c("fullname","datayear"), 
                       by.y=c("NAME","year"))

# save
saveRDS(foster_acs_df,"code/foster_acs_df.rds")
