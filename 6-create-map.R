# Create correlation map (Figure 3 in paper)

rm(list=ls())
setwd("C:/...")

library(tm)
library(stringr)
library(tidyverse)
library(plotly)
library(rjson)
library(haven)
library(dplyr)
library(maps) #fipscodes

# from maps library, retrieve state fips codes
data(state.fips)

# state panel data
statedf <- readRDS("code/state_panel_data.rds")
# lowercase state names
statedf$state_lower <- tolower(statedf$state)

# edit misspelled state names
fipsdf <- state.fips
fipsdf$polyname[60] <- "washington"
fipsdf$polyname[55] <- "virginia"
fipsdf$polyname[39] <- "north carolina"
fipsdf$polyname[35] <- "new york"
fipsdf$polyname[21] <- "massachusetts"
fipsdf$polyname[23] <- "michigan"
fipsdf$polyname[24] <- "michigan"

# Merge datasets
df <- merge(statedf, fipsdf, by.x="state_lower",by.y="polyname")

# Create variables
df$entry_per_1k <- df$entered * df$child_pop / 1000
df$child_entry_rate <- df$entered / df$child_pop

# Create df for mapping correlations
mapdf <- df[c(1,12,29,27,23)] %>%
  group_by(state_lower,abb,fips) %>%
  summarize(COR=cor(drugdeathrate,child_entry_rate))
  # alternative: cor(drugdeathrate,child_per_1k)

# Remove DC from dataset
mapdf <- subset(mapdf, state_lower != "district of columbia")

# Set projection
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa')
)

# Create map
fig <- plot_ly()
fig <- fig %>% add_trace(
  type = "choropleth",
  locations = mapdf$abb,
  locationmode='USA-states',
  z = mapdf$COR,
  colorscale = "Bluered",
  zmin = -1,
  zmax = 1,
  marker = list(line=list(width=0))
)


# Move color bar legend down
fig <- fig %>% colorbar(title = "\n \n \n \n \n \n \n")

# Title
fig <- fig %>% layout(
  title = "Correlation Between Drug-Related Death Rate (per 100k) 
  and Percent of Children Entering Foster Care by State, 2011-17
  (Sources: AFCARS, CDC)"
)

fig <- fig %>% layout(
  geo = g
)

# Display map
fig
