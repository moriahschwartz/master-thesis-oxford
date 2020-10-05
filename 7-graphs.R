# Creation of graphs

rm(list=ls())
setwd("C:/...")

# County panel data
df <- readRDS("code/county/panel_data.rds")


# Line plot: Figures 1-2
library(ggplot2)
library(dplyr)
library(plotly)

# Sum variables by year
sums <- df[-c(12:15)] %>%
  group_by(datayear) %>%
  summarise_all(funs(sum))

# Average variables by year
means <- df[-c(12:15)] %>%
  group_by(datayear) %>%
  summarise_all(funs(mean))

# Figure 1: Total child welfare entries over time
entries <- ggplot() +
  geom_line(data = sums, aes(x = datayear, y = entered), color = "blue") +
  labs(x = "Year", y = "Total Number of Children Entering Foster Care",size=12) +
  theme_classic(base_size=16) +
  ylim(75000,130000)
entries

# Figure 2: Average drug-related death rates over time
deaths <- ggplot() +
  geom_line(data = means, aes(x = datayear, y = drugdeathrate), color = "red") +
  labs(y = "Drug-Related Deaths (per 100k)", x = "Year") +
  ylim(0,40) +
  theme_classic(base_size =  16)
deaths


# Bar graph
# Figure 4: Number of counties offering any/all modes of MAT over time, bargraph

# some_MAT includes # of counties offering any/all modes; data must to sum to this value in stacked barplot
sums$incom_MAT <- sums$some_MAT - sums$MAT_acc

# Create stacked bar graph
plot_ly(data=sums, x = ~datayear, y = ~MAT_acc, type = 'bar', name = '3 Modes',
        text=~MAT_acc, textposition = 'auto') %>%
  add_trace(y = ~incom_MAT, name = '1-2 Modes', text=~incom_MAT,
            textposition='auto') %>%
  layout(yaxis = list(title = ' '), xaxis = list(title = "Year"), barmode = 'stack')
