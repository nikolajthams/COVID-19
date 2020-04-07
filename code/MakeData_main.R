library(shiny)
library(reshape2)
library(ggplot2)
library(shiny)
library(tidyverse)
library(scales)
library(shinydashboard)
library(DT)
library(knitr)
library(plotly)
library(magrittr)
library(tidyselect)
library(shinyhelper)


theme_set(theme_minimal())

region2country <- function(data){
  #### Function to make Faroe Islands and Greenland appear independent
  regions = c('Faroe Islands', 'Greenland')
  regions = intersect(data$Province.State, regions)
  data$Country.Region = as.character(data$Country.Region)
  for(region in regions){
    data$Country.Region[data$Province.State == region] = region
  }
  return(data)
}

# Define data paths -------------------------------------------------------
source("code/data_paths.R")

# Load confirmed data ---------------------------------------------------------------
data <- read.csv2(
  cases_path,
  sep = ","
)
data = region2country(data)
# Drop irrelevant data
drops <- c("Lat", "Long", "Province.State")
data <- data[,!(names(data) %in% drops)]
data <- aggregate(. ~ Country.Region, data = data, FUN = sum)
# Prepare for ggplot
data <- melt(
  data,
  id.vars = "Country.Region",
  variable.name = "Date",
  value.name = "Cases"
) %>%
  mutate(
    "Date" = as.Date(substring(Date, 2), format = "%m.%d.%y")
  )

data %<>% group_by(Country.Region) %>%
  arrange(Date) %>%
  mutate(NewCases = Cases - lag(Cases),
         NewCases = ifelse(is.na(NewCases), 0, NewCases)) %>%
  ungroup()

# Add population data -------------------------------------------------------------
cdata <- pop_path %>%
  read_delim(
    .,
    delim = ","
  ) %>%
  mutate(
    "Urban population" = as.numeric(
      gsub('^\\%|\\%$', '', `Urban population`)
    ),
    Country = ifelse(
      Country == "United States",
      "US",
      ifelse(
        Country == "South Korea",
        "Korea, South",
        Country
      )
    )
  )

data <- left_join(
  data,
  select(
    cdata,
    Country,
    Population,
    "Density (P/km2)",
    "Urban population"
  ),
  by = c(
    "Country.Region" = "Country"
  )
)

# Death by age group
death.by.age <- read.csv(
  deaths_path_age,
  header=TRUE,
  stringsAsFactors = FALSE,
  sep = ";"
)
countries.w.age.data = death.by.age$Country


# Add recovery data ------------------------------------------------------
data_r <- read.csv2(
  recover_path,
  sep = ","
)
data_r = region2country(data_r)
# Drop irrelevant data
drops <- c("Lat", "Long", "Province.State")
data_r <- data_r[,!(names(data_r) %in% drops)]
data_r <- aggregate(. ~ Country.Region, data = data_r, FUN = sum)
# Prepare for ggplot
data_r <- melt(
  data_r,
  id.vars = "Country.Region",
  variable.name = "Date",
  value.name = "Cases"
) %>%
  mutate(
    "Date" = as.Date(substring(Date, 2), format = "%m.%d.%y")
  ) %>%
  rename(
    "Recovered" = "Cases"
  )

data <- left_join(
  data,
  data_r,
  by = c(
    "Date",
    "Country.Region"
  )
)


# Add mortality data ------------------------------------------------------
data_r <- read.csv2(
  death_path,
  sep = ","
)
data_r = region2country(data_r)
# Drop irrelevant data
drops <- c("Lat", "Long", "Province.State")
data_r <- data_r[,!(names(data_r) %in% drops)]
data_r <- aggregate(. ~ Country.Region, data = data_r, FUN = sum)
# Prepare for ggplot
data_r <- melt(
  data_r,
  id.vars = "Country.Region",
  variable.name = "Date",
  value.name = "Cases"
) %>%
  mutate(
    "Date" = as.Date(substring(Date, 2), format = "%m.%d.%y")
  ) %>%
  rename(
    "Deaths" = "Cases"
  )

data <- left_join(
  data,
  data_r,
  by = c(
    "Date",
    "Country.Region"
  )
) %>%
  mutate(
    "StillInfected" = Cases - (Recovered + Deaths),
    "MortalityRate" = (Deaths / Cases) * 100,
    "MortalityRatePop" = (Deaths / Population) * 100,
    "RecoveryRate"  = (Recovered / Cases) * 100
  ) %<>% 
  group_by(Country.Region) %>%
  arrange(Date) %>%
  mutate(NewDeaths = Deaths - lag(Deaths),
         NewDeaths = ifelse(is.na(NewDeaths), 0, NewDeaths)) %>%
  ungroup()

  write_delim(
      data,
      "code/data/frontpage_data.csv",
      delim = ","
  )