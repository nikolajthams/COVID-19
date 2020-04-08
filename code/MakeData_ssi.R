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


# Define data paths -------------------------------------------------------
source("code/data_paths.R")

# Load Danish data from SSI -----------------------------------------------
ssi <- ssi_path %>%
  read_delim(
    .,
    delim = ",",
    locale = locale(grouping_mark = ".")
  ) %>%
  rename(
    `Lab confirmed cases` = "Laboratoriebekræftede"
  ) %>%
  dplyr::select(-X1) %>%
  mutate(
    "Lab confirmed cases" = gsub("\\*", "", `Lab confirmed cases`) %>% as.numeric,
    Tested = gsub("\\*", "", Tested) %>% as.numeric
  ) %>%
  mutate(
    Date = ifelse(
      grepl("27. januar", Date, ignore.case = T),
      # Date == "27. januar - 3. marts",
      paste(
        as.numeric(substr(lead(Date), 1, 2)) - 1,
        ". marts",
        sep = ""
      ),
      Date
    ),
    Date = gsub(". marts", "/03/2020", Date) %>% 
        gsub(". april", "/04/2020", .) %>% 
        as.Date(., format = "%d/%m/%Y"),
    InfectionRate = `Lab confirmed cases` / Tested
  )

agefiles <- list.files("code/data/ssi_agegroups/")
agedata  <- lapply(
  agefiles,
  function(x) {
    data <- read_delim(
      paste("code/data/ssi_agegroups/", x, sep = ""), 
      delim = ",",
      locale = locale(decimal_mark = ".")
    ) %>% 
      select(-X1) %>%
      mutate(
        Date = as.Date(
          substr(x, 6, 13), format = "%d%m%Y"
        ),
        `Laboratoriebekræftede` = ifelse(
          Date >= "2020-04-01" & `Laboratoriebekræftede` < 10,
          `Laboratoriebekræftede` * 1000,
          Laboratoriebekræftede
        )
        # ,
        # `Antal testede personer` = `Antal testede personer` / 10
      )
    return(data)
  }
) %>% 
  do.call("rbind", .) %>%
  ungroup

write_delim(
    ssi,
    "code/data/ssi_processed_daily.csv",
    delim = ","
)
write_delim(
    agedata,
    "code/data/ssi_processed_agegroups.csv",
    delim = ","
)