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
# 
source("code/data_paths.R")
# input <- tibble(
#   "wvv.death_rate" = c(
#     0, 0, 0, 0.0011, 0.0008, 0.0042, 0.0152, 0.0628, 0.1024
#   ) %>% as.character
# )
# input <- tibble(
#   "wvv.death_rate" = c(
#     0.0, 0.0, 0.0, 0.001, 0.001, 0.006, 0.017, 0.070, 0.183
#   ) %>% as.character
# )

countries.w.age.data <- c("China", "Germany", "Italy", "Korea, South", "Denmark")

  ## Read data
  demographics <-
    read_delim(
      demo_path,
      delim = ";",
      locale = locale(decimal = ",")
    ) %>%
    arrange(Country) %>%
    select(-`Total population`)

  colnames(demographics)[-1] <- paste0(
    10 * (0:8), rep("-", 8), 9 + 10 * (0:8)
  )
  
  wvv.data <- read_delim(
    "code/data/wvvdata.csv",
    delim = ","
  ) 
  # %>% 
  #   left_join(
  #     .,
  #     demographics,
  #     by = "Country"
  #   )
  ## These two parameters need to be adjusted
  death.rate <- 
    as.character(
        c(
            0.0, 0.0, 0.0, 0.001, 0.001, 0.006, 0.017, 0.070, 0.183
        )
    ) %>%
    strsplit(
      .,
      ","
    ) %>%
    unlist %>%
    as.numeric
  
  # relative.death.risk = as.numeric(unlist(strsplit(input$wvv.rel_risk,",")))
  rel.rate.high <- c(0, 0, 0.003, 0.003, 0.008, 0.038, 0.134, 0.271, 0.909)
  rel.rate.low  <- c(0, 0, 0, 0, 0, 0.003, 0.015, 0.074, 0.543)
  
  # numbers from south korea
  # https://en.wikipedia.org/wiki/Coronavirus_disease_2019#Prognosis
  ## Actual relevant computation
  
  .bin_search_below <- function(x, p, low, high, alpha = 0.05) {
    if (x <= 0) return(0)
    l <- low
    u <- high
    
    checkval <- ceiling((l + u) / 2)
    steps <- 0
    max_step <- ifelse(
      x <= 0,
      20,
      ceiling(log2(x / p - 1))
    )
    exit_condition <- T
    
    while (steps < max_step & exit_condition) {
      steps <- steps + 1
      prob <- pbinom(
        x - 1,
        checkval,
        p,
        lower.tail = F
      )
      
      if (
        prob > (alpha / 2)
      ) {
        # u <- mid
        u <- checkval
        checkval <- ceiling((l + u) / 2)
      } else {
        l <- checkval
        checkval <- ceiling((l + u) / 2)
      }
      
      if (abs(prob - alpha / 2) < 1e-9) exit_condition <- F
    }
    
    return(checkval)
  }
  
  .bin_search_above <- function(x, p, low, high, alpha = 0.05) {
    l <- low
    u <- high
    
    checkval <- ceiling((l + u) / 2)
    steps <- 0
    max_step <- ifelse(
      x <= 0,
      20,
      ceiling(log2(x / p - 1))
    )
    exit_condition <- T
    
    while (steps < max_step & exit_condition) {
      .checkval_old <- checkval
      steps <- steps + 1
      prob <- pbinom(
        x,
        checkval,
        p
      )
      
      if (
        prob > (alpha / 2)
      ) {
        l <- checkval
        checkval <- ceiling((l + u) / 2)
      } else {
        u <- checkval
        checkval <- ceiling((l + u) / 2)
      }
      
      if (abs(prob - alpha / 2) < 1e-9) exit_condition <- F
      if (.checkval_old == checkval) exit_condition <- F
    }
    
    return(checkval)
  }
  
  find_confidence_bounds <- function(x, p, alpha = 0.05) {
    n.est <- x / p
    if (p > 0) {
      lower.n <- .bin_search_below(
        x, p, 0, ceiling(n.est), alpha
      )
      
      upper.n <- .bin_search_above(
        x, p, floor(n.est), floor(n.est) + 2 * (n.est - lower.n), alpha
      )
      
      res <- c(lower.n, upper.n)
    } else {
      res <- c(0, 0)
    }
    return(res)
  }

    update_data <- function(country, rel.rate) {
    if (!(country %in% countries.w.age.data)) {
      dem <- filter(demographics, Country %in% country) %>% 
      select(-Country) %>%
      as.numeric
      dd <- filter(wvv.data, Country %in% country) %>% pull(Deaths)

      out <- ((rel.rate.low * dem / sum(rel.rate.low * dem)) / death.rate) %>%
      {sum(. * is.finite(.), na.rm = T)} %>%
      {. * dd}
    
      return(out)
    } else {
      num.death <- death.by.age %>%
      filter(
        Country == country
      ) %>%
      select(-Country) %>%
      as.numeric

      num.death.scaled <- num.death / sum(num.death) * death_mat[i, j]
      
      out <- find_confidence_bounds()
    }
    
  }

  tst <- update_data(
    "Denmark",
    rel.rate.low
  )
  cbind(
    "Original" = wvv.data %>% filter(Country == "Denmark") %>% pull(Cases.low),
    "New" = tst
  )
  
  # tictoc::tic()
  for (country in countries.w.age.data) {
    i = which(deathdata$Country.Region == country)
    # num.death <-
      # as.numeric(subset(death.by.age, Country == country, select = -c(Country)))
    num.death <- death.by.age %>%
      filter(
        Country == country
      ) %>%
      select(-Country) %>%
      as.numeric
    
    active_cases <- rep(NA, ncol(death_mat))
    
    for (j in 1:ncol(death_mat)) {
      num.death.scaled <- num.death / sum(num.death) * death_mat[i, j]
      bounds <- sapply(
        1:length(death.rate), 
        function(k) {
          find_confidence_bounds(num.death.scaled[k], death.rate[k])
        }
      )      
      activedata.low[i, j]  <- sum(bounds[1, ], na.rm = TRUE)
      activedata.high[i, j] <- sum(bounds[2, ], na.rm = TRUE)
    }
  }
  # tictoc::toc()
  
  activedata.low %<>% 
    melt(
      .,
      id.vars = "Country",
      variable.name = "Date",
      value.name = "Cases.low"
    ) %>%
    mutate_at(
      "Date",
      as.Date
    )
  
  activedata.high %<>% 
    melt(
      .,
      id.vars = "Country",
      variable.name = "Date",
      value.name = "Cases.high"
    ) %>%
    mutate_at(
      "Date",
      as.Date
    )
  
  confirmeddata %<>%
    melt(
      .,
      id.vars = "Country.Region",
      variable.name = "Date",
      value.name = "ConfirmedCases"
    ) %>%
    mutate(
      Date = as.Date(substring(Date, 2), format = "%m.%d.%y")
    )
  
  deathdata %<>% 
    melt(
      .,
      id.vars = "Country.Region",
      variable.name = "Date",
      value.name = "Deaths"
    ) %>%
    mutate(
      Date = as.Date(substring(Date, 2), format = "%m.%d.%y")
    )
  
  wvv.data <- merge(
    activedata.high, 
    activedata.low, by = c("Country", "Date")
  )
  wvv.data <- merge(
    wvv.data,
    deathdata,
    by.x = c("Country", "Date"),
    by.y = c("Country.Region", "Date")
  )
  wvv.data <- merge(
    wvv.data,
    confirmeddata,
    by.x = c("Country", "Date"),
    by.y = c("Country.Region", "Date")
  )
  wvv.data %<>% 
    mutate_at(
      c("Cases.high", "Cases.low"),
      round
    ) %>%
    mutate_at(
      c("Cases.high", "Cases.low"),
      as.integer
    )



write_delim(
    wvv.data,
    "code/data/wvvdata.csv",
    delim = ","
)








dt <- wvv.data %>%
  filter(
    Country == "Denmark"
  ) %>%
  mutate(
    Date = Date - 20,
    tmp = Deaths == 0 & lead(Deaths) > 0,
    tmp = cumsum(tmp)
  ) %>%
  filter(
    tmp > 0
  ) %>%
  mutate(
    t = seq_along(Date) - 1
  )

mh <- nls(
  Cases.high ~ (1 + r)**t,
  data = dt,
  start = list(
    "r" = 0.1
  )
)
ml <- nls(
  Cases.low ~ (1 + r)**t,
  data = dt,
  start = list(
    "r" = 0.1
  )
)

predict(mh, tibble("t" = 45))
predict(ml, tibble("t" = 45))



## Load deSolve package
library(deSolve)

## Create an SIR function
sir <- function(time, state, parameters) {

  with(as.list(c(state, parameters)), {

    dS <- -beta * S * I
    dI <-  beta * S * I - gamma * I
    dR <-                 gamma * I

    return(list(c(dS, dI, dR)))
  })
}

### Set parameters
## Proportion in each compartment: Susceptible 0.999999, Infected 0.000001, Recovered 0
init       <- c(
  S = 1 - 1e4 / 5.8e6, 
  I = 1e4 / 5.8e6, 
  R = 1e3 / 5.8e6
)
## beta: infection parameter; gamma: recovery parameter
parameters <- c(beta = 1.2 * 1 / 14, gamma = 1 / 7)
## Time frame
times      <- seq(0, 20, by = 1)

## Solve using ode (General Solver for Ordinary Differential Equations)
out <- ode(y = init, times = times, func = sir, parms = parameters)
## change to data frame
out <- as.data.frame(out)
## Delete time variable
out$time <- NULL
## Show data
head(out, 10)

matplot(x = times, y = out, type = "l",
        xlab = "Time", ylab = "Susceptible and Recovered", main = "SIR Model",
        lwd = 1, lty = 1, bty = "l", col = 2:4)

## Add legend
legend(40, 0.7, c("Susceptible", "Infected", "Recovered"), pch = 1, col = 2:4, bty = "n")







