make.wvv.data <- reactive({
  
  ## Read data
  demographics <-
    read.csv(
      demo_path,
      sep = ";",
      stringsAsFactors = FALSE,
      dec = ","
    ) %>%
    arrange(Country)
  
  deathdata <-
    read.csv(
      death_path,
      header = TRUE,
      stringsAsFactors = FALSE,
      dec = ","
    ) %>%
    arrange(Country.Region)
  
  confirmeddata <-
    read.csv(
      cases_path,
      header = TRUE,
      stringsAsFactors = FALSE,
      dec = ","
    )
  
  death.by.age <- read.csv(
    deaths_path_age,
    header=TRUE,
    stringsAsFactors = FALSE,
    sep = ";"
  )
  countries.w.age.data = death.by.age$Country
  
  
  ## Prepare deathdata
  drops <- c("Lat", "Long", "Province.State")
  
  deathdata %<>% 
    select(-all_of(drops)) %>%
    group_by(Country.Region) %>%
    summarise_all(sum) %>% 
    ungroup 
  
  dates = as.Date(
    substring(colnames(deathdata)[-1], 2), 
    format = "%m.%d.%y"
  )
  
  confirmeddata %<>% 
    select(-all_of(drops)) %>%
    group_by(Country.Region) %>%
    summarise_all(sum) %>% 
    ungroup
  
  
  demographics %<>% select(-Total.population)
  colnames(demographics)[-1] <- paste0(
    10 * (0:8), rep("-", 8), 9 + 10 * (0:8)
  )
  
  ## Align data
  countries <-
    intersect(
      demographics$Country, 
      deathdata$Country.Region
    )
  
  deathdata %<>%
    filter(
      Country.Region %in% countries
    )
  
  confirmeddata %<>%
    filter(
      Country.Region %in% countries
    )
  
  demographics %<>%
    filter(
      Country %in% countries
    )
  
  death_mat <- 
    deathdata %>%
    select(-Country.Region) %>% 
    as.matrix
  
  demographics %<>%
    select(-Country) %>% 
    as.matrix
  
  ## These two parameters need to be adjusted
  death.rate <- 
    input$wvv.death_rate %>%
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
  make_data <- function(death_mat, relative.death.risk) {
    activedata <- matrix(NA, nrow(death_mat), ncol(death_mat))
    for (i in 1:nrow(activedata)) {
      for (j in 1:ncol(activedata)) {
        demo_adjusted_risk <- relative.death.risk * demographics[i, ]
        est.num.death <-
          demo_adjusted_risk * death_mat[i, j] / sum(demo_adjusted_risk)
        estimate = est.num.death / death.rate
        activedata[i, j] <-
          sum(estimate * is.finite(estimate), na.rm = TRUE)
      }
    }
    activedata = data.frame(activedata)
    colnames(activedata) = dates
    activedata$Country = countries
    return(activedata)
  }
  activedata.low  <- make_data(death_mat, rel.rate.low)
  activedata.high <- make_data(death_mat, rel.rate.high)
  
  ## Actual relevant computation (given p_D(a)) -> Section 7.1
  ## Function to compute binomial confidence bounds on n
  find_confidence_bounds <- function(x, p, alpha = 0.05) {
    n.est <- x / p
    if (p > 0) {
      # lower bound
      lower.n <- ceiling(n.est)
      prob <- 1
      while (prob > alpha / 2 & lower.n >= 0) {
        prob <- pbinom(x - 1, lower.n, p, lower.tail = FALSE)
        lower.n <- lower.n - 1
      }
      lower.n <- lower.n + 1
      # upper bound
      upper.n <- floor(n.est)
      prob <- 1
      while (prob > alpha / 2) {
        prob <- pbinom(x, upper.n, p)
        upper.n <- upper.n + 1
      }
      upper.n <- upper.n - 1
      res <- c(lower.n, upper.n)
    }
    else{
      res <- c(NA, NA)
    }
    return(res)
  }
  
  
  
  # Replace values with bounds for countries with known curves
  
  
  
  
  # Replace values with bounds for countries with known curves
  ##### PBM version #####
  # tictoc::tic()
  # .f2 <- find_confidence_bounds  %>% Vectorize(., vectorize.args = "x")
  # for (country in countries.w.age.data) {
  #   i = which(deathdata$Country.Region == country)
  #   num.death <- death.by.age %>%
  #     filter(
  #       Country == country
  #     ) %>%
  #     select(-Country) %>%
  #     as.numeric
  #   
  #   bounds <- lapply(
  #     death.rate,
  #     function(x) {
  #       .f2(
  #         death_mat[i, ] %>% as.vector, 
  #         x
  #       )
  #     }
  #   )
  #   bounds <-  lapply(
  #       1:length(death.rate),
  #       function(x) {
  #         bounds[[x]] * death.rate[x]
  #       }
  #     ) %>% Reduce("+", .)
  #   activedata.low[i, -ncol(activedata.low)]   <- bounds[1, ]
  #   activedata.high[i, -ncol(activedata.high)] <- bounds[1, ]
  # }
  # tictoc::toc()
  #########################
  for (country in countries.w.age.data) {
    i = which(deathdata$Country.Region == country)
    num.death <- as.numeric(subset(death.by.age, Country == country, select = -c(Country)))
    active_cases <- rep(NA, ncol(death_mat))
    for(j in 1:ncol(death_mat)){
      num.death.scaled <- num.death/sum(num.death)*death_mat[i, j]
      ## bounds <- sapply(death.rate, function(p)
      ##   find_confidence_bounds(death_mat[i, j], p))
      bounds <- sapply(1:length(death.rate), function(k)
        find_confidence_bounds(num.death.scaled[k], death.rate[k]))
      activedata.low[i,j] <- sum(bounds[1,]/death.rate, na.rm=TRUE)
      activedata.high[i,j] <- sum(bounds[2,]/death.rate, na.rm=TRUE)
    }
  }
  
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
  
  return(wvv.data)
})
