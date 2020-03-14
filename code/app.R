library(shiny)
library(reshape2)
library(ggplot2)
library(shiny)
library(tidyverse)
library(scales)
library(shinydashboard)


# Function definitions ----------------------------------------------------

nls2 <- function (formula, data = parent.frame(), start, control = nls.control(), 
          algorithm = c("default", "plinear", "port", "brute-force", 
                        "grid-search", "random-search", "plinear-brute", "plinear-random"), 
          trace = FALSE, weights, ..., all = FALSE) 
{
  if (!inherits(formula, "formula")) 
    formula <- as.formula(formula, env = parent.frame())
  L <- list(formula = formula, data = data, control = control, 
            trace = trace)
  if (!missing(start)) {
    if (inherits(start, "nls")) {
      start <- coef(start)
      start <- start[grep("^[^.]", names(start))]
    }
    L$start <- start
  }
  finIter <- NROW(L$start)
  L <- append(L, list(...))
  algorithm <- match.arg(algorithm)
  if (algorithm == "grid-search") 
    algorithm <- "brute-force"
  call <- match.call()
  if (algorithm == "brute-force" || algorithm == "random-search" || 
      algorithm == "plinear-brute" || algorithm == "plinear-random") {
    nls <- function(formula, data, start, weights, ...) {
      nlsModel <- if (algorithm == "plinear-brute" || algorithm == 
                      "plinear-random") 
        stats:::nlsModel.plinear
      else stats:::nlsModel
      environment(nlsModel) <- environment()
      stop <- function(...) {
        msg <- "singular gradient matrix at initial parameter estimates"
        if (list(...)[[1]] == msg) 
          return()
        stop(...)
      }
      m <- if (missing(weights)) {
        nlsModel(formula, data, start)
      }
      else {
        wts <- eval(substitute(weights), data, environment(formula))
        nlsModel(formula, data, start, wts)
      }
      structure(list(m = m, call = call, convInfo = list(isConv = TRUE, 
                                                         finIter = finIter, finTol = NA)), class = "nls")
    }
  }
  else L$algorithm <- algorithm
  if (missing(start)) 
    return(do.call(nls, L))
  else L$start <- as.data.frame(as.list(start))
  if (NROW(L$start) == 1) 
    return(do.call(nls, L))
  if (NROW(L$start) == 2) {
    if (algorithm == "brute-force" || algorithm == "plinear-brute") {
      rng <- as.data.frame(lapply(start, range))
      mn <- rng[1, ]
      mx <- rng[2, ]
      k1 <- pmax(ceiling(sum(mx > mn)), 1)
      k <- pmax(ceiling(control$maxiter^(1/k1)), 1)
      DF <- as.data.frame(rbind(mn, mx, k))
      finIter <- k^k1
      L$start <- expand.grid(lapply(DF, function(x) seq(x[1], 
                                                        x[2], length = x[3])))
    }
    else {
      finIter <- control$maxiter
      u <- matrix(runif(finIter * NCOL(start)), NCOL(start))
      L$start <- t(u * unlist(start[1, ]) + (1 - u) * unlist(start[2, 
                                                                   ]))
      L$start <- as.data.frame(L$start)
      names(L$start) <- names(start)
    }
  }
  result <- apply(L$start, 1, function(start) {
    L$start <- start
    xx <- try(do.call(nls, L))
    yy <- if (inherits(xx, "try-error")) 
      NA
    else xx
    if (trace) 
      print(yy)
    yy
  })
  if (all) {
    for (i in seq_along(result)) result[[i]]$data <- substitute(data)
  }
  else {
    ss <- lapply(result, function(x) if (identical(x, NA)) 
      NA
      else deviance(x))
    result <- result[[which.min(ss)]]
    result$data <- substitute(data)
  }
  result
}


# Load confirmed data ---------------------------------------------------------------
data <- read.csv2(
  "../csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv",
  sep = ","
)
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
cdata <- "data/pop_data.csv" %>% 
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



# Add recovery data ------------------------------------------------------
data_r <- read.csv2(
  "../csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv",
  sep = ","
)
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
  "../csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv",
  sep = ","
)
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
    "StillSick" = Cases - (Recovered + Deaths),
    "MortalityRate" = (Deaths / Cases) * 100,
    "RecoveryRate"  = (Recovered / Cases) * 100
  )

# Exponential growth models --------------------------------------------------------------
.fit_nls <- function(country, dt) {
  if (length(country) > 1) {
    mm <- list()
    
    for (i in country) {
      fm0 <- lm(
        I(log(Cases + 1)) ~ t,
        data = dt,
        subset = Country.Region == i
      ) %>% coef
      names(fm0) <- c("l", "r")
      
      mm[[i]] <- nls(
        I(Cases + 1) ~ (1 + r)**(t - l),
        data = dt,
        subset = Country.Region == i,
        start = fm0,
        control = nls.control(maxiter = 1e4)
      )
    }
  } else {
    fm0 <- lm(
      I(log(Cases + 1)) ~ t,
      data = dt,
      subset = Country.Region %in% country
    ) %>% coef
    names(fm0) <- c("l", "r")
    
    mm <- NULL
    
    try({
      mm <- nls(
        I(Cases + 1) ~ (1 + r)**(t - l),
        data = filter(
          dt,
          Country.Region %in% country
        ),
        start = fm0,
        control = nls.control(maxiter = 1e5, minFactor = 1 / 2**10)
      )
    }, silent = T)
    
    if (is.null(mm)) {
      mm <- nls2(
        I(Cases + 1) ~ (1 + r)**(t - l),
        data = filter(
          dt,
          Country.Region %in% country
        ),
        start = expand.grid(
          "l" = seq(1, 20, length.out = 100),
          "r" = seq(0, 1, length.out = 100)
        ),
        control = nls.control(maxiter = 1e5, minFactor = 1 / 2**10),
        algorithm = "grid-search"
      )
    }
  }
  
  # fm0 <- lm(
  #   I(log(Cases + 1)) ~ t,
  #   data = dt,
  #   subset = Country.Region %in% country
  # ) %>% coef
  # names(fm0) <- c("l", "r")
  # 
  # mm <- nls(
  #   I(Cases + 1) ~ (1 + r)**(t - l),
  #   data = dt,
  #   subset = Country.Region %in% country,
  #   start = fm0,
  #   control = nls.control(maxiter = 1e4)
  # )
  
  return(
    mm
  )
}

.get_plots <- function(model, country, dt, tmax = NULL) {
  plotdata <- dt %>%
    filter(
      Country.Region %in% country
    ) %>%
    select(
      Cases, t, Country.Region
    ) %>%
    mutate(
      "Method" = "Actual cases"
    )
  
  if (is.null(tmax)) tmax <- max(plotdata$t)
  
  tmpdata <- expand.grid(
    "Country.Region" = country,
    "Method" = "Predicted cases\n(Assuming no interventions)",
    "t" = seq(0, tmax, by = 1)
  )
  
  if (length(country) > 1) {
    predictions <- c()
    
    for (i in country) {
      predictions <- c(predictions, predict(model[[i]], filter(tmpdata, Country.Region == i)) - 1)
    }
    
    tmpdata$Cases <- predictions
  } else {
    tmpdata$Cases <- predict(model, tmpdata) - 1
  }
  
  plotdata <- rbind(
    plotdata,
    select(tmpdata, Cases, t, Method, Country.Region)
  )
  
  plotdata$Group <- as.factor(plotdata$Method):as.factor(plotdata$Country.Region)
  
  return({
    ggplot(
      data = plotdata,
      aes(x = t-1, y = Cases, color = Method)
    ) + 
      geom_line(lwd = 1, alpha = 0.5) + 
      xlab("Days from first case") + 
      ylab("Cumulative cases") + 
      ggtitle(
        paste(
          "Country: ",
          paste(country, sep = ","),
          "\n",
          "Estimated infection rate: ",
          round(coef(model)[2], 3),
          "\n",
          "Estimated lag-phase duration (days): ",
          round(abs(coef(model)[1]), 3),
          sep = ""
        )
      ) +
      theme_minimal() +
      theme(text = element_text(size = 12),legend.position="bottom")+
      labs(colour ="Method:")
  })
}



# UI ----------------------------------------------------------------------
ui <- dashboardPage(
  
  dashboardHeader(title = "COVID-19"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem(text = "Plots", tabName = "plots", icon = icon("dashboard")),
      menuItem(text = "Exponential growth models", tabName = "expmod", icon = icon("dashboard"))
    )
  ),
  
  dashboardBody(
    tabItems(
      # Pane with country plots
      tabItem(
        tabName = "plots",
        
        fluidPage(
          includeHTML("mainpage.html"),
          
          sidebarLayout(
            sidebarPanel(
              radioButtons(
                "log", 
                "Y-axis scale", 
                choices = c("Original scale" = "unscaled", "Logarithmic scale" = "log")
              ),
              
              selectInput(
                "countries",
                "Countries",
                choices = data$Country.Region,
                selected = "Denmark",
                multiple = T
              ),
              
              radioButtons(
                "output",
                "Output",
                choices = c(
                  "Total confirmed cases" = "Cases",
                  "New confirmed cases" = "NewCases",
                  "Still infected" = "StillSick",
                  "Recovered" = "Recovered",
                  "Deaths" = "Deaths",
                  "Percentage of population infected" = "PercentageOfPopulation",
                  "Proportion of deaths among infected" = "MortalityRate",
                  "Proportion of recoveries among infected" = "RecoveryRate"
                )
              ),
              
              
              
              downloadButton("downloadData", "Download Selected Data")
            ),
            
            mainPanel(
              plotOutput("country_plot")
            )
          )
        )
      ),
      
      # Pane with exponential growth models
      tabItem(
        tabName = "expmod",
        
        fluidPage(
          selectInput(
            "expmod_countries",
            "Fit an exponential growth model",
            choices = data$Country.Region,
            selected = "Denmark",
            multiple = F
          ),
          
          box(
            plotOutput("expmod_plot"),
            width = 12
          ),
          
          includeHTML("expmod_descriptions.html")
        )
      )
      
    )
  )
)


yaxislab <- c(
  "Total confirmed cases"= "Cases",
  "New confirmed cases" = "NewCases",
  "Still infected" = "StillSick",
  "Percentage of population infected "= "PercentageOfPopulation" ,
  "Mortality rate (%)" = "MortalityRate",
  "Recovery rate (%)" = "RecoveryRate")


# Server ------------------------------------------------------------------
server <- function(input, output) {
  
  number_ticks <- function(n) {
    function(limits)
      pretty(limits, n)
  }
  
  datasetInput <- reactive({
    data_tmp <- data %>%
      filter(
        Country.Region %in% c(input$countries)
      ) %>%
      mutate(
        Country.Region = factor(Country.Region, levels = c(input$countries))
      ) %>%
      group_by(Country.Region) %>%
      mutate(
        LeadCases = ifelse(
          is.na(lead(Cases)),
          Inf,
          lead(Cases)
        )
      ) %>%
      ungroup %>% {
        LastDayBecoreConfirmedCase <-
          (.) %>% arrange(Date) %>% filter(LeadCases > 0) %>% summarize(min(Date)) %>% pull()
        (.) %>% filter(Date >= LastDayBecoreConfirmedCase)
      } %>%
      select(-LeadCases) %>%
      mutate(
        "t" = (Date - as.Date(min(Date))) %>% as.numeric,
        "PercentageOfPopulation" = (Cases / Population) * 100
      )
  })
  
  

  output$country_plot <- renderPlot({
    

    
    
    p <- ggplot(datasetInput() ,
                aes_string(
                  x = "Date",
                  y = input$output,
                  colour = "Country.Region"
                )) +
      geom_line() +
      scale_x_date(breaks = date_breaks("week"), date_labels = "%b %d")
    if (input$log == "log") {
      p <- p + 
        scale_y_continuous(trans = 'log10') 
    }
    p <- p + theme_minimal()+ 
      ylab(names(yaxislab)[yaxislab == input$output])
    
    p
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("COVID19_", paste(sort(input$countries), collapse = "_"), ".csv", sep =
              "")
    },
    content = function(file) {
      write.csv(datasetInput(), file, row.names = TRUE)
    }
  )
  
  output$expmod_plot <- renderPlot({
    plotdata <- data %>%
      filter(
        Country.Region %in% c(input$expmod_countries)
      ) %>%
      mutate(
        Country.Region = factor(Country.Region, levels = c(input$expmod_countries))
      ) %>%
      group_by(Country.Region) %>%
      mutate(
        LeadCases = ifelse(
          is.na(lead(Cases)),
          Inf,
          lead(Cases)
        )
      ) %>%
      ungroup %>% {
        LastDayBecoreConfirmedCase <-
          (.) %>% arrange(Date) %>% filter(LeadCases > 0) %>% summarize(min(Date)) %>% pull()
        (.) %>% filter(Date >= LastDayBecoreConfirmedCase)
      } %>%
      select(-LeadCases) %>%
      mutate(
        "t" = (Date - as.Date(min(Date))) %>% as.numeric
      )
    
    modelfit <- .fit_nls(input$expmod_countries, plotdata)
    .get_plots(modelfit, input$expmod_countries, plotdata)
  })
  
}
shinyApp(ui = ui, server = server)