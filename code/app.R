library(shiny)
library(reshape2)
library(ggplot2)
library(shiny)
library(tidyverse)
library(scales)

library(shinydashboard)

# Load data ---------------------------------------------------------------
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
    
    mm <- nls(
      I(Cases + 1) ~ (1 + r)**(t - l),
      data = dt,
      subset = Country.Region %in% country,
      start = fm0,
      control = nls.control(maxiter = 1e4)
    )
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
      "Method" = "Actual"
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
      aes(x = t, y = Cases, color = Method)#, group = Country.Region)
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
      theme(text = element_text(size = 12))
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
          sidebarLayout(
            sidebarPanel(
              radioButtons(
                "log", 
                "Y-axis scale", 
                choices = c("Original scale", "Logarithmic scale")
              ),
              
              radioButtons(
                "response", 
                "Show numbers as:", 
                choices = c("Nominal level", "Percentage of population")
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
                  "Total cases as percentage of population" = "PercentageOfPopulation"
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
            width = 10
          )
        )
      )
      
    )
  )
)


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
      p <- p + scale_y_continuous(trans = 'log10')
    }
    p <- p + theme_minimal()
    
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