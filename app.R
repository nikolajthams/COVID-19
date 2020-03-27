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
theme_set(theme_minimal())

# Define data paths -------------------------------------------------------
source("code/data_paths.R")

# Function definitions ----------------------------------------------------

nls2 <- function(formula, data = parent.frame(), start, control = nls.control(),
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
  cases_path,
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
    "RecoveryRate"  = (Recovered / Cases) * 100
  ) %<>% 
  group_by(Country.Region) %>%
  arrange(Date) %>%
  mutate(NewDeaths = Deaths - lag(Deaths),
         NewDeaths = ifelse(is.na(NewDeaths), 0, NewDeaths)) %>%
  ungroup()
# Load Danish data from SSI -----------------------------------------------
ssi <- ssi_path %>%
  read_delim(
    .,
    delim = ",",
    locale = locale(grouping_mark = ".")
  ) %>%
  dplyr::select(-X1) %>%
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
    Date = gsub(". marts", "/03/2020", Date) %>% as.Date(., format = "%d/%m/%Y"),
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
        )# ,
        # `Antal testede personer` = `Antal testede personer` / 10
      )
    return(data)
  }
) %>% 
  do.call("rbind", .) %>%
  ungroup


# Exponential growth models --------------------------------------------------------------
.fit_nls <- function(country, dt, get_convergence = F) {
  if (length(country) > 1) {
    mm <- list()
    fm0 <- list()
    conv <- list()
    for (i in country) {
      fm0[[i]] <- lm(
        I(log(Cases + 1)) ~ t,
        data = dt,
        subset = Country.Region == i
      ) %>% coef
      names(fm0[[i]]) <- c("l", "r")
      
      
      try({
        mm[[i]] <- nls(
          I(Cases + 1) ~ (1 + r)**(t - l),
          data = filter(
            dt,
            Country.Region == i
          ),
          start = fm0[[i]],
          control = nls.control(maxiter = 1e5, minFactor = 1 / 2**10)
        )
        conv[[i]] <- "Yes"
      }, silent = T)
      
      if (is.null(mm[[i]])) {
        mm[[i]] <- nls2(
          I(Cases + 1) ~ (1 + r)**(t - l),
          data = filter(
            dt,
            Country.Region == country
          ),
          start = expand.grid(
            "l" = seq(1, 20, length.out = 40),
            "r" = seq(0, 1, length.out = 100)
          ),
          control = nls.control(maxiter = 1e3, minFactor = 1 / 2**10),
          algorithm = "grid-search"
        )
        conv[[i]] <- "No"
      }
      
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
          "l" = seq(1, 20, length.out = 40),
          "r" = seq(0, 1, length.out = 100)
        ),
        control = nls.control(maxiter = 1e3, minFactor = 1 / 2**10),
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
  
  if (get_convergence) {
    return(
      list(mm, conv)
    )
  } else {
    return(
      mm
    )
  }
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
  
  return({ggplotly(
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
      theme(text = element_text(size = 12),legend.position="bottom") +
      labs(colour ="Method:")
  )})
}



# UI ----------------------------------------------------------------------
ui <- dashboardPage(
  dashboardHeader(title = "COVID19"),
  
  dashboardSidebar(width = 250,
                   sidebarMenu(
                     menuItem(
                       text = "Plots", tabName = "plots", icon = icon("bar-chart-o")
                     ),
                     menuItem(
                       text = "Danish data on COVID19 tests", tabName = "ssidat", icon = icon("bar-chart-o")
                     ),
                     menuItem(
                       text = "Estimated number of dark cases", tabName = "wirvsvirus", icon = icon("file-alt")
                     ),
                     menuItem(
                       text = "Exponential growth models", tabName = "expmod_head", icon = icon("dashboard"),
                       menuSubItem(
                         text = "Detailed model descriptions",
                         tabName = "expmod_sub1",
                         icon = icon("file-alt")
                       ),
                       menuSubItem(
                         text = "Fit models",
                         tabName = "expmod",
                         icon = icon("dashboard")
                       ),
                       menuSubItem(text = "Compare models by country", tabName = "tables", icon = icon("table"))
                     ),
                     
                     menuItem(text = "About", tabName = "mainpage", icon = icon("file-alt"))
                   )
  ),
  
  dashboardBody(
    tags$head(HTML("<script async src='https://www.googletagmanager.com/gtag/js?id=UA-160709431-1'
></script>")),
    tags$head(includeScript("code/analytics.js")),
    tabItems(
      # Welcome page
      tabItem(
        tabName = "mainpage",
        
        fluidPage(
          withMathJax(
            includeMarkdown("code/docs/mainpage.Rmd")
          )
        )
      ),
      
      # Pane with country plots
      tabItem(
        tabName = "plots",
        
        fluidPage(
          sidebarLayout(
            sidebarPanel(
              radioButtons(
                "log",
                "Y-axis scale",
                choices = c("Original scale" = "unscaled", "Logarithmic scale" = "log"),
                selected="log"
              ),
              
              selectInput(
                "countries",
                "Countries",
                choices = data$Country.Region,
                selected = c("Denmark", "Italy", "United Kingdom", "US", "Spain"),
                multiple = T
              ),
              checkboxInput("rebase", "View graph from patient number x", TRUE),
              conditionalPanel("input.rebase",
                               numericInput('rebase.value', 'Patient number', value=100, min=1, step=20)),
              
              radioButtons(
                "output",
                "Output",
                choices = c(
                  "Total confirmed cases" = "Cases",
                  "New confirmed cases" = "NewCases",
                  "Still infected" = "StillInfected",
                  "Recovered" = "Recovered",
                  "Total deaths" = "Deaths",
                  "New deaths" = "NewDeaths",
                  "Percentage of population infected" = "PercentageOfPopulation",
                  "Proportion of deaths among infected" = "MortalityRate",
                  "Proportion of recoveries among infected" = "RecoveryRate"
                )
              ),
              checkboxInput("barchart", "View as bar chart (works well with 'New deaths' and 'New cases' when few countries are selected)", FALSE),
              
              
              downloadButton("downloadData", "Download Selected Data")
            ),
            
            mainPanel(
              
              div(
                style = "position:relative",
                plotlyOutput("country_plot"), 
                # hover = hoverOpts("plot_hover", delay = 100, delayType = "debounce")),
                uiOutput("hover_info")
              ),
              fluidPage(
                withMathJax(
                  includeMarkdown("code/docs/text_below_plot.md")
                )
              )
            )
          ),
          
          textOutput("JH_data_lastupdate")
        )
      ),
      
      # Pane with Danish data:
      tabItem(
        tabName = "ssidat",
        
        fluidPage(
          box(
            includeMarkdown("code/docs/ssi_doc.Rmd"),
            width = 12
          ),
          
          radioButtons(
            "ageYN", "View:",
            choices = c(
              "Daily number of tests and percentage positive" = "tot",
              "Cumulative Number of tests and percentage positive by age group" = "age"
            )
          ),
          
          plotlyOutput("ssiplot")
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
            plotlyOutput("expmod_plot"),
            width = 12
          ),
          
          br(), br(), br(),
          withMathJax(
            includeMarkdown("code/docs/expmod_descriptions.Rmd")
          )
        )
      ),
      
      # Pane with model text
      tabItem(
        tabName = "expmod_sub1",
        
        fluidPage(
          withMathJax(
            includeMarkdown("code/docs/expmod_detailed.Rmd")
          )
        )
      ),
      
      # Pane with comparison between models
      tabItem(
        tabName = "tables",
        
        fluidPage(
          verbatimTextOutput("expmod_tables_lastupdate"),
          
          dataTableOutput("expmod_tables")
        )
      ),
      
      # Pane with wvv data
      tabItem(
        tabName = "wirvsvirus",
        (
          sidebarLayout(
            
            sidebarPanel(
              
              radioButtons(
                "wvv.log",
                "Y-axis scale",
                choices = c("Original scale" = "unscaled", "Logarithmic scale" = "log"),
                selected = "log"
              ),
              radioButtons(
                "wvv.compare_ouput",
                "Compare estimator to",
                choices = c("Deaths" = "deaths", "Confirmed cases" = "confirmed_cases"),
                selected="confirmed_cases"
              ),
              
              selectInput(
                "wvv.countries",
                "Countries",
                choices = data$Country.Region,
                selected = c("Germany", "Italy", "Spain"),
                multiple = T
              ),
              
              numericInput(
                "wvv.death_delay",
                "Days from illness onset to death",
                value = 20,
                min = 1
              ),
              textInput(
                "wvv.death_rate",
                "Case fatality rate for each age group (0-9, 10-19, ...) [comma-separated]",
                value=c("0, 0, 0, 0.0011, 0.0008, 0.0042, 0.0152, 0.0628, 0.1024")
              ),
              h5("Default case fatality rate: South Korea")
              # textInput(
              #   "wvv.rel_risk",
              #   "Relative risk (comma-separated)",
              #   value=c("0, 0, 0, 0.0014,0.004,0.013,0.065,0.274,0.641")
              # ),
              # sliderInput(
              #   "wvv.dr1",
              #   "Death rate 0-9",
              #   min=0, max=1, value=0
              # )
            ),
            
            mainPanel(
              div(
                style = "position:relative",
                plotlyOutput("wirvsvirus"),
              ),
              h6("Solid curves indicate confirmed numbers. Shaded regions are estimated number of infected, measured from illness onset."),
              textOutput("countries.age.data"),
              textOutput("countries.no.age.data"),
              fluidPage(
                withMathJax(
                  includeMarkdown("code/docs/wvv_explanation.md")
                )
              )
            )
          )
        )
      )
    )
  )
)


yaxislab <- c(
  "Total confirmed cases" = "Cases",
  "New confirmed cases" = "NewCases",
  "Still infected" = "StillInfected",
  "Cumulative recovered patients" = "Recovered",
  "Cumulative deaths" = "Deaths",
  "Population infected (%)" = "PercentageOfPopulation" ,
  "Mortality rate (%)" = "MortalityRate",
  "Recovery rate (%)" = "RecoveryRate",
  "New deaths" = "NewDeaths")


# Server ------------------------------------------------------------------
server <- function(input, output) {
  
  source("code/make_wvv_data_v2.R", local = T)
  
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
      filter(
        LeadCases > ifelse(input$rebase == TRUE, input$rebase.value, 0)
      ) %>%
      # ungroup %>% {
      #   LastDayBecoreConfirmedCase <-
      #     (.) %>% arrange(Date) %>% filter(LeadCases > ifelse(input$rebase == TRUE, input$rebase.value, 0)) %>% summarize(min(Date)) %>% pull()
      #   (.) %>% filter(Date >= LastDayBecoreConfirmedCase)
      # } %>%
      select(-LeadCases) %>%
      mutate(
        "t" = (Date - as.Date(min(Date))) %>% as.numeric,
        "PercentageOfPopulation" = (Cases / Population) * 100
      ) %>% ungroup
  })
  
  output$countries.age.data <- renderText({
    paste("Countries with age data:", paste(intersect(input$countries, countries.w.age.data), collapse = ", "), collapse="")
  })
  output$countries.no.age.data <- renderText({
    paste("Countries with no age data:", paste(setdiff(input$countries, countries.w.age.data), collapse = ", "), collapse="")
  })
  
  
  output$JH_data_lastupdate <- renderText({
    paste(
      "Johns Hopkins data was last updated at:",
      file.info(cases_path)$mtime,
      "(Central European Time)",
      sep = " "
    )
  })
  
  output$country_plot <- renderPlotly({
    patient.x.name = paste("Days since patient", input$rebase.value)
    
    if(input$rebase == TRUE){
      p <- ggplot(datasetInput()%>%rename(!!patient.x.name:="t"),
                  aes_string(
                    x = paste("`", patient.x.name, "`", sep = ""),
                    y = input$output,
                    colour = "Country.Region", 
                    label = "Date"
                  )) + 
        xlab(paste("Days since patient ", input$rebase.value)) + scale_x_continuous(breaks=c(0, seq(7,1000,7)))# + geom_point(aes_string(text = hover.date))
    } else {
      p <- ggplot(datasetInput(),
                  aes_string(
                    x = "Date",
                    y = input$output,
                    colour = "Country.Region"
                  )) + 
        scale_x_date(breaks = date_breaks("week"), date_labels = "%b %d")
    }
    if (input$log == "log") {
      p <- p + scale_y_log10()
    }
    p <- p + theme_minimal() +
      ggtitle(names(yaxislab)[yaxislab == input$output]) + 
      theme(plot.title = element_text(hjust = 0.5)) + 
      ylab(names(yaxislab)[yaxislab == input$output]) +
      labs(colour="Country")
    if (input$barchart){#(input$output %in% c("NewCases", "NewDeaths")){
      p = p + geom_bar(aes(fill=Country.Region), position="dodge", stat="identity", alpha=1, lwd = 0.1)
    } else {
      p = p + geom_line() + geom_point(alpha=0.5, size=1.2) 
    }
    p = ggplotly(p)
    p
  })
  
  output$hover_info <- renderUI({
    hover <- input$plot_hover
    point <- nearPoints(datasetInput(), hover, threshold = 10, maxpoints = 1, addDist = TRUE)
    if (nrow(point) == 0) return(NULL)
    
    # calculate point position INSIDE the image as percent of total dimensions
    # from left (horizontal) and from top (vertical)
    left_pct <- (hover$x - hover$domain$left) / (hover$domain$right - hover$domain$left)
    top_pct <- (hover$domain$top - hover$y) / (hover$domain$top - hover$domain$bottom)
    
    # calculate distance from left and bottom side of the picture in pixels
    left_px <- hover$range$left + left_pct * (hover$range$right - hover$range$left)
    top_px <- hover$range$top + top_pct * (hover$range$bottom - hover$range$top)
    
    # create style property fot tooltip
    # color is set so tooltip is a bit transparent
    # z-index is set so we are sure are tooltip will be on top
    style <- paste0("position:absolute; z-index:100; background-color: rgba(245, 245, 245, 0.85); ",
                    "left:", left_px + 2, "px; top:", top_px + 2, "px;")
    
    # actual tooltip created as wellPanel
    wellPanel(
      style = style,
      p(HTML(paste0("<b> Country: </b>", point$Country.Region, "<br/>",
                    "<b> Date: </b>", point$Date, "<br/>",
                    "<b> Value: </b>", point[,input$output], "<br/>"
      )))
    )
  })
  
  
  
  output$dynamic <- renderPrint({
    req(input$plot_hover)
    verbatimTextOutput("vals")
  })
  
  output$vals <- renderPrint({
    hover <- input$plot_hover
    HoverData <- nearPoints(datasetInput(),input$plot_hover) %>%  select(Country.Region,Date,input$output)
    req(nrow(HoverData) != 0)
    knitr::kable(HoverData, "pandoc")
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
  
  output$expmod_plot <- renderPlotly({
    validate(
      need(
        input$expmod_countries != "",
        'Please select a country to analyse.'
      )
    )
    
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
    
    modelfit <- .fit_nls(input$expmod_countries, plotdata, F)
    .get_plots(modelfit, input$expmod_countries, plotdata)
  })
  
  output$expmod_tables_lastupdate <- renderText({
    paste(
      "Models were last updated at:",
      file.info("code/data/all_models.csv")$mtime,
      "(Central European Time)",
      sep = " "
    )
  })
  
  output$expmod_tables <- renderDataTable({
    all_models <- "code/data/all_models.csv" %>%
      read_delim(
        .,
        delim = ";"
      )
    
    all_models
  })
  
  output$wirvsvirus <- renderPlotly({
    wvv.data <- make.wvv.data()
    
    
    make_estimate_plot <- function(input) {
      if (input$wvv.compare_ouput == "confirmed_cases") {
        firstDate <- 
          wvv.data %>%
          filter(
            ConfirmedCases != 0,
            Country %in% input$wvv.countries
          ) %>%
          pull(Date) %>%
          min
      } else {
        firstDate <- 
          wvv.data %>%
          filter(
            Deaths != 0,
            Country %in% input$wvv.countries
          ) %>%
          pull(Date) %>%
          min
      }
      wvv.data %<>% filter(
        Country %in% input$wvv.countries,
        Date >= firstDate
      ) %>%
        mutate(
          "Date " = as.Date(Date - input$wvv.death_delay)
        )
      
      p <- ggplot(
        # data = filter(
        #   wvv.data,
        #   Country %in% input$wvv.countries,
        #   Date >= firstDate
        # ),
        data = wvv.data,
        aes(
          colour = Country
        )
      ) +
        scale_x_date(breaks = date_breaks("week"), date_labels = "%b %d") +
        ylab("Number of cases")
      
      if (input$wvv.compare_ouput == "confirmed_cases") {
        p <- p + 
          geom_line(
            aes(
              x = Date, y = ConfirmedCases
            )
          )
      } else {
        p <- p + 
          geom_line(
            aes(
              x = Date, y = Deaths
            )
          )
      }
      # wvv.data %<>% mutate(Date2 = as.Date(Date - input$wvv.death_delay))
      p <- p + geom_ribbon(
        aes(
          x = `Date `,
          ymin = Cases.low,
          ymax = Cases.high,
          fill = Country,
          text1 = Cases.high,
          text2 = Cases.low
        ),
        alpha = 0.3
      )
      
      if (input$wvv.log == "log") {
        p <- p + scale_y_log10(
          # labels = function(x) format(x, scientific = F),
          # oob = squish_infinite
        )
      } 
      # else {
      #   p <- p + scale_y_continuous(
      #     labels = function(x) format(x, scientific = F)
      #   )
      # }
      
      gg_color_hue <- function(n) {
        hues = seq(15, 375, length = n + 1)
        hcl(h = hues, l = 65, c = 100)[1:n]
      }
      
      return(p)
    }
    
    p <- make_estimate_plot(input)
    p <- ggplotly(p, tooltip = c(
      "colour", "x", "y", "text1", "text2"
    ))
    
    for (i in 1:length(p$x$data)) {
      if (grepl(",1", p$x$data[[i]]$legendgroup)) {
        p$x$data[[i]]$legendgroup <- gsub(
          "[()]|,|1",
          "",
          p$x$data[[i]]$legendgroup
        )
        
        p$x$data[[i]]$name <- paste(
          gsub(
            "[()]|,|1",
            "",
            p$x$data[[i]]$name
          ),
          ", confirmed",
          sep = ""
        )
      } else {
        p$x$data[[i]]$name <- paste(
          p$x$data[[i]]$name,
          ", estimated",
          sep = ""
        )
      }
    }
    
    p
    
    
  })
  
  
  output$ssiplot <- renderPlotly({
    if (input$ageYN == "tot") {
      p <- ggplot(
        data = ssi %>%
          mutate(
            InfectionRate = round(InfectionRate * 100, 4)
          ) %>%
          rename(
            "Number of tested people, daily" = Tested,
            "Percentage of tests positive, daily" = InfectionRate
          ) %>%
          dplyr::select(
            Date,
            "Number of tested people, daily",
            "Percentage of tests positive, daily"
          ) %>%
          melt(
            id.vars = "Date"
          ),
        aes(
          x = Date,
          y = value,
          colour = T
        )
      ) +
        scale_x_date(breaks = date_breaks("week"), date_labels = "%b %d") +
        geom_line() +
        geom_point() + 
        facet_wrap(
          ~ variable,
          scales = "free_y"
        ) + 
        ylab("") +
        theme_minimal()
      
      p <- ggplotly(p, tooltip = c("Date", "value"))
      for (i in 1:length(p$x$data)){
        # p2$x$data[[i]]$text <- c(p$x$data[[i]]$text, "") 
        p$x$data[[i]]$showlegend <- FALSE
      }
      p
      
      
    } else {
      plotdata <- agedata %>%
        mutate(
          "Percentage of tests positive, total" = round(Laboratoriebekræftede / `Antal testede personer` * 100, 4)
        ) %>%
        rename(
          "Number of tested people, total" = "Antal testede personer"
        ) %>%
        dplyr::select(
          Date,
          Aldersgrupper,
          "Number of tested people, total",
          "Percentage of tests positive, total"
        ) %>%
        melt(
          id.vars = c("Date", "Aldersgrupper")
        )
      
      
      p <- ggplot(
        data = plotdata,
        aes(
          x = Date,
          y = value,
          colour = Aldersgrupper
        )
      ) + 
        geom_line() + 
        geom_point() + 
        scale_x_date(labels = date_format("%b %d")) + 
        facet_wrap(
          ~ variable,
          scales = "free"
        ) +
        theme_minimal() + 
        theme(
          text = element_text(size = 12),
          legend.position = "bottom"
        ) +
        ylab("")
      
      ggplotly(p) %>% 
        layout(
          legend = list(
            orientation = "h",
            y = -0.2
          )
        )
    }
  })
  
  
}
shinyApp(ui = ui, server = server)
