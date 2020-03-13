library(shiny)
library(reshape2)
library(ggplot2)
library(shiny)
library(tidyverse)
# Load data
data = read.csv2("../csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv", sep=",")
# Drop irrelevant data
drops = c("Lat", "Long", "Province.State")
data = data[ , !(names(data) %in% drops)]
data = aggregate(.~Country.Region, data = data, FUN=sum)
# Prepare for ggplot
data = melt(data, id.vars="Country.Region", variable.name = "Date", value.name="Cases")
data$Date = as.Date(substring(data$Date, 2), format="%m.%d.%y")
subset(data, Country.Region == "US")


data %<>%  group_by(Country.Region) %>% 
  arrange(Date) %>% 
  mutate(NewCases = Cases-lag(Cases),
         NewCases = ifelse(is.na(NewCases), 0, NewCases)) %>% 
  ungroup()
# Define UI for app that draws a histogram ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("COVID-19"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      radioButtons("log","Scale", choices=c("unscaled", "log")),
      selectInput("countries", "Countries", choices=data$Country.Region, selected="Denmark", multiple = T),
      radioButtons("output","Output", choices=c("Total confirmed cases" ="Cases", "New Confrimed cases"= "NewCases"))
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      # Output: Histogram ----
      plotOutput("country_plot")
      
    )
  )
)

# Define server logic required to draw a histogram ----
server <- function(input, output) {

  output$country_plot <- renderPlot({
    
    data_tmp = subset(data, Country.Region %in% c(input$countries)) %>% 
      mutate(Country.Region = factor(Country.Region,levels = c(input$countries)))
    p = ggplot(data_tmp, aes_string(x="Date", y=input$output, colour="Country.Region")) + geom_line()
    if (input$log == "log"){p = p+scale_y_continuous(trans='log10')}
    p = p + theme_minimal()
    p
  })
}
shinyApp(ui = ui, server = server)