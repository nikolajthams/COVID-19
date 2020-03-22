library(ggplot2)

## Read data
demographics <- read.csv("../wirvsvirus/demographics.csv", sep=";", stringsAsFactors=FALSE, dec=",")
deathdata <- read.csv("../csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv", header=TRUE, stringsAsFactors=FALSE, dec=",")
demographics <- demographics[order(demographics$Country),]
deathdata <- deathdata[order(deathdata$Country.Region),]

## Prepare deathdata
drops <- c("Lat", "Long", "Province.State")
deathdata <- deathdata[,!(names(deathdata) %in% drops)]
deathdata <- aggregate(. ~ Country.Region, data = deathdata, FUN = sum)
dates = as.Date(substring(colnames(deathdata)[2:ncol(deathdata)], 2), format = "%m.%d.%y")

demographics <- subset(demographics, select=c(-Total.population))
colnames(demographics)[2:ncol(demographics)] = paste0(10*(0:8), rep("-", 8), 9+10*(0:8))

## Align data
countries <- intersect(demographics$Country, deathdata$Country.Region)
deathdata = subset(deathdata, Country.Region %in% countries)
demographics = subset(demographics, Country %in% countries)

death_mat = as.matrix(subset(deathdata, select=c(-Country.Region)))
demographics = as.matrix(subset(demographics, select=c(-Country)))

########################################

## These two parameters need to be adjusted
relative.death.risk <- c(0, 0, 0, 0.001887880854,
                         0.004195290786, 0.01305784257,
                         0.06544653626, 0.2743720174, 0.6410404321)

# numbers from south korea
# https://en.wikipedia.org/wiki/Coronavirus_disease_2019#Prognosis
death.rate <- c(0, 0, 0, 0.0011, 0.0008, 0.0042, 0.0152, 0.0628, 0.1024)

## Actual relevant computation
activedata <- matrix(NA, nrow(death_mat), ncol(death_mat))
for(i in 1:nrow(activedata)){
  for(j in 1:ncol(activedata)){
    demo_adjusted_risk <- relative.death.risk * demographics[i,]
    est.num.death <- demo_adjusted_risk*death_mat[i, j]/sum(demo_adjusted_risk)
    activedata[i, j] <- sum(est.num.death/death.rate, na.rm=TRUE)
  }
}
activedata = data.frame(activedata)
colnames(activedata) = dates
activedata$Country = countries

activedata <- melt(
  activedata,
  id.vars = "Country",
  variable.name = "Date",
  value.name = "Cases"
)
activedata$Date = as.Date(activedata$Date)
deathdata <- melt(
  deathdata,
  id.vars = "Country.Region", 
  variable.name = "Date", 
  value.name = "Deaths"
)
deathdata$Date = as.Date(substring(deathdata$Date, 2), format = "%m.%d.%y")

wvv.data = merge(activedata, deathdata, by.x=c("Country", "Date"), by.y=c("Country.Region", "Date"))

make_estimate_plot = function(input){
  firstDeath = min(subset(wvv.data, (Deaths != 0 & Country %in% input$wvv.countries))$Date)
  p = ggplot(subset(wvv.data, (Country %in% input$wvv.countries & Date >= firstDeath)), 
             aes(colour=Country, group=Country)) + 
    geom_line(aes(x = Date, y = Deaths)) +
    geom_line(aes(x = Date - input$wvv.death_delay, y = Cases), linetype="dashed") +
    scale_x_date(breaks = date_breaks("week"), date_labels = "%b %d")
  if (input$wvv.log == "log") {
    p <- p + scale_y_log10()
  }
  return(p)
}


