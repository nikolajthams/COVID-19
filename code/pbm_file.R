library(tidyverse)
library(shiny)
library(reshape2)
library(ggplot2)
library(shiny)



# From NTT ----------------------------------------------------------------
  data = read.csv2("../csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv", sep = ",")
  # Drop irrelevant data
  drops = c("Lat", "Long", "Province.State")
  data = data[ , !(names(data) %in% drops)]
  data = aggregate(.~Country.Region, data = data, FUN = sum)
  # Prepare for ggplot
  data = melt(data, id.vars = "Country.Region", variable.name = "Date", value.name = "Cases")
  data$Date = as.Date(substring(data$Date, 2), format = "%m.%d.%y")

# Enrich data -------------------------------------------------------------
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

dt <- left_join(
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
) %>%
  mutate(
    "t" = (Date - as.Date(min(Date))) %>% as.numeric
  ) %>%
  rename(
    "Country" = "Country.Region"
  ) %>%
  mutate_at(
    "Country",
    as.factor
  )


# Fit models --------------------------------------------------------------
.fit_nls <- function(country, dt) {
  fm0 <- nls(
    I(log(Cases + 1)) ~ (t-l) * (1 + r),
    data = dt,
    subset = Country == country,
    start = list("r" = 1, "l" = 1),
    control = nls.control(maxiter = 5e3, minFactor = 1/2048)
  ) %>% coef
  
  mm <- nls(
    I(Cases + 1) ~ (1 + r)**(t-l),
    data = dt,
    subset = Country == country,
    start = fm0,
    control = nls.control(maxiter = 5e3, minFactor = 1/2048)
  )
  
  return(
    mm
  )
}

.get_plots <- function(model, country, dt, tmax = max(dt$t)) {
  # plotdata <- expand_grid(
  #   "Method" = c("Actual", "Predicted"),
  #   "t" = filter(dt, Country == country)$t
  # )
  plotdata <- dt %>%
    filter(
       Country == country
    ) %>%
    select(
      Cases, t
    ) %>%
    mutate(
      "Method" = "Actual"
    )
  tmpdata <- expand_grid(
    "Method" = "Predicted cases\n(Assuming no interventions)",
    "t" = seq(0, tmax, by = 1)
  )
  tmpdata$Cases <- predict(model, tmpdata) - 1
  
  plotdata <- rbind(
    plotdata,
    select(tmpdata, Cases, t, Method)
  )
  
  return({
    ggplot(
      data = plotdata,
      aes(x = t, y = Cases, color = Method)
    ) + 
      geom_line(
        lty = c(
          rep(1, nrow(plotdata) - nrow(tmpdata)),
          rep(2, nrow(tmpdata))
        ),
        lwd = 1,
        alpha = 0.5
      ) + 
      xlab("Days from January 22, 2020") + 
      ylab("Cumulative cases") + 
      ggtitle(
        paste(
          "Country: ",
          country,
          "\n",
          "Estimated infection rate: ",
          round(coef(model)[1], 3),
          "\n",
          "Estimated lag-phase duration (days): ",
          round(coef(model)[2], 3),
          sep = ""
        )
      ) +
      theme_minimal()
  })
}


# Examples ----------------------------------------------------------------
# mm <- .fit_nls("Sweden", dt)
# .get_plots(mm, "Sweden", dt)
