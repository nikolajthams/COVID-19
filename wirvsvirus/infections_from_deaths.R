####
# Test script to estimate active cases based on total deaths
####


## Read data
demographics <- read.csv("demographics.csv", sep=";", stringsAsFactors=FALSE)
deathdata <- read.csv("../csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv", header=TRUE, stringsAsFactors=FALSE)
demographics <- demographics[order(demographics$Country),]
deathdata <- deathdata[order(deathdata$Country.Region),]

## Align data
cdeath <- deathdata$Country.Region
cdemo <- demographics$Country
countries <- intersect(cdemo, cdeath)

# subselect countries with both data (demographics)
demographics_mat <- as.matrix(demographics[cdemo %in% countries,
                                           startsWith(colnames(demographics), "X")])
demographics_mat <- sub(",", ".", demographics_mat, fixed = TRUE)
class(demographics_mat) = "numeric"
# subselect countries with both data (deathdata)
death_mat <- as.matrix(deathdata[cdeath %in% countries,
                                 startsWith(colnames(deathdata), "X")])
# average over provinces/states
death_mat_unique <- matrix(NA, length(countries), ncol(death_mat))
rownames <- cdeath[cdeath %in% countries]
for(k in 1:length(countries)){
  death_mat_unique[k,] <- colSums(death_mat[rownames == countries[k],,drop=FALSE])
}

## TMP adjust demographics (should be done in the csv file)
demographics_mat[,9] <- demographics_mat[,9] + demographics_mat[,10]
demographics_mat <- demographics_mat[,-10]

########################################

## These two parameters need to be adjusted
relative.death.risk <- c(0, 0, 0, 0.001887880854,
                         0.004195290786, 0.01305784257,
                         0.06544653626, 0.2743720174, 0.6410404321)
#death.rate <- c(0, 0, 0, 0.01, 0.04, 0.06, 0.15, 0.24, 0.5)
# numbers from south korea
# https://en.wikipedia.org/wiki/Coronavirus_disease_2019#Prognosis
death.rate <- c(0, 0, 0, 0.0011, 0.0008, 0.0042, 0.0152, 0.0628, 0.1024)


## Actual relevant computation
active_mat <- matrix(NA, nrow(death_mat_unique), ncol(death_mat_unique))
active_mat_lower <- matrix(NA, nrow(death_mat_unique), ncol(death_mat_unique))
active_mat_upper <- matrix(NA, nrow(death_mat_unique), ncol(death_mat_unique))
for(i in 1:nrow(death_mat_unique)){
  for(j in 1:ncol(death_mat_unique)){
    demo_adjusted_risk <- relative.death.risk * demographics_mat[i,]
    pAgivenD <- demo_adjusted_risk/sum(demo_adjusted_risk)
    est.num.death <- death_mat_unique[i, j]*pAgivenD
    active_mat[i, j] <- sum(est.num.death/death.rate, na.rm=TRUE)
  }
}

## Test plot for Italy
plot(active_mat[66,], type="l", col="black")
lines(active_mat_lower[66,], col="green")
lines(active_mat_upper[66,], col="green")
lines(death_mat_unique[66,], col="red")

## Test plot for Denmark
plot(active_mat[36,], type="l", col="black")
lines(death_mat_unique[36,], col="red")


## Actual relevant computation (given p_D(a)) -> Section 1.1
## Function to compute binomial confidence bounds on n
find_confidence_bounds <- function(x, p, alpha=0.05){
  n.est <- x/p
  if(p>0){
    # lower bound
    lower.n <- floor(n.est)
    prob <- 1
    while(prob > alpha/2 & lower.n >= 0){
      prob <- pbinom(x-1, lower.n, p, lower.tail=FALSE)
      lower.n <- lower.n-1
    }
    lower.n <- lower.n + 1
    # upper bound
    upper.n <- ceiling(n.est)
    prob <- 1
    while(prob > alpha/2){
      prob <- pbinom(x, upper.n, p)
      upper.n <- upper.n+1
    }
    upper.n <- upper.n - 1
    res <- c(lower.n, upper.n)
  }
  else{
    res <- c(NA, NA)
  }
  return(res)
}


# value for Italy
num.death <- c(0,0,0,9,25,83,312,1090,1528)
active_cases <- rep(NA, ncol(death_mat_unique))
active_cases_lower <- rep(NA, ncol(death_mat_unique))
active_cases_upper <- rep(NA, ncol(death_mat_unique))
for(j in 1:ncol(death_mat_unique)){
  num.death.scaled <- num.death/sum(num.death)*death_mat_unique[66, j]
  active_cases[j] <- sum(num.death.scaled/death.rate, na.rm=TRUE)
  bounds <- sapply(death.rate, function(p)
    find_confidence_bounds(death_mat_unique[i, j], p))
  active_cases_lower[j] <- sum(bounds[1,]/death.rate, na.rm=TRUE)
  active_cases_upper[j] <- sum(bounds[2,]/death.rate, na.rm=TRUE)
}


## Test plot for Italy
plot(active_cases, type="l", col="black")
lines(active_cases_lower, col="green")
lines(active_cases_upper, col="green")
lines(death_mat_unique[66,], col="red")
