#!/bin/bash
cd "/Users/rnm667/OneDrive - Københavns Universitet/PhD/Projects/COVID-19"

primary=/Volumes/groupdirs/SCIENCE-MATH-ShinyWWW/PBM/COVID19
secondary="/Volumes/groupdirs/SCIENCE-MATH-ShinyWWW/PBM/COVID19 - Copy"

rm -f $primary/time_series_19_*
rm -f $secondary/time_series_19_*

sudo cp -f csse_covid_19_data/csse_covid_19_time_series/* $primary/ && echo updated data

sudo cp -f csse_covid_19_data/csse_covid_19_time_series/* $secondary/ &&  echo updated data

cd $primary
R CMD BATCH $primary/compute_all_models.R && echo computed new models
cd $secondary
R CMD BATCH $secondary/compute_all_models.R && echo computed new models

