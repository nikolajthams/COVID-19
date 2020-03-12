import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import matplotlib.dates as mdates
from datetime import datetime

months = mdates.MonthLocator()  # every month
years_fmt = mdates.DateFormatter('%M')
dt = lambda x: datetime.strptime(x, '%m/%d/%y')


data = pd.read_csv("csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")
data = data.drop(["Province/State", 'Lat', 'Long'], axis=1)
data = data.groupby('Country/Region').sum()
data.columns = pd.to_datetime(data.columns)

fig, ax = plt.subplots()
ax.plot(data.loc['Denmark'])
ax.plot(data.loc['US'])
ax.set_yscale('log')
 
