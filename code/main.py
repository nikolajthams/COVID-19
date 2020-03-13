import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from pandas.plotting import register_matplotlib_converters
register_matplotlib_converters()

data = pd.read_csv("csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")
data = data.drop(["Province/State", 'Lat', 'Long'], axis=1)
data = data.groupby('Country/Region').sum()
data.columns = pd.to_datetime(data.columns)

fig, ax = plt.subplots()
ax.plot(data.loc['United Kingdom'])
ax.plot(data.loc['US'])
ax.plot(data.loc['Italy'])
ax.plot(data.loc['Denmark'])
ax.set_yscale('log')

data.index
