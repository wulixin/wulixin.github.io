

Pandas for Data Wrangling
Data Visualization
SQL Databases
Modules and Python Programming
VSCode for Python
Jupyter Notebook Automation with Papermill
Forecasting with Sktime

 #Python for Data Science Automation. Perform an end-to-end business 
 #forecast automation using pandas, sktime, 
 #and papermill, and learn Python in the process.
 # -i https://pypi.tuna.tsinghua.edu.cn/simple
 import pandas as pd 
 import numpy as np 
 import sktime 
 import papermill 
 import matplotlib.pyplot as plt
 
 from pandas.core.base import PandasObject
 
 from sktime.forecasting.arima import AutoARIMA
 from sktime.utils.plotting import plot_series
 
 
**# read csv file
data = pd.read_csv('AirPassengers.csv')
data['Date'] = pd.to_datetime(data['Date'])
data.head()



**# create 12 month moving average
**data['MA12'] = data['Passengers'].rolling(12).mean()
​
**# plot the data and MA
**import plotly.express as px
fig = px.line(data, x="Date", y=["Passengers", "MA12"], template = 'plotly_dark')
fig.show()


**# extract month and year from dates**
data['Month'] = [i.month for i in data['Date']]
data['Year'] = [i.year for i in data['Date']]
​
**# create a sequence of numbers
**data['Series'] = np.arange(1,len(data)+1)
​
**# drop unnecessary columns and re-arrange
**data.drop(['Date', 'MA12'], axis=1, inplace=True)
data = data[['Series', 'Year', 'Month', 'Passengers']] 
​
**# check the head of the dataset**
data.head()


**# split data into train-test set
**train = data[data['Year'] < 1960]
test = data[data['Year'] >= 1960]
​
**# check shape
**train.shape, test.shape


**# import the regression module**
from pycaret.regression import *
​
**# initialize setup**
s = setup(data = train, test_data = test, target = 'Passengers', 
     fold_strategy = 'timeseries', numeric_features = ['Year', 'Series'],
     fold = 3, transform_target = True, session_id = 123)
     

best = compare_models(sort = 'MAE')


prediction_holdout = predict_model(best);


**# generate predictions on the original dataset**
predictions = predict_model(best, data=data)
​
**# add a date column in the dataset**
predictions['Date'] = pd.date_range(start='1949-01-01', end = '1960-12-01', freq = 'MS')
​
**# line plot**
fig = px.line(predictions, x='Date', y=["Passengers", "Label"], template = 'plotly_dark')
​
**# add a vertical rectange for test-set separation**
fig.add_vrect(x0="1960-01-01", x1="1960-12-01", fillcolor="grey", opacity=0.25, line_width=0)fig.show()


final_best = finalize_model(best)


future_dates = pd.date_range(start = '1961-01-01', end = '1965-01-01', freq = 'MS')
​
future_df = pd.DataFrame()
​
future_df['Month'] = [i.month for i in future_dates]
future_df['Year'] = [i.year for i in future_dates]    
future_df['Series'] = np.arange(145 (145+len(future_dates)))future_df.head()


predictions_future = predict_model(final_best, data=future_df)
predictions_future.head()


concat_df = pd.concat([data,predictions_future], axis=0)
concat_df_i = pd.date_range(start='1949-01-01', end = '1965-01-01', freq = 'MS')
concat_df.set_index(concat_df_i, inplace=True)fig = 
​
px.line(concat_df, x=concat_df.index, y=["Passengers", "Label"], template = 'plotly_dark')
fig.show()















     
     









 
 
 
