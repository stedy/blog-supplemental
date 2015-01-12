import pandas as pd
import numpy as np
import datetime as dt

bars = pd.read_csv("../data/bars_2014.csv")
run = pd.read_csv("../data/run_2014.csv")

def get_week(row):
    return dt.datetime.strptime(row['date'], "%Y-%m-%d").isocalendar()[1]

def calories_out(row):
    return int(row['distance'] * 145)

def calories_in(row):
    return row['count'] * 2 * 180

def residual(row):
    return row['cal_out'] - row['cal_in']

bargroup = bars.groupby('date')

runm = pd.DataFrame(bargroup.count())

runm.columns = ['count']
run = run.set_index('date')
run = run.fillna(0)

runm['date'] = runm.index

merged = run.join(runm)

merged['week'] = merged.apply(get_week, axis=1)
merged['cal_out'] = merged.apply(calories_out, axis=1)
merged['cal_in'] = merged.apply(calories_in, axis=1)
merged['residual'] = merged.apply(residual, axis=1)
merged.drop(['date', 'distance', 'count'], axis=1,inplace=True)

merged = merged.set_index('week')
print merged.stack()
