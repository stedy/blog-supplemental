import argparse
import datetime as dt
import json
import pandas as pd
import numpy as np
from ggplot import *

parser = argparse.ArgumentParser(description = """Yearly summary trends for
                                    LibraryThing JSON data""")
parser.add_argument('json', help = "JSON file from LibraryThing")
args = parser.parse_args()

raw_data = open(args.json)
data = json.load(raw_data)

def convert_date(row):
    return dt.datetime.strptime(row['entrydate'], "%Y-%m-%d").strftime("%Y")

def get_week(row):
    return dt.datetime.strptime(row['entrydate'], "%Y-%m-%d").isocalendar()[1]

results = []
for key in data.keys():
    try:
        results.append((str(key), data[key]['rating'],
            str(data[key]['entrydate'])))
    except:
       pass

df = pd.DataFrame(results)
df.columns = ['key', 'rating', 'entrydate']
df['year'] = df.apply(convert_date, axis=1)

mean_table = pd.pivot_table(df, values='rating',
        index='year', aggfunc = np.mean)

print mean_table

df['week'] = df.apply(get_week, axis=1)

p = ggplot(aes(y='rating', x='week', colour=str('year')), data=df) + \
        geom_point() + geom_smooth(alpha = 0.1)
ggsave(p, 'all_years.png')
