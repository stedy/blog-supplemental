import pandas as pd
import googlemaps
import numpy as np

API = ''

gmaps = googlemaps.Client(API)

baseurl = "https://en.wikipedia.org/wiki/List_of_airports_in_the_United_States"
tables = pd.read_html(baseurl)

def decode(point_str):
    '''Decodes a polyline that has been encoded using Google's algorithm
    http://code.google.com/apis/maps/documentation/polylinealgorithm.html
    hat tip to: https://gist.github.com/signed0/2031157'''

    coord_chunks = [[]]

    for char in point_str:
        value = ord(char) - 63
        split_after = not (value & 0x20)
        value &= 0x1f

        coord_chunks[-1].append(value)

        if split_after:
            coord_chunks.append([])

    del coord_chunks[-1]

    coords = []

    for coord_chunk in coord_chunks:
        coord = 0
        for i, chunk in enumerate(coord_chunk):
            coord |= chunk << (i * 5)
        if coord & 0x1:
            coord = ~coord
        coord >>= 1
        coord /= 100000.0

        coords.append(coord)

    points = []
    prev_x = 0
    prev_y = 0

    for i in xrange(0, len(coords) - 1, 2):
        if coords[i] == 0 and coords[i + 1] == 0:
            continue
        prev_x += coords[i + 1]
        prev_y += coords[i]
        points.append((round(prev_x, 6), round(prev_y, 6)))
    return points

us_airports = tables[2]
us_airports.columns = us_airports.iloc[0]
us_airports = us_airports.drop(us_airports.index[0])

us_airports['State'] = [x if x.isupper() else np.nan for x in us_airports['City']]
us_airports['State'].fillna(method='ffill', inplace=True)

us_airports = us_airports[pd.notnull(us_airports['IATA'])]
us_airports['City'].replace(to_replace = ["New York"], value="New York City",inplace=True)

locations = []
for index, row in us_airports.iterrows():
    city_state = '{},{}'.format(row['City'].split("/")[-1], row['State'])
    city_bounds = gmaps.geocode(city_state)[0]['geometry']['location']
    airport_string = '{},{}'.format(row['Airport'].split("/")[0].split("(")[0].encode('utf-8'), row['State'])
    airport_location = gmaps.geocode(airport_string)[0]['geometry']['location']
    airport_lat = airport_location['lat']
    airport_lng = airport_location['lng']
    x = '{},{}'.format(city_bounds['lat'], city_bounds['lng'])
    y = '{},{}'.format(airport_lat, airport_lng)
    locations.append((city_state, x, y, row['Enplanements'], row['IATA']))

#then swap out to directions API

API = ''
gmaps = googlemaps.Client(API)

with open("distances.csv", 'wb') as ad:
    for x in locations:
        print x
        directions = gmaps.directions(x[1], x[2])
        if len(directions) > 0:
            distance_obj = directions[0]['legs'][0]['distance']['text']
        else:
            distance_obj = np.nan

        ad.write('"{}",{},{},{}\n'.format(x[0],distance_obj,x[3].replace(",",""),x[4].encode('utf-8')))
