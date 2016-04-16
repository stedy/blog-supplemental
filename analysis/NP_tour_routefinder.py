from itertools import tee, izip
import feather
import googlemaps
import pandas as pd

API = ''

gmaps = googlemaps.Client(API)

def pairwise(iterable):
    "s -> (s0,s1), (s1,s2), (s2, s3), ..."
    a, b = tee(iterable)
    next(b, None)
    return izip(a, b)

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

station_data = []
tsp_route = feather.read_dataframe("TSP_route.feather")

shortest_directions = []
distance = []
for (i1, row1), (i2, row2) in pairwise(tsp_route.iterrows()):
    #item2 = {}
    x = '{},{}'.format(row1['lat'], row1['long'])
    y = '{},{}'.format(row2['lat'], row2['long'])
    directions = gmaps.directions(x, y)
    distance.append(float(directions[0]['legs'][0]['distance']['text'].rstrip(" mi")))
    shortest_directions.append(decode(directions[0]['overview_polyline']['points']))

shortest_directions = [e for l in shortest_directions for e in l]
print "Total distance according to Google maps = {} miles".format(sum(distance))

df = pd.DataFrame(shortest_directions)
feather.write_dataframe(df, "frompython.feather")

