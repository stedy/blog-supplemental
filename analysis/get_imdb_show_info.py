import argparse
import requests
import sqlite3
import pandas as pd

parser = argparse.ArgumentParser(description="""Command line utility to
        determine which TV show episodes are above the norm for a season as
        rated by IMDb users
        for more informed binge watching""")
parser.add_argument('-key', help="Text file with OMDb API key",
        type=argparse.FileType("r"))

args = parser.parse_args()
api_key = args.key.read().rstrip("\n")

conn = sqlite3.connect('bwtv.db')
with conn:
    cur = conn.cursor()
    needed = list(cur.execute("SELECT * from needed"))
    completed = list(cur.execute("SELECT * from completed"))
    still_needed = set(needed) - set(completed)

    for x in still_needed:
        imdbID = x[0]
        omdb_url = "http://www.omdbapi.com/?i=" + imdbID + "&apikey=" + api_key
        omdb_url_req = requests.get(omdb_url)

        total_seasons = omdb_url_req.json()['totalSeasons']
        if total_seasons == "N/A":
            cur.execute("INSERT INTO failed (iMDBid) VALUES (?)", [imdbID])
            conn.commit()
            continue
        else:
            show_name = omdb_url_req.json()['Title']
            print (show_name, imdbID)

            season = list(range(1, int(total_seasons) + 1))
            summary_list = ["Season", "Episode", "Value", "Name"]
            final_df = pd.DataFrame()

            for xx in season:
                omdb_season_url = "http://www.omdbapi.com/?i=" + imdbID + "&Season=" + str(xx) + "&apikey=" + api_key
                omdb_season_url_req = requests.get(omdb_season_url)
                if omdb_season_url_req.json()['Response'] != 'False':

                    episode = [y['Episode'] for y in omdb_season_url_req.json()['Episodes']]
                    rating = [y['imdbRating'] for y in omdb_season_url_req.json()['Episodes']]
                    title = [y['Title'] for y in omdb_season_url_req.json()['Episodes']]
                    local_season = [xx] * len(omdb_season_url_req.json()['Episodes'])
                    df = pd.DataFrame([local_season, episode, rating, title])

                    df = df.transpose()
                    df.columns = summary_list
                    final_df = final_df.append(df)

            final_df = final_df.assign(iMDB = imdbID)
            final_df = final_df.assign(Title = show_name)

            cur.executemany("INSERT INTO base (Season, Episode, Value, Name, Title,\
                iMDBid) VALUES (?,?,?,?,?,?)", list(final_df[['Season', 'Episode', 'Value',\
                'Name', 'Title', 'iMDB']].to_records(index = False)))
            cur.execute("INSERT INTO completed (iMDBid) VALUES (?)", [imdbID])
            cur.execute("DELETE FROM needed WHERE (iMDBid) = (?)", [imdbID])
            conn.commit()
