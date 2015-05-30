import json
import urllib2
import BeautifulSoup as bs
import re

baseurl = "http://gd2.mlb.com/components/game/mlb/year_2014/"

months = ["month_" + format(y,'02') + "/" for y in range(4,11)]
days = ["day_" + format(x, '02') + "/" for x in range(32)]

for month in months:
    for day in days:
        dailyurl = baseurl + month + day
        try:
            response = urllib2.urlopen(dailyurl)
        except urllib2.HTTPError, err:
            if err.code == 404:
                pass

        soup = bs.BeautifulSoup(response.read())

        gameday = []
        for x in soup.findAll('a'):
            if re.search("_seamlb_1", x['href']):
                gameday.append(x.contents)

        if len(gameday) > 0:
            for game in gameday:
                jsonurl = dailyurl + game[0].lstrip(" ") + "boxscore.json"
                response = urllib2.urlopen(jsonurl)
                data = json.loads(response.read())

                gamedata = data['data']['boxscore']['game_info']

                soup = bs.BeautifulSoup(gamedata)

                attendance = soup.find('attendance').contents[0]
                date = data['data']['boxscore']['date']

                felix = 0
                pitching = data['data']['boxscore']['pitching']
                try:
                    for x in pitching:
                        for y in x['pitcher']:
                            if y["name"] == "Hernandez, F":
                                felix += 1
                    if felix > 0:
                        print "%s, '%s', %s" % (attendance, date, True)
                    else:
                        print "%s, '%s', %s" % (attendance, date, False)
                except TypeError:
                    print "%s, '%s', %s" % (attendance, date, 'NA')
