from bs4 import BeautifulSoup
from lxml import html
import urllib2
from Pitchers import *
from Batters import *
from Tools import *
from Lineups import *
import Config

## Returns array links for games played on given date
def GetGamesOnDate(month, day, year):
	gameLinks = []								# Array of game links on given date (to be appended to base url later)
	gameInfo = []
	date = year+"-"+month+"-"+day

	# Get HTML from website
	r = urllib2.urlopen(Config.url['Base'] + "/games/standings.cgi?select="+year+"&year="+year+"&month="+month+"&day="+day+"&submit=Find+Games")
	soup = BeautifulSoup(r, "lxml")

	# Get table of games on page
	content = soup.find('div', attrs={'id':'page_content'})
	contentChildren = content.findChildren()
	table = "None"
	for child in contentChildren:
		if child.name == 'pre':
			table = child
			break;
	if table == "None":
		return None

	# Get game preview links from each game (we only want FIRST link, so need iterative method to do so)
	for a in table.find_all('a'):
		if "/boxes/" in a.get('href'):
			gameLinks.append(a.get('href'))

	r.close()
	return gameLinks;


## Main Web Scraping Function Begins ###
Month = Config.startDate['Month']
Day = Config.startDate['Day']
Year = Config.startDate['Year']

# Get array of games played for each day of the year
gameLinks = GetGamesOnDate(Month, Day, Year)
date = Month + Day + Year
InitializeFile(Year,Config.header)

# For each link in game links, get game record
# Each record should contain:
#		Date of game
#		Batter name
#		Pitcher name
# 		Batter v Pitcher hand (*)
#		Batter location		  (*)
#		Pitcher v Batter hand (*)
#		Pitcher location	  (*)
#		At bats
#		Hit
# Where, (*) are all Hits/Plate appearances (either 0 or 1 depending on if hit on date)
records = []
for game in gameLinks:
	gameRecord = GetGameRecords(date, game)
	records.append(gameRecord)
# gameRecord = GetGameRecords(str(date), gameLinks[1])
# records.append(gameRecord)

AddResultsToFile(Year,records)







