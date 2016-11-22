from bs4 import BeautifulSoup
from lxml import html
import urllib2

from Tools import *
from Pitchers import *
from Batters import *
import Config
import time


def GetGameRecords(date, link):
	# Get game lineups and break into home/away teams
	homeTeamLineup, awayTeamLineup = GetGameLineups(link)

	# Get info on starting pitchers
	#	{Name, Handedness, hit% vRHB, hit% vLHB, hit% Home, hit% Away}
	homePitcherInfo = GetPitcherInfo(homeTeamLineup[0])
	awayPitcherInfo = GetPitcherInfo(awayTeamLineup[0])

	# Get info on starting batters
	#	{Name, Handedness, hit% vRHP, hit% vLHP, hit% Home, hit% Away, @bats, if hit}
	homeBattersInfo = []
	for batter in homeTeamLineup[1:10]:
		batterInfo = GetBatterInfo(batter[0])
		batterInfo.append(batter[1])				# At Bats for current game
		batterInfo.append(batter[2])				# If player got hit
		homeBattersInfo.append(batterInfo)

	awayBattersInfo = []
	for batter in awayTeamLineup[1:10]:
		batterInfo = GetBatterInfo(batter[0])
		batterInfo.append(batter[1])				# At Bats for current game
		batterInfo.append(batter[2])				# If player got hit
		awayBattersInfo.append(batterInfo)

	## Create records for each head-to-head performance
	individualRecords = []
	for batter in homeBattersInfo:
		record = GetHTHPerformance(batter,awayPitcherInfo,1)
		record.insert(0, date)
		record.append(batter[6])
		record.append(batter[7])
		individualRecords.append(record)

	for batter in awayBattersInfo:
		record = GetHTHPerformance(batter, homePitcherInfo, 0)
		record.insert(0, date)
		record.append(batter[6])
		record.append(batter[7])
		individualRecords.append(record)

	return individualRecords


## Creates individual head-to-head performance record
# Returns an array of: {batter name, pitcher name, 
#						vHandednessOfPitcher,home/away performance of pitcher,
#						vHandednessOfBatter, home/away performance of batter}
# homeOrAway == 0 if batter is away, 1 if batter is home.
def GetHTHPerformance(batter, pitcher, homeOrAway):
	# Get name of batter and pitcher
	batterName = batter[0]
	pitcherName = pitcher[0]

	# Get handedness of batter and pitcher
	batterHand = batter[1]
	pitcherHand = pitcher[1]

	# Get performance of batter v pitcher handedness and vice-versa
	## NOTE: average of both if batter/pitcher is both-handed
	batterVHP = "%.9f" % ((float(ReplaceIfEmpty(pitcher[2])) + float(ReplaceIfEmpty(pitcher[3])))/2)
	if (pitcherHand == "Right"):
		batterVHP = pitcher[2]
	elif (pitcherHand == "Left"):
		batterVHP = pitcher[3]

	pitcherVHB = "%.9f" % ((float(ReplaceIfEmpty(batter[2])) + float(ReplaceIfEmpty(batter[3])))/2)
	if (batterHand == "Right"):
		pitcherVHB = batter[2]
	elif (batterHand == "Left"):
		pitcherVHB = batter[3]

	# Get performance of batter and pitcher at location (Home or away)
	batterAtLoc = batter[4]
	pitcherAtLoc = pitcher[5]
	if (homeOrAway == 0):
		batterAtLoc = batter[5]
		pitcherAtLoc = pitcher[4]

	HTHPerformance = []
	HTHPerformance.append(batterName)
	HTHPerformance.append(pitcherName)
	HTHPerformance.append(batterVHP)
	HTHPerformance.append(batterAtLoc)
	HTHPerformance.append(pitcherVHB)
	HTHPerformance.append(pitcherAtLoc)

	return HTHPerformance


## Get Lineups, for both teams, of given game link.
# Will return an array with home team lineup first, away team second.
def GetGameLineups(link):
	lineups = []

	# Get HTML from website
	r = urllib2.urlopen(Config.url['Base'] + link)
	soup = BeautifulSoup(r, "lxml")

	# Get home and away team names: {Home Team, Away Team}
	linescore = soup.find('pre', attrs={'id':'linescore'})
	teamNames = []
	for a in linescore.find_all('a'):
		name = a.get('href')
		name = GetTeamNameFromLink(name, "/teams/")
		teamNames.append(name)


	# Get starting lineup for home team and pitcher
	homeTeamName = CollapseTeamName(Config.TeamAbbrs[teamNames[0]])
	homeTeamTable = soup.find('table', attrs={'id':homeTeamName+'batting'})
	homeTeamLineup = GetLineupFromTable(homeTeamTable)

	# Get starting lineup for away team and pitcher
	awayTeamName = CollapseTeamName(Config.TeamAbbrs[teamNames[1]])
	awayTeamTable = soup.find('table', attrs={'id':awayTeamName+'batting'})
	awayTeamLineup = GetLineupFromTable(awayTeamTable)

	r.close()
	return homeTeamLineup, awayTeamLineup

## Given soup HTML of table, returns lineup links
# Will begin with home team pitcher, followed by starting batting lineup(in order)
# Batter will contain: {shortName, at bats, and if hit}
def GetLineupFromTable(ele):
	lineup = []

	# For each row in team's table, get link to batter.
	# When separator row found, get next, which is pitcher and prepend to list
	#time.sleep(3)
	#print(type(ele.tbody))
	while(ele is None): time.sleep(1)
	while(ele.tbody == None): time.sleep(1)
	for row in ele.tbody.find_all('tr'):
		if "normal_text" not in row.get('class'):
			name = row.findNext('tr')
			name = name.find('a').get('href')
			lineup.insert(0,GetShortName(name))
			break;
		elif isinstance(row.find_all('td')[0].contents[0], unicode):
			continue;

		tds = row.find_all('td')
		name = tds[0]
		name = name.find('a').get('href')
		ab = tds[1].text
		hit = DidBatterGetHit(tds[3].text)

		batter = []
		batter.append(GetShortName(name))
		batter.append(ReplaceIfEmpty(ab))
		batter.append(hit)

		lineup.append(batter)


	return lineup
	
## Returns 1 if batter got hit, 0 otherwise
def DidBatterGetHit(ele):
	gotHit = int(ReplaceIfEmpty(ele))
	if(gotHit > 0):
		gotHit = 1
	else:
		gotHit = 0
	return str(gotHit)

def GetShortName(link):
	shortName = link.split("/players/",1)[1]
	shortName = shortName.split('/')[1]
	shortName = shortName.split('.')[0]
	return shortName