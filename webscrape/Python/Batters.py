from bs4 import BeautifulSoup
from lxml import html
import urllib2

from Tools import *
import Config

## Gets relevant information for given batter with shortName
# Array will contain: {Name, Handedness, hit% vRHP, hit% vLHP, hit% Home, hit% Away}
def GetBatterInfo(shortName):
	batterInfo = []						# Array of (Name, Handedness, HomeVAway performance, RHP/LHP performance) for batter at link

	# Get HTML from website
	r = urllib2.urlopen(Config.url['Base']+"/players/split.cgi?id="+shortName+"&year=Career&t=b")
	soup = BeautifulSoup(r, "lxml")

	### Get relevant info about pitcher ###
	# Get name
	nameLoc = soup.find('span', attrs={'id':'player_name'})
	name = nameLoc.text.strip()

	# Get handedness
	pInfoTable = nameLoc.parent.parent
	metrics = pInfoTable.select('p')[1].text
	
	handedness = metrics.split("Bats: ")[1]
	sep = handedness.split("Throws:")[1]
	handedness = handedness.split(sep,1)[0]
	handedness = handedness.replace(' ','')[:-8]

	# Get split info for against RHP/LHP and Home/Away
	print("\t"+shortName) 
	splitInfo = GetSplitInfo(soup)

	### Append relevant info to array ###
	batterInfo.append(name)
	batterInfo.append(handedness)
	for split in splitInfo:
		batterInfo.append(split)

	r.close()
	return batterInfo;

## Gets split info for given batter
# Will contain hits/@bats for: {vRHP, vLHP, Home, Away}
def GetSplitInfo(soup):

	# RHP and LHP performance: {RHP, LHP}
	handedPerformance = []
	platoonTbls = soup.find_all('table',attrs={'id':'plato'})
	for platoonTbl in platoonTbls:
		while(platoonTbl.tbody == None): time.sleep(1)
		for plat in platoonTbl.tbody.find_all('tr'):
			while(plat == None): time.sleep(1)
			tds = plat.find_all('td')
			if "vs RHP" == tds[1].text:
				ab = tds[5].text
				hits = tds[7].text
				#print(ab + " : " + hits)	
				handedPerformance.insert(0,HitsPerAB(hits,ab))
			elif "vs LHP" == tds[1].text: 
				ab = tds[5].text
				hits = tds[7].text	
				#print(ab + " : " + hits)
				handedPerformance.insert(1,HitsPerAB(hits,ab))

	# Get Home v Away performance: {Home, Away}
	locPerformance = []
	hvaTbl = soup.find('table',attrs={'id':'hmvis'})
	for hva in hvaTbl.tbody.find_all('tr'):
		tds = hva.find_all('td')
		ab = tds[5].text
		hits = tds[7].text	
		locPerformance.append(HitsPerAB(hits,ab))

	splitInfo = []
	splitInfo.append(handedPerformance[0])
	splitInfo.append(handedPerformance[1])
	splitInfo.append(locPerformance[0])
	splitInfo.append(locPerformance[1])

	return splitInfo