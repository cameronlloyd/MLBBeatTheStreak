from bs4 import BeautifulSoup
from lxml import html
import urllib2

from Tools import *
import Config

## Gets relevant information for given pitcher with shortName
# Array will contain: {Name, Handedness, hit% vRHB, hit% vLHB, hit% Home, hit% Away}
def GetPitcherInfo(shortName):
	pitcherInfo = []						# Array of (Name, Handedness, HomeVAway performance, RHB/LHB performance) for pitcher at link

	# Get HTML from website
	r = urllib2.urlopen(Config.url['Base']+"/players/split.cgi?id="+shortName+"&year=Career&t=p")
	soup = BeautifulSoup(r, "lxml")

	### Get relevant info about pitcher ###
	# Get name
	nameLoc = soup.find('span', attrs={'id':'player_name'})
	name = nameLoc.text.strip()

	# Get handedness
	pInfoTable = nameLoc.parent.parent
	metrics = pInfoTable.select('p')[1].text
	
	handedness = metrics.split("Throws: ")[1]
	sep = handedness.split("Height:")[1]
	handedness = handedness.split(sep,1)[0]
	handedness = handedness.replace(' ','')[:-8]

	# Get split info for against RHB/LHB and Home/Away 
	print(shortName)
	splitInfo = GetSplitInfo(soup)

	### Append relevant info to array ###
	pitcherInfo.append(name)
	pitcherInfo.append(handedness)
	for split in splitInfo:
		pitcherInfo.append(split)

	r.close()
	return pitcherInfo;

## Gets split info for given pitcher
# Will contain hits/@bats for: {vRHB, vLHB, Home, Away}
def GetSplitInfo(soup):

	# RHB and LHB performance: {RHB, LHB}
	handedPerformance = []
	platoonTbls = soup.find_all('table',attrs={'id':'plato'})
	for platoonTbl in platoonTbls:
		#print(type(platoonTbl.tbody))
		while(platoonTbl.tbody is None): time.sleep(1)
		for plat in platoonTbl.tbody:
			tds = None
			try:
				tds = plat.find_all('td')	# try findAll
			except:
				print("error: " + plat)

			while(tds is None): 
				print(plat)
				tds = plat.findAll('td')	# try findAll
				time.sleep(1)
			
			if "vs RHB" == tds[1].text:
				ab = tds[4].text
				hits = tds[6].text
				#print(ab + " : " + hits)	
				handedPerformance.insert(0,HitsPerAB(hits,ab))
			elif "vs LHB" == tds[1].text: 
				ab = tds[4].text
				hits = tds[6].text	
				#print(ab + " : " + hits)
				handedPerformance.insert(1,HitsPerAB(hits,ab))

	# Get Home v Away performance: {Home, Away}
	locPerformance = []
	hvaTbl = soup.find('table',attrs={'id':'hmvis'}).find('tbody')
	for hva in hvaTbl:
		tds = hva.find_all('td')
		ab = tds[4].text
		hits = tds[6].text	
		locPerformance.append(HitsPerAB(hits,ab))

	splitInfo = []
	splitInfo.append(handedPerformance[0])
	splitInfo.append(handedPerformance[1])
	splitInfo.append(locPerformance[0])
	splitInfo.append(locPerformance[1])

	return splitInfo