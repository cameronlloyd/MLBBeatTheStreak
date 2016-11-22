import Config

def InitializeFile(name, header):
	out = open('./Results/HeadToHead/'+name+'.csv', 'w+')
	for col in header:
		out.write(col+',')
	out.write('\n')

def AddResultsToFile(name, records):
	with open('./Results/HeadToHead/'+name+'.csv', "a") as out: 
		for record in records:
			for stat in record:
				for col in stat:
					#print(col)
					out.write(col.encode('utf-8')+',')
				out.write('\n')

def GetTeamNameFromLink(link, preNameSpace):
	name = link.split(preNameSpace,1)[1]
	name = name.split('/')[0]	
	return name


def CollapseTeamName(name):
	name = name.replace(" ","")
	name = name.replace(".","")
	return name

def ReplaceIfEmpty(str):
	if str == '':
		str = "0"
	elif str == ' ':
		str = "0"
	return str

def HitsPerAB(hits,ab):
	hits = float(ReplaceIfEmpty(hits))
	ab = float(ReplaceIfEmpty(ab))

	result = 0
	if ab!=0:
		result = hits/ab
	result = "%.9f" % result

	return result