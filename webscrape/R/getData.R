library('rvest')
library('XML')



#Scrape all baseball games between startDate and endDate
#the two arguments have the form "year/month/day" [No zero in front of a number below ten is needed!]
scrapeCalender <- function(startDate,endDate) {
  
  dates = c(seq(as.Date(startDate),as.Date(endDate),"days"))
  
  data = data.frame(avgHandBatter = c(), avgPlaceBatter =c(),avgHandPitcher = c(),avgPlacePitcher = c(),hit=c(),atBats=c(),sampleHandBatter=c(),samplePlaceBatter=c(),sampleHandPitcher=c(),samplePlacePitcher=c(),Date=c())
  
  for (i in 1:length(dates)) {
    day = dates[i]
    print(day)
    splitDate = strsplit(as.character(day),"-")
    year = unlist(splitDate)[1]
    month = unlist(splitDate)[2]
    day = unlist(splitDate)[3]
    
    tryCatch({
    dataFromDay = scrapeDay(year,month,day)
    data = rbind(data,dataFromDay)
    saveRDS(data,file=paste(as.character(dates[i])," Data.rda",sep=""))
    }, error = function(e){cat("ERROR:",conditionMessage(e),"\n")})
  }
  
  return (data)
  
  
}



scrapeDay <- function(year,month,day) {
  
  result = data.frame(avgHandBatter = c(), avgPlaceBatter =c(),avgHandPitcher = c(),avgPlacePitcher = c(),hit=c(),atBats=c(),sampleHandBatter = c(),samplePlaceBatter=c(),sampleHandPitcher=c(),samplePlacePitcher=c())
  
  date = paste(year,month,day,sep="-")
  link = paste("http://www.baseball-reference.com/games/standings.cgi?date=",date,sep="")
  
  dayInBaseball = html(link)
  game.links = xpathSApply(dayInBaseball, "//pre/a[starts-with(@href,'/boxes/')]", xmlGetAttr, "href")
  
  for (link in game.links) {
    link = paste("http://www.baseball-reference.com",link,sep="")
    print("Scraping a game now")
    print(paste("The link for the game being scraped now is: ",link,sep=""))
    
    gameFrame = scrapeGame(link)
    result = rbind(result,gameFrame)
    print("Finished scraping game")
  }
  
  dateColumn = rep(date,nrow(result))
  result = data.frame(result,dateColumn)
  names(result) = c("avgHandBatter" ,"avgPlaceBatter","avgHandPitcher","avgPlacePitcher","hit","atBats","sampleHandBatter","samplePlaceBatter","sampleHandPitcher","samplePlacePitcher","Date")
  
  return (result)
  
}



scrapeGame <-function(game.link) {
  
  gameInBaseball = html(game.link)
  startingLineups = xpathSApply(gameInBaseball,"//div[@id='div_lineups']//a[starts-with(@href,'/players/')]",xmlGetAttr,"href") #An array of the links of all players we are interested in for this game
  
  awayBatters=NULL
  homeBatters=NULL
  awayPitcher=NULL
  homePitcher=NULL
  
  if (length(startingLineups)==20) {
    #Then this is an American league game, and the away and home pitcher will always be the last row in the table; There will also always be 20 links in the table
    awayBatters = startingLineups[seq(1,length(startingLineups)-2,2)] #links for awayBatters
    homeBatters = startingLineups[seq(2,length(startingLineups)-2,2)] #links for homeBatters
    awayPitcher = startingLineups[length(startingLineups)-1] #link for awayPitcher
    homePitcher = startingLineups[length(startingLineups)] #link for homePitcher
  }
  else {
    awayBatters = startingLineups[seq(1,length(startingLineups),2)] #links for awayBatters
    homeBatters = startingLineups[seq(2,length(startingLineups),2)] #links for homeBatters 
    pitchers = xpathSApply(gameInBaseball,"//table[contains(@id,'pitching')]//td[@csk='0']//a[starts-with(@href,'/players/')]",xmlGetAttr,"href") #Go through the pitching tables to grab the pitchers
    #Note the assumption that the starting pitcher is the first row in this table (which I believe is a safe assumption)
    awayPitcher = pitchers[1] #The first pitching table is always the away pitcher.
    homePitcher = pitchers[2]
    
  }
  result = data.frame(avgHandBatter = c(), avgPlaceBatter =c(),avgHandPitcher = c(),avgPlacePitcher = c(),hit=c(),sampleHandBatter=c(),samplePlaceBatter=c(),sampleHandPitcher=c(),samplePlacePitcher=c())
  
  print("Scraping the pitchers now")
  awayPitcherData = scrapePitcher(awayPitcher,FALSE) #[handedness,againstRightiesAVG,againstLeftiesAVG,awayAVG]
  homePitcherData = scrapePitcher(homePitcher,TRUE) #[handedness,againstRightiesAVG,againstLeftiesAVG,homeAVG]
  print("Done scraping the pitchers")
  
  #Create a table with column 1=player link, column 2=Did player get hit in the game [This table includes all who batted in the game]
  #THIS IS HOW WE GET INFORMATION ABOUT THE GAME!
  battingPlayers = xpathSApply(gameInBaseball,"//table[contains(@id,'batting')]//tbody//a[starts-with(@href,'/players/')]",xmlGetAttr,"href")
  battingHits = gameInBaseball %>% html_nodes(xpath="//table[contains(@id,'batting')]//tbody//tr[starts-with(@class,'normal')]//td[4]") %>% html_text()
  atBats = gameInBaseball %>% html_nodes(xpath="//table[contains(@id,'batting')]//tbody//tr[starts-with(@class,'normal')]//td[2]") %>% html_text()
  hitTable = data.frame(player.link = battingPlayers, hit = as.numeric(battingHits)>0,atBats = as.numeric(atBats))
  
  
  
  
  print("scraping the away batters now")
  for (batter in awayBatters) {
    result = rbind(result,createRow(batter,FALSE,homePitcherData,hitTable))
  }
  print("scraping the home batters now")
  for (batter in homeBatters) {
    result = rbind(result,createRow(batter,TRUE,awayPitcherData,hitTable))
  }
  
  return (result)
}



  
#Return: Performance against lefties, performance against righties, handedness, performance where they are playing (either home or away)
#home is a boolean: True if pitcher is playing this game at home; false otherwise
#a tuple is returned - > [handedness,againstRightiesAVG,againstLeftiesAVG,placeAVG,sampleRight,sampleLeft,samplePlace]
scrapePitcher <-function(pitcher.link,home) {
  print(pitcher.link)

  pitcherID = extractID(pitcher.link)
  
  pitcherSplitLink = paste("http://www.baseball-reference.com/players/split.cgi?id=",pitcherID,"&year=Career&t=p",sep="") #This grabs the Career statistics. WE WILL PROBABLY NOT WANT THIS
  
  pitcherHTML = html(pitcherSplitLink)
  
  handedness = extractHand(pitcherHTML,TRUE)
  
  #platoon splits: This might not be most efficient -- but I'm doing it because the other way (to grab the table directly, may actually be less efficient)
  hitsRight = pitcherHTML %>% html_nodes(xpath='//table[@id="plato"]/tbody/tr[1]/td[7]') %>% html_text() #first row is assumed to be against righties
  appsRight = pitcherHTML %>% html_nodes(xpath='//table[@id="plato"]/tbody/tr[1]/td[4]') %>% html_text()
  hitsLeft = pitcherHTML %>% html_nodes(xpath='//table[@id="plato"]/tbody/tr[2]/td[7]') %>% html_text() #second row is assumed to be against lefties
  appsLeft = pitcherHTML %>% html_nodes(xpath='//table[@id="plato"]/tbody/tr[2]/td[4]') %>% html_text()
  
  againstRightiesAVG = as.numeric(hitsRight)/as.numeric(appsRight)
  againstLeftiesAVG = as.numeric(hitsLeft)/as.numeric(appsLeft)
  
  
  hitsPlace = NULL
  appsPlace = NULL
  #Home/away split
  if (home) {
    #first row is assumed to be at home
    hitsPlace = pitcherHTML %>% html_nodes(xpath='//table[@id="hmvis"]/tbody/tr[1]/td[7]') %>% html_text() 
    appsPlace = pitcherHTML %>% html_nodes(xpath='//table[@id="hmvis"]/tbody/tr[1]/td[4]') %>% html_text()
    
  }
  else
  {
    #second row is assumed to be away
    hitsPlace = pitcherHTML %>% html_nodes(xpath='//table[@id="hmvis"]/tbody/tr[2]/td[7]') %>% html_text()
    appsPlace = pitcherHTML %>% html_nodes(xpath='//table[@id="hmvis"]/tbody/tr[2]/td[4]') %>% html_text()
  }
  avgPlace = as.numeric(hitsPlace)/as.numeric(appsPlace)
  
  pitcherInfo = c(handedness,againstRightiesAVG,againstLeftiesAVG,avgPlace,appsRight,appsLeft,appsPlace)
  
  return(pitcherInfo)
}




#This function inputs a batters link, the vital data for the pitcher they are facing, and the place of the batter (second argument)
#This function returns a row to be inserted into the output dataframe [BatterAvgAgainstPitcherHand,BatterAvgInPlace,PitcherAvgAgainstBatterHand,PitcherAvgInPlace,sampleBatterHand,sampleBatterPlace,samplePitcherHand,samplePitcherPlace]
#home is a boolean: True if batter is playing this game at home; false otherwise
#opp.pitcherData: The data [handedness,againstRightiesAVG,againstLeftiesAVG,placeAVG,sampleRight,sampleLeft,samplePlace] collected for the opposing pitcher
#Note that this function is both scraping the batters data, and creating the row at the same time
#HitTable comes from the batting tables in the boxscore. It contains one column for player.link, and the other column is boolean: did they get a hit or not
createRow <-function(batter.link,home,opp.PitcherData,hitTable) {
  print(batter.link)
  #Determine whether the batter got a hit in the game
  hit = hitTable[which(hitTable$player.link==batter.link),2]
  atBats = hitTable[which(hitTable$player.link==batter.link),3]
  
  
  
  #print(batter.link)
  batterID = extractID(batter.link)
  batterSplitLink = paste("http://www.baseball-reference.com/players/split.cgi?id=",batterID,"&year=Career&t=b",sep="") #This grabs the Career statistics. WE WILL PROBABLY NOT WANT THIS
  print(batterSplitLink)
  batterHTML = html(batterSplitLink)
  batterHand = extractHand(batterHTML,FALSE)
  
  #platoon splits: This might not be most efficient -- but I'm doing it because the other way (to grab the table directly, may actually be less efficient)
  pitcherHand = opp.PitcherData[1]
  
  #1: Get the average of the batter against the handedness of the starting pitcher
  avgHandBatter = NULL #**** FINAL ROW DATA
  sampleHandBatter = NULL
  if (pitcherHand=="RIGHT") {
    hitsRight = batterHTML %>% html_nodes(xpath='//table[@id="plato"]/tbody/tr[1]/td[8]') %>% html_text() #first row is assumed to be against righties
    appsRight = batterHTML %>% html_nodes(xpath='//table[@id="plato"]/tbody/tr[1]/td[5]') %>% html_text()
    sampleHandBatter = as.numeric(appsRight)
    avgHandBatter = as.numeric(hitsRight)/as.numeric(appsRight)
    
  }
  else {
    hitsLeft = batterHTML %>% html_nodes(xpath='//table[@id="plato"]/tbody/tr[2]/td[8]') %>% html_text() #second row is assumed to be against lefties
    appsLeft = batterHTML %>% html_nodes(xpath='//table[@id="plato"]/tbody/tr[2]/td[5]') %>% html_text()
    sampleHandBatter = as.numeric(appsLeft)
    avgHandBatter = as.numeric(hitsLeft)/as.numeric(appsLeft)
  }
  
  
  #2: Get the average of the starting pitcher against the handedness of the batter
  avgHandPitcher = NULL #**** FINAL ROW DATA
  sampleHandPitcher = NULL
  if (batterHand=="RIGHT") {
    avgHandPitcher = opp.PitcherData[2]
    sampleHandPitcher = opp.PitcherData[5]
  }
  else if (batterHand=="LEFT") {
    avgHandPitcher = opp.PitcherData[3]
    sampleHandPitcher = opp.PitcherData[6]
  }
  else {
    #batter is a switch hitter
    if (pitcherHand=="LEFT") {
      avgHandPitcher = opp.PitcherData[2]
      sampleHandPitcher = opp.PitcherData[5]
    }
    else {
      avgHandPitcher = opp.PitcherData[3]
      sampleHandPitcher =  opp.PitcherData[6]
    }
    
  }
  
  
  #3 & 4: Get the average of the batter in the place, and the average of the pitcher in the place
  avgPlaceBatter=NULL #**** FINAL ROW DATA
  avgPlacePitcher = opp.PitcherData[4] #**** Final ROW DATA
  hitsPlace = NULL
  appsPlace = NULL
  #Home/away split
  if (home) {
    #first row is assumed to be at home
    hitsPlace = batterHTML %>% html_nodes(xpath='//table[@id="hmvis"]/tbody/tr[1]/td[8]') %>% html_text() 
    appsPlace = batterHTML %>% html_nodes(xpath='//table[@id="hmvis"]/tbody/tr[1]/td[5]') %>% html_text()
  }
  else
  {
    #second row is assumed to be away
    hitsPlace = batterHTML %>% html_nodes(xpath='//table[@id="hmvis"]/tbody/tr[2]/td[8]') %>% html_text()
    appsPlace = batterHTML %>% html_nodes(xpath='//table[@id="hmvis"]/tbody/tr[2]/td[5]') %>% html_text()
  }
  avgPlaceBatter = as.numeric(hitsPlace)/as.numeric(appsPlace)
  
  samplePlaceBatter = appsPlace
  samplePlacePitcher = opp.PitcherData[7]

  if (length(avgPlaceBatter)==0) {avgPlaceBatter=NA}
  if (length(avgPlacePitcher)==0) {avgPlacePitcher=NA}
  if (length(avgHandBatter)==0) {avgHandBatter=NA}
  if (length(avgHandPitcher)==0) {avgHandPitcher=NA}
  if (length(hit)==0) {hit=NA}
  if (length(atBats)==0) {atBats=NA}
  if (length(sampleHandBatter)==0) {sampleHandBatter=NA}
  if (length(samplePlaceBatter)==0) {samplePlaceBatter=NA}
  if (length(sampleHandPitcher)==0) {sampleHandPitcher=NA}
  if (length(samplePlacePitcher)==0) {samplePlacePitcher=NA}
  result = data.frame(avgHandBatter = c(avgHandBatter),avgPlaceBatter = c(avgPlaceBatter),avgHandPitcher = c(avgHandPitcher),avgPlacePitcher = c(avgPlacePitcher), hit = c(hit),atBats = c(atBats),sampleHandBatter = c(sampleHandBatter),
                      samplePlaceBatter=c(samplePlaceBatter),sampleHandPitcher = c(sampleHandPitcher), samplePlacePitcher = c(samplePlacePitcher))
  
  print(result)
  return (result)
  
}

#Extract the player id from the player link. There is probably a better way to do this using regular expressions. This assumes there is always 11 characters
#before the ID, and 6 characters after the ID.
extractID <-function(player.link) {
  
  lastSlashIndex = 12 #12 is the last slash index in the link
  lastPeriodIndex = nchar(player.link) - 6 #-6 accounts for the .shtml text at the end of the link
  
  splitLink = strsplit(player.link,"")
  id = splitLink[[1]][lastSlashIndex:lastPeriodIndex]
  
  return (paste(id,collapse=""))
  
}


extractHand <-function(player.HTML,is.pitcher) {
  #print(player.HTML)
  
  #if (!is.pitcher) {
  #  print(player.HTML)
  #}
  textVectors = player.HTML %>% html_nodes('p') %>% html_text()

  #print(textVectors)
  
  for (text in textVectors){
    if (grepl("Position",text)){
      text = strsplit(text,":") #split this string by the colons
      handString=NULL
      if (is.pitcher) {
        handString= text[[1]][4] #the pitcher handedness is assumed to be stored in the fourth character vector after the colon split
      }
      else {
        handString = text[[1]][3] #the batter handedness is assumed to be stored in the third character vector after the colon split
      }
      
      return (ifelse(grepl('Right',handString),'RIGHT',ifelse(grepl('Left',handString),'LEFT','BOTH')))
      
    }
  }
}


startDate = "2016/4/3"
endDate = "2016/10/2"
x = scrapeCalender(startDate,endDate)

###cleaning up the data (a bit) -- I should have done this in the script somewhere, but whatever
x$avgHandPitcher = as.numeric(as.character(x$avgHandPitcher))
x$avgPlacePitcher = as.numeric(as.character(x$avgPlacePitcher))
x$hit = as.factor(x$hit)
x$samplePlaceBatter = as.numeric(as.character(x$samplePlaceBatter))
x$sampleHandPitcher = as.numeric(as.character(x$sampleHandPitcher))
x$samplePlacePitcher = as.numeric(as.character(x$samplePlacePitcher))

write.csv(x,file="baseballData.csv")



