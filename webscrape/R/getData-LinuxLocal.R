library('rvest')
library('XML')

config <- config::get()

#Scrape all baseball games between startDate and endDate
#the two arguments have the form "year/month/day" [No zero in front of a number below ten is needed!]
scrapeCalender <- function(startDate,endDate) {
  
  dates = c(seq(as.Date(startDate),as.Date(endDate),"days"))
  
  data = data.frame(avgHandBatter = c(), avgPlaceBatter =c(),avgHandPitcher = c(),avgPlacePitcher = c(),hit=c(),atBats=c(),sampleHandBatter=c(),samplePlaceBatter=c(),sampleHandPitcher=c(),samplePlacePitcher=c(),Date=c())
  lastYear = 0
  for (i in 1:length(dates)) {
    day = dates[i]
    print(day)
    splitDate = strsplit(as.character(day),"-")
    year = unlist(splitDate)[1]
    if (lastYear != year){
      # Load park factors table for current year when year changes.
      # Loads for each year rather than each batter/pitcher to improve efficiency
      dir = paste("./ParkFactors/",toString(year),".csv",sep="")
      ParkFactors <<-read.table(dir)
      lastYear = year
    }
    month = unlist(splitDate)[2]
    day = unlist(splitDate)[3]
    
    tryCatch({
      dataFromDay = scrapeDay(year,month,day)
      data = rbind(data,dataFromDay)
      saveRDS(data,file=paste(as.character(dates[i])," Data.rda",sep=""))
    }, error = function(e){
      cat("ERROR:",conditionMessage(e),"\n")
      })
  }

  data = cleanData(data)
  
  return (data)
  
  
}



scrapeDay <- function(year,month,day) {
  
  result = data.frame(avgHandBatterCar = c(), 
                      avgPlaceBatterCar =c(),
                      avgHandPitcherCar = c(),
                      avgPlacePitcherCar = c(),
                      sampleHandBatterCar = c(),
                      samplePlaceBatterCar=c(),
                      sampleHandPitcherCar=c(),
                      samplePlacePitcherCar=c(),
                      avgHandBatter2015 = c(), 
                      avgPlaceBatter2015 =c(),
                      avgHandPitcher2015 = c(),
                      avgPlacePitcher2015 = c(),
                      sampleHandBatter2015 = c(),
                      samplePlaceBatter2015=c(),
                      sampleHandPitcher2015=c(),
                      samplePlacePitcher2015=c(),
                      avgHandBatter2014 = c(), 
                      avgPlaceBatter2014 =c(),
                      avgHandPitcher2014 = c(),
                      avgPlacePitcher2014 = c(),
                      sampleHandBatter2014 = c(),
                      samplePlaceBatter2014=c(),
                      sampleHandPitcher2014=c(),
                      samplePlacePitcher2014=c(),
                      hit=c(),
                      atBats=c(),
                      pitcherInMonth=c(),
                      batterInMonth=c(),
                      parkFactor=c())
  
  date = paste(year,month,day,sep="-")
  link = paste("http://www.baseball-reference.com/games/standings.cgi?date=",date,sep="")
  
  dayInBaseball = read_html(link) #These two lines are not protected
  game.links = html_nodes(dayInBaseball, xpath="//pre/a[starts-with(@href,'/boxes/')]") %>% xml_attr("href")
  
  
  for (link in game.links) {
    link = paste("http://www.baseball-reference.com",link,sep="")
    print("Scraping a game now")
    print(paste("The link for the game being scraped now is: ",link,sep=""))
    
    
    gameFrame = scrapeGame(link,as.numeric(month))
    result = rbind(result,gameFrame)
    
    print("Finished scraping game")
  }
  
  dateColumn = rep(date,nrow(result))
  result = data.frame(result,dateColumn)
  #names(result) = c("avgHandBatter" ,"avgPlaceBatter","avgHandPitcher","avgPlacePitcher","hit","atBats","sampleHandBatter","samplePlaceBatter","sampleHandPitcher","samplePlacePitcher","Date")
  
  return (result)
  
}


#Month is numeric. It is the month for this game
scrapeGame <-function(game.link,month) {
  
  # gameInBaseball = html(game.link)
  gameInBaseball = read_html(game.link)
  
  # Get home team -> park factor
  # Assuming link always contains home team abbr, which seems to always be the case
  homeTeam = unlist(strsplit(game.link,"/"))
  homeTeam = homeTeam[5]
  parkFactor = getParkFactor(homeTeam)
  
  startingLineups = html_nodes(gameInBaseball,xpath="//div[@id='div_lineups']//a[starts-with(@href,'/players/')]") %>% xml_attr("href")#An array of the links of all players we are interested in for this game
  
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
    pitchers = html_nodes(gameInBaseball,xpath="//table[contains(@id,'pitching')]//td[@csk='0']//a[starts-with(@href,'/players/')]") %>% xml_attr("href") #Go through the pitching tables to grab the pitchers
    
    #Note the assumption that the starting pitcher is the first row in this table (which I believe is a safe assumption)
    awayPitcher = pitchers[1] #The first pitching table is always the away pitcher.
    homePitcher = pitchers[2]
    
  }
  result = data.frame(avgHandBatter = c(), avgPlaceBatter =c(),avgHandPitcher = c(),avgPlacePitcher = c(),hit=c(),sampleHandBatter=c(),samplePlaceBatter=c(),sampleHandPitcher=c(),samplePlacePitcher=c())
  
  print("Scraping the pitchers now")
  #awayPitcherData = scrapePitcher(awayPitcher,FALSE) #[handedness,againstRightiesAVG,againstLeftiesAVG,awayAVG]
  awayPitcherDataCareer = grabPitcherSplitInfo(awayPitcher,'Career',FALSE,month)
  awayPitcherData2015 = grabPitcherSplitInfo(awayPitcher,'2015',FALSE,month)
  awayPitcherData2014 = grabPitcherSplitInfo(awayPitcher,'2014',FALSE,month)
  #awayPitcherDataForMonth = scrapeMonth(awayPitcher,month,TRUE)
  awayPitcherData = c(list(awayPitcherDataCareer),list(awayPitcherData2015),list(awayPitcherData2014))
  homePitcherDataCareer = grabPitcherSplitInfo(homePitcher,'Career',TRUE,month)
  homePitcherData2015 = grabPitcherSplitInfo(homePitcher,'2015',TRUE,month)
  homePitcherData2014 = grabPitcherSplitInfo(homePitcher,'2014',TRUE,month)
  #homePitcherDataForMonth = scrapeMonth(homePitcher,month,TRUE)
  homePitcherData = c(list(homePitcherDataCareer),list(homePitcherData2015),list(homePitcherData2014))
  print("Done scraping the pitchers")
  
  #Create a table with column 1=player link, column 2=Did player get hit in the game [This table includes all who batted in the game]
  #THIS IS HOW WE GET INFORMATION ABOUT THE GAME!
  battingPlayers = html_nodes(gameInBaseball,xpath="//table[contains(@id,'batting')]//tbody//a[starts-with(@href,'/players/')]") %>% xml_attr("href")
  
  battingHits = gameInBaseball %>% html_nodes(xpath="//table[contains(@id,'batting')]//tbody//tr[starts-with(@class,'normal')]//td[4]") %>% html_text()
  atBats = gameInBaseball %>% html_nodes(xpath="//table[contains(@id,'batting')]//tbody//tr[starts-with(@class,'normal')]//td[2]") %>% html_text()
  hitTable = data.frame(player.link = battingPlayers, hit = as.numeric(battingHits)>0,atBats = as.numeric(atBats))
  
  
  
  
  print("scraping the away batters now")
  for (batter in awayBatters) {
    result = rbind(result,createRow(batter,FALSE,homePitcherData,hitTable,month,parkFactor))
  }
  print("scraping the home batters now")
  for (batter in homeBatters) {
    result = rbind(result,createRow(batter,TRUE,awayPitcherData,hitTable,month,parkFactor))
  }
  
  closeAllConnections()
  return (result)
}




#Return: Performance against lefties, performance against righties, handedness, performance where they are playing (either home or away)
#home is a boolean: True if batter is playing this game at home; false otherwise
#pitcherData : A vector of four things [PitcherDataCareer,PitcherData2015,PitcherData2014,PitcherDataMonth] (The first three elements are lists.The last element is a number)
#month: The month of the game being scraped. This is used to pull the players career average within a given month
#parkFactor:  Numeric value of the 'park factor' for the park this current game is being played in
#Each element of pitcherData is a list: [Handedness,againstRightiesAVG,againstLeftiesAVG,avgPlace,appsRight,appsLeft,appsPlace]
#This function returns a row for a batter-pitcher pair in a game. This row has data for the career for the players, for 2015, and for 2014
createRow <-function(batter.link,home,pitcherData,hitTable,month,parkFactor) {
  
  
  Career.Dat = pitcherBatterYear(batter.link,home,pitcherData[[1]],'Career',month)
  Career.Dat.Standard = Career.Dat[,head(1:ncol(Career.Dat),n=-2)] #only take columns that are available for each year
  month.Career.Dat = Career.Dat[,tail(1:ncol(Career.Dat),n=2)] #grab the 'Month' columns, which are only for the career
  Fifteen.Dat = pitcherBatterYear(batter.link,home,pitcherData[[2]],'2015',month)
  Fourteen.Dat = pitcherBatterYear(batter.link,home,pitcherData[[3]],'2014',month)
  
  
  #Determine whether the batter got a hit in the game
  hit = hitTable[which(hitTable$player.link==batter.link),2]
  atBats = hitTable[which(hitTable$player.link==batter.link),3]
  if (length(hit)==0) {hit=NA}
  if (length(atBats)==0) {atBats=NA}
  fromGame.Dat = data.frame(hit =c(hit),atBats=c(atBats))
  otherInfo.Dat = data.frame(pitcherInMonth = c(pitcherInMonth = c(month.Career.Dat[1]),batterInMonth= c(month.Career.Dat[2])), parkFactor = c(parkFactor))
  
  #Put it all together and return it
  result = cbind(Career.Dat.Standard,Fifteen.Dat,Fourteen.Dat,fromGame.Dat,otherInfo.Dat)
  
  return (result)
  
}


#Returns a 7 tuple for a given pitcher in a given year
#[Handedness,againstRightiesAVG,againstLeftiesAVG,avgPlace,appsRight,appsLeft,appsPlace]
#home: True if pitcher is at home, false otherwise
#Year: One of 'Career', '2016', '2015', etc. Year determines which link will be scraped
#month: The month of the game being scraped. This is used to pull the players career average within a given month
grabPitcherSplitInfo <- function(pitcher.link,year,home,month) {
  
  
  pitcherID = extractID(pitcher.link)
  pitcherSplitLink = paste("http://www.baseball-reference.com/players/split.cgi?id=",pitcherID,"&year=",year,"&t=p",sep="") #This grabs the Career statistics. WE WILL PROBABLY NOT WANT THIS
  
  pitcherHTML = read_html(pitcherSplitLink)
  
  handedness = extractHand(pitcherHTML,TRUE)
  
  
  
  hitIndex=NULL
  appsIndex=NULL
  #Career links have extra column at the beginning
  if (year=='Career') {
    hitIndex='7'
    appsIndex='4'
  }
  else {
    hitIndex='6'
    appsIndex='3'
  }
  
  
  
  #platoon splits: This might not be most efficient -- but I'm doing it because the other way (to grab the table directly, may actually be less efficient)
  hitsRight = pitcherHTML %>% html_nodes(xpath=paste('//table[@id=\"plato\"]/tbody/tr[1]/td[',hitIndex,']',sep="")) %>% html_text() #first row is assumed to be against righties
  appsRight = pitcherHTML %>% html_nodes(xpath=paste('//table[@id=\"plato\"]/tbody/tr[1]/td[',appsIndex,']',sep="")) %>% html_text()
  hitsLeft = pitcherHTML %>% html_nodes(xpath=paste('//table[@id=\"plato\"]/tbody/tr[2]/td[',hitIndex,']',sep="")) %>% html_text() #second row is assumed to be against lefties
  appsLeft = pitcherHTML %>% html_nodes(xpath=paste('//table[@id=\"plato\"]/tbody/tr[2]/td[',appsIndex,']',sep="")) %>% html_text()
  
  againstRightiesAVG = as.numeric(hitsRight)/as.numeric(appsRight)
  againstLeftiesAVG = as.numeric(hitsLeft)/as.numeric(appsLeft)
  
  
  hitsPlace = NULL
  appsPlace = NULL
  #Home/away split
  if (home) {
    #first row is assumed to be at home
    hitsPlace = pitcherHTML %>% html_nodes(xpath=paste('//table[@id=\"hmvis\"]/tbody/tr[1]/td[',hitIndex,']',sep="")) %>% html_text() 
    appsPlace = pitcherHTML %>% html_nodes(xpath=paste('//table[@id=\"hmvis\"]/tbody/tr[1]/td[',appsIndex,']',sep="")) %>% html_text()
    
  }
  else
  {
    #second row is assumed to be away
    hitsPlace = pitcherHTML %>% html_nodes(xpath=paste('//table[@id=\"hmvis\"]/tbody/tr[2]/td[',hitIndex,']',sep="")) %>% html_text()
    appsPlace = pitcherHTML %>% html_nodes(xpath=paste('//table[@id=\"hmvis\"]/tbody/tr[2]/td[',appsIndex,']',sep="")) %>% html_text()
  }
  avgPlace = as.numeric(hitsPlace)/as.numeric(appsPlace)
  
  pitcherInfo = c(handedness,againstRightiesAVG,againstLeftiesAVG,avgPlace,appsRight,appsLeft,appsPlace)
  
  if (year!='Career'){
    return(pitcherInfo)
  }
  else {
    pitcherInMonth = scrapeMonth(pitcherHTML,month,TRUE)
    pitcherInfo = c(pitcherInfo,pitcherInMonth)
    return(pitcherInfo)
  }
  
}







#Given a batter (specifed through the batter link, and pitcher data for a given year), extract [BatterAvgAgainstPitcherHand,BatterAvgInPlace,PitcherAvgAgainstBatterHand,PitcherAvgInPlace,sampleBatterHand,sampleBatterPlace,samplePitcherHand,samplePitcherPlace]
#for that year
#home: True if the batter is at home. False otherwise
#Returns a one row data frame with information discussed a couple lines above
#year: One of 'Career', '2016', '2015', etc. Year determines which link will be scraped
#month: The month of the game being scraped. This is used to pull the players career average within a given month
pitcherBatterYear <-function(batter.link,home,opp.PitcherData,year,month) {
  # print(batter.link)
  
  
  #print(batter.link)
  batterID = extractID(batter.link)
  batterSplitLink = paste("http://www.baseball-reference.com/players/split.cgi?id=",batterID,"&year=",year,"&t=b",sep="") #This grabs the Career statistics. WE WILL PROBABLY NOT WANT THIS
  # print(batterSplitLink)
  batterHTML = read_html(batterSplitLink)
  
  batterHand = extractHand(batterHTML,FALSE)
  
  #platoon splits: This might not be most efficient -- but I'm doing it because the other way (to grab the table directly, may actually be less efficient)
  pitcherHand = opp.PitcherData[1]
  
  #1: Get the average of the batter against the handedness of the starting pitcher
  avgHandBatter = NULL #**** FINAL ROW DATA
  sampleHandBatter = NULL
  hitIndex=NULL
  appsIndex=NULL
  #There is an extra column at the beginning for the career
  if (year=='Career') {
    hitIndex='8'
    appsIndex='5'
  }
  else {
    hitIndex='7'
    appsIndex='4'
  }
  if (pitcherHand=="RIGHT") {
    hitsRight = batterHTML %>% html_nodes(xpath=paste('//table[@id=\"plato\"]/tbody/tr[1]/td[',hitIndex,']',sep="")) %>% html_text() #first row is assumed to be against righties
    appsRight = batterHTML %>% html_nodes(xpath=paste('//table[@id=\"plato\"]/tbody/tr[1]/td[',appsIndex,']',sep="")) %>% html_text()
    sampleHandBatter = as.numeric(appsRight)
    avgHandBatter = as.numeric(hitsRight)/as.numeric(appsRight)
    
  }
  else {
    hitsLeft = batterHTML %>% html_nodes(xpath=paste('//table[@id=\"plato\"]/tbody/tr[2]/td[',hitIndex,']',sep="")) %>% html_text() #second row is assumed to be against lefties
    appsLeft = batterHTML %>% html_nodes(xpath=paste('//table[@id=\"plato\"]/tbody/tr[2]/td[',appsIndex,']',sep="")) %>% html_text()
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
    hitsPlace = batterHTML %>% html_nodes(xpath=paste('//table[@id=\"hmvis\"]/tbody/tr[1]/td[',hitIndex,']',sep="")) %>% html_text() 
    appsPlace = batterHTML %>% html_nodes(xpath=paste('//table[@id=\"hmvis\"]/tbody/tr[1]/td[',appsIndex,']',sep="")) %>% html_text()
  }
  else
  {
    #second row is assumed to be away
    hitsPlace = batterHTML %>% html_nodes(xpath=paste('//table[@id=\"hmvis\"]/tbody/tr[2]/td[',hitIndex,']',sep="")) %>% html_text()
    appsPlace = batterHTML %>% html_nodes(xpath=paste('//table[@id=\"hmvis\"]/tbody/tr[2]/td[',appsIndex,']',sep="")) %>% html_text()
  }
  avgPlaceBatter = as.numeric(hitsPlace)/as.numeric(appsPlace)
  
  samplePlaceBatter = appsPlace
  samplePlacePitcher = opp.PitcherData[7]
  
  
  if (length(avgPlaceBatter)==0) {avgPlaceBatter=NA}
  if (length(avgPlacePitcher)==0) {avgPlacePitcher=NA}
  if (length(avgHandBatter)==0) {avgHandBatter=NA}
  if (length(avgHandPitcher)==0) {avgHandPitcher=NA}
  if (length(sampleHandBatter)==0) {sampleHandBatter=NA}
  if (length(samplePlaceBatter)==0) {samplePlaceBatter=NA}
  if (length(sampleHandPitcher)==0) {sampleHandPitcher=NA}
  if (length(samplePlacePitcher)==0) {samplePlacePitcher=NA}
  
  
  
  #Handle monthly info
  if (year!='Career') {
    result = data.frame(c(avgHandBatter),c(avgPlaceBatter),c(avgHandPitcher),c(avgPlacePitcher),c(sampleHandBatter),c(samplePlaceBatter),c(sampleHandPitcher),c(samplePlacePitcher))
    return (result)
  }
  else {
    pitcherInMonth = opp.PitcherData[8]
    batterInMonth = scrapeMonth(batterHTML,month,FALSE)
    if (length(pitcherInMonth)==0) {pitcherInMonth=NA}
    if (length(batterInMonth)==0) {batterInMonth=NA}
    result = data.frame(c(avgHandBatter),c(avgPlaceBatter),c(avgHandPitcher),c(avgPlacePitcher),c(sampleHandBatter),c(samplePlaceBatter),c(sampleHandPitcher),c(samplePlacePitcher),c(pitcherInMonth),c(batterInMonth))
    return (result)
  }
  
}


# This returns the batting average for the given player during the currentmonth.
# If the player is a pitcher, pitcher==TRUE.  Otherwise, pitcher==FALSE
scrapeMonth <- function(player.page,month,pitcher) {
  monthRow = player.page %>% html_nodes(xpath='//table[@id="month"]/tbody');
  
  # Get the string name of the month  
  if (month == 3 || month == 4){
    month = "April/March"
  }
  else if(month == 5){
    month = "May"
  }
  else if(month == 6){
    month = "June"
  }
  else if(month == 7){
    month = "July"
  }
  else if(month == 8){
    month = "August"
  }
  else if(month == 9 || month == 10){
    month = "Sept/Oct"
  }
  else {
    # This shouldn't occur..
    month = NA
  }
  
  # Get the HTML row of month
  rows = monthRow %>% html_nodes(xpath='./tr')
  if (!is.na(monthRow)){
    for (row in rows){
      curr = row %>% html_nodes(xpath='./td[2]') %>% html_text()
      if (grepl(curr, month)){
        monthRow = row
        break
      }
    }
  }
  
  # Get Hits/PA for row of month
  monthAVG = NA
  if (!is.na(monthRow)){
    if (pitcher){
      hits = monthRow %>% html_nodes(xpath='./td[7]') %>% html_text()
      pa = monthRow %>% html_nodes(xpath='./td[4]') %>% html_text()
      monthAVG = as.numeric(hits)/as.numeric(pa)
    }
    else {
      hits = monthRow %>% html_nodes(xpath='./td[8]') %>% html_text()
      pa = monthRow %>% html_nodes(xpath='./td[5]') %>% html_text()
      monthAVG = as.numeric(hits)/as.numeric(pa)     
    }
  }
  else{
    monthAVG = -1;
  }
  
  return (monthAVG)
}

# Retrieves park factor using the given home team abbreviation and the table of Park Factors
getParkFactor <- function(homeTeam){
  parkFactor = -1
  n = dim(ParkFactors)[1]
  homeTeam = StandardAbbr(homeTeam)
  
  tryCatch({
    for (i in 1:n){
      park = ParkFactors[i,]
      if(grepl(park[1,1],homeTeam)){
        parkFactor = as.character(park[1,2])
        parkFactor = gsub(",","",parkFactor)  # Remove comma
        parkFactor = gsub(" ","",parkFactor)  # Remove space
        parkFactor = as.numeric(parkFactor)   # Convert to number
        break
      }
    }
  }, error = function(e){
    cat("ERROR:",conditionMessage(e),"\n")
    })
  
  if (parkFactor == -1){
    print(paste("Team: ",homeTeam," not found in parkfactor table.",sep=""))  
  }
  
  return (parkFactor)
}















# Some abbreviations are outdated.  This attempts to mitigate those special cases
StandardAbbr <- function(teamName){
  team.Abbrs = config$StandardAbbrs
  
  for (abbr in names(team.Abbrs)){
    for (i in 1:length(team.Abbrs[[abbr]])) {
      if (grepl(teamName, team.Abbrs[[abbr]][i])){
        teamName = abbr
      }
    }
  }
  
  return(teamName)
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



#This function adjusts the names of all the columns, and makes the data types appropriate
cleanData <- function(data) {
  
  namesCareer = c("avgHandBatterCareer","avgPlaceBatterCareer","avgHandPitcherCareer","avgPlacePitcherCareer","sampleHandBatterCareer","samplePlaceBatterCareer","sampleHandPitcherCareer","samplePlacePitcherCareer")
  names2015 = gsub("Career","2015",namesCareer)
  names2014 = gsub("Career","2014",namesCareer)
  
  
  names(data)[1:8] = namesCareer
  names(data)[9:16] = names2015
  names(data)[17:24] = names2014
  names(data)[25:30] = c('hit','atBats','pitcherAvgInMonth','batterAvgInMonth','parkFactor','Date')
  
  numericCols = c(1:24,26:28)
  data[,numericCols] = apply(data[,numericCols], 2, function(x) as.numeric(as.character(x)))
  factorCols = c(25,29)
  data[,factorCols] = apply(data[,factorCols], 2, function(x) as.factor(x))
  
  return (data)
  
  
  
  
}




startDate = "2016/7/16"
endDate = "2016/7/16"
dat = scrapeCalender(startDate,endDate)
write.csv(dat,file="baseballData.csv")