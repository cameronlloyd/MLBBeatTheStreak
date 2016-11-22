library('rvest')
library('XML')

config <- config::get()

#Scrape all park factors for pitchers and batters at given year.  Should only need to be ran once
#Values above 1 indicate that the park is good for hitters, and values below 1 indicate that the park is good for pitchers
scrapeParkFactors <- function(year) {
  data = data.frame(ParkName = c(), Hits = c())
  
  link = paste("http://www.espn.com/mlb/stats/parkfactor/_/year/",year,sep="")
  page = read_html(link)
  team.Abbrs = config$TeamAbbrs
  
  park.factors = html_nodes(page, xpath="//table[contains(@class,'tablehead')]//tr")
  lastTeam = ""
  for (park in park.factors) {
    rowClass = html_attr(park,"class")
    if(rowClass == "stathead" || rowClass == "colhead") next  # Skip first two rows in table
    
    # Get stadium name and associate it with team name.
    # Then, associate team name with it's abbreviation
    stadiumLink = park %>% html_nodes(xpath='./td[2]/a') %>% xml_attr("href")
    teamName = getTeamName(stadiumLink)
    
    # Seems to be duplicate rows sometimes
    if (!grepl(teamName,lastTeam)){
      tryCatch({
        for (abbr in names(team.Abbrs)){
          for (i in 1:length(team.Abbrs[[abbr]])) {
            if (grepl(teamName, team.Abbrs[[abbr]][i])){
              teamName = abbr
            }
          }
        }
      }, error = function(e){cat("ERROR:",conditionMessage(e),"\n")})
      
      
      # Get runs for stadium
      stadiumHits = park %>% html_nodes(xpath='./td[5]') %>% html_text()
      
      tryCatch({
        # Bind info to table
        data = rbind(data, data.frame(TeamPark= c(teamName), Hits=c(as.numeric(stadiumHits))))
      }, error = function(e){cat("ERROR:",conditionMessage(e)," for: ", teamName," hits: ", stadiumHits,"\n")})
      
      lastTeam = teamName
    }
  
  }
  
  # Write results to file
  fileName = paste("./ParkFactors/",toString(year),sep="")
  fileName = paste(fileName,".csv",sep="")
  write.csv(data,file=fileName,row.names=FALSE)
}

# Gets team name associated with stadium at 'stadium.link'
getTeamName <- function(stadium.link){
  link = read_html(stadium.link)
  
  teamName = html_nodes(link, xpath="//img[contains(@class,'teamlogo')]/../../h3") %>% html_text()
  
  return (teamName)
}

scrapeParkFactors(2016)
