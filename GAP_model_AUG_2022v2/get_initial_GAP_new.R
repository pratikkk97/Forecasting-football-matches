get_initial_GAP_new <- function (data_in,league_name,season_no,team,stat_name){

Homestat=data_in[[paste("Home",stat_name,sep="_")]]
Awaystat=data_in[[paste("Away",stat_name,sep="_")]]

pos1=which(data_in$season==season_no & data_in$Competition==league_name & data_in$Home_Team==team)
pos2=which(data_in$season==season_no & data_in$Competition==league_name & data_in$Away_Team==team)

if (length(pos1)>0 & length(pos2)>0){ #### if team not in league last season, take average over whole league
	GAPhomefor=mean(Homestat[pos1],na.rm=TRUE)
	GAPawayfor=mean(Awaystat[pos2],na.rm=TRUE)
	GAPhomeagainst=mean(Homestat[pos1],na.rm=TRUE)
	GAPawayagainst=mean(Awaystat[pos2],na.rm=TRUE)
} else { 
	posA=which(data_in$season==season_no & data_in$Competition==league_name,data_in$Home_Team==team)
	posB=which(data_in$season==season_no & data_in$Competition==league_name,data_in$Away_Team==team)
	
	GAPhomefor=mean(Homestat[posA],na.rm=TRUE)
	GAPawayfor=mean(Awaystat[posB],na.rm=TRUE)
	GAPhomeagainst=mean(Awaystat[posB],na.rm=TRUE)
	GAPawayagainst=mean(Homestat[posA],na.rm=TRUE)
}

### outputs
output=list()
output$GAPhomefor=GAPhomefor
output$GAPawayfor=GAPawayfor
output$GAPhomeagainst=GAPhomeagainst
output$GAPawayagainst=GAPawayagainst

return(output)}


