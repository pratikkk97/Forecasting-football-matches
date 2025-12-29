setwd("~/Library/CloudStorage/OneDrive-LoughboroughUniversity/LUL/Modules/21LLP502 - Dissertation/Dissertation/Data/GAP/GAP_model_AUG_2022v2")

library(tidyverse)
library(readxl)
library('boot')
library('Metrics')
library('AICcmodavg')
source('get_initial_GAP_new.R')
source('fastmerge.R')
library(caret)
library('verification')
library(MCS)




##################### Read data
data_all=read_csv("data/Brazil_new.csv")
data_all=fastmerge(data_all,read_csv("data/Mexico_new.csv"))
data_all=fastmerge(data_all,read_csv("data/Colombia_new.csv"))
data_all=fastmerge(data_all,read_csv("data/England_new.csv"))
data_all=fastmerge(data_all,read_csv("data/Italy_new.csv"))
data_all=fastmerge(data_all,read_csv("data/Spain_new.csv"))


##################### Parameters
league_choices=c("Brazil. Serie A","Brazil. Serie B","England. Premier League","Spain. LaLiga","Spain. Segunda División","England. League One","England. Championship")

stat_choices=c("Shots","xG","Shots_on_target","smart_passes","crosses","Penalty_area_entries","Touches_in_penalty_area")

####### Remove unneeded rows (to save memory)

data_all=data_all[c("Date","Competition","Home_Team","Away_Team","Home_Goals","Away_Goals",paste("Home",stat_choices,sep="_"),paste("Away",stat_choices,sep="_"))]


##### Add datenums
tmp=numeric()
for (i in 1:dim(data_all)[1]){
	tmp[i]=unclass(as.Date(substr(data_all[[i,"Date"]],1,10),format="%Y-%m-%d")) ## add datenumbers (Days since 1st Jan 1970)
}
data_all['datenum']=tmp

data_all=data_all[order(data_all$datenum),] #### order by date


posleagues=numeric()
for (i in 1:length(league_choices)){
	posleagues=c(posleagues,which(data_all["Competition"]==league_choices[i]))
}
data_all=data_all[posleagues,]

##################### Add results to dataframe

data_all['Home_win']=as.integer(data_all$Home_Goals>data_all$Away_Goals)
data_all['Draw']=as.integer(data_all$Home_Goals==data_all$Away_Goals)
data_all['Away_win']=as.integer(data_all$Home_Goals<data_all$Away_Goals)


################################################
#### Calculate GAP ratings
################################################

##################### GAP parameters

lambda=0.3
phi1=0.6
phi2=0.6

#### initialise new columns in data_all
data_all["season"]=NA
for (stat_choice in stat_choices){
	data_all[[paste("Home",stat_choice,"pred",sep="_")]]=NA
	data_all[[paste("Away",stat_choice,"pred",sep="_")]]=NA
}

data_all[["Days_into_season"]]=NA

for (league_choice in league_choices){

	###### pull out league of interest
	pos_league=which(data_all['Competition']==league_choice)
	data_league=data_all[pos_league,] #### only chosen league

	### Add season numbers
	season=numeric()
	season[1]=1
	j=1
	for (i in 2:dim(data_league)[1]){
		if (as.numeric(data_league[i,'datenum']-data_league[i-1,'datenum'])>45){
			j=j+1
		}		
		season[i]=j	
	}

	data_league['season']=season

	############ Calculate GAP predictions for given league and season

	for (season_choice in 2:max(season)){
		pos_league_season=which(data_league$season==season_choice)
		data_league_season=data_league[pos_league_season,]
		data_all[["season"]][pos_league]=season

		
		data_all[["Days_into_season"]][pos_league[pos_league_season]]=data_all[["datenum"]][pos_league[pos_league_season]]-min(data_all[["datenum"]][pos_league[pos_league_season]]) ## number of days since first game of season

##### initialise GAP ratings
		uniqueteams=unique(data_league_season$Home_Team[data_league_season$season==season_choice & data_league_season$Competition==league_choice])
	
		for (stat_choice in stat_choices){
			GAPhomefor=numeric()
			GAPawayfor=numeric()
			GAPhomeagainst=numeric()
			GAPawayagainst=numeric()
			for (i in 1:length(uniqueteams)){
				output=get_initial_GAP_new(data_league,league_choice,season_choice-1,uniqueteams[i],stat_choice)
				GAPhomefor[i]=output$GAPhomefor
				GAPawayfor[i]=output$GAPawayfor
				GAPhomeagainst[i]=output$GAPhomeagainst
				GAPawayagainst[i]=output$GAPawayagainst
			}

			### initialise vector of predicted stat (1 for each match)
			Home_pred=numeric() 
			Away_pred=numeric()

			### Calculate predicted stat and update ratings after every match
			for (i in 1:length(data_league_season$Home_Team)) {
				posH=which(uniqueteams==data_league_season$Home_Team[i]) ### find location in "unique teams" of home team
				posA=which(uniqueteams==data_league_season$Away_Team[i]) ### find location in "unique teams" of away team
	
				Home_pred[i]=mean(c(GAPhomefor[posH],GAPawayagainst[posA])) ### estimate stat of home team (mean of home attacking and away defensive ratings)
				Away_pred[i]=mean(c(GAPawayfor[posA],GAPhomeagainst[posH])) ### estimate stat of away team (mean of away attacking and home defensive ratings)

				#### update GAP ratings after game
	
				GAPhomefor[posH]=max(GAPhomefor[posH]+phi1*lambda*(data_league_season[[paste("Home",stat_choice,sep="_")]][i]-Home_pred[i]),0)
				GAPawayfor[posH]=max(GAPawayfor[posH]+(1-phi1)*lambda*(data_league_season[[paste("Home",stat_choice,sep="_")]][i]-Home_pred[i]),0)
	
				GAPawayagainst[posA]=max(GAPawayagainst[posA]+phi2*lambda*(data_league_season[[paste("Home",stat_choice,sep="_")]][i]-Home_pred[i]),0)
				GAPhomeagainst[posA]=max(GAPhomeagainst[posA]+(1-phi2)*lambda*(data_league_season[[paste("Home",stat_choice,sep="_")]][i]-Home_pred[i]),0)
	
				GAPawayfor[posA]=max(GAPawayfor[posA]+phi2*lambda*(data_league_season[[paste("Away",stat_choice,sep="_")]][i]-Away_pred[i]),0)
				GAPhomefor[posA]=max(GAPhomefor[posA]+(1-phi2)*lambda*(data_league_season[[paste("Away",stat_choice,sep="_")]][i]-Away_pred[i]),0)

				GAPhomeagainst[posH]=max(GAPhomeagainst[posH]+phi1*lambda*(data_league_season[[paste("Away",stat_choice,sep="_")]][i]-Away_pred[i]),0)
				GAPawayagainst[posH]=max(GAPawayagainst[posH]+(1-phi1)*lambda*(data_league_season[[paste("Away",stat_choice,sep="_")]][i]-Away_pred[i]),0)
			}

			#data_league_season[paste("Home",stat_choice,"pred",sep="_")]=Home_pred
			#data_league_season[paste("Away",stat_choice,"pred",sep="_")]=Away_pred

			##### Add predicted stats to the overall table

			data_all[[paste("Home",stat_choice,"pred",sep="_")]][pos_league[pos_league_season]]=Home_pred
			data_all[[paste("Away",stat_choice,"pred",sep="_")]][pos_league[pos_league_season]]=Away_pred
		}			
	}
}

#write.csv(data_all, "data_all_season.csv")
data_all=data_all[which(data_all["season"]>1),] ## remove first season
#View(data_all)

#evaluating predicted stats with MAE

mae_Shots_m <-sum(mae(data_all$Home_Shots,data_all$Home_Shots_pred), mae(data_all$Away_Shots,data_all$Away_Shots_pred))
mae_Shots_m
mae_xG_m <-sum(mae(data_all$Home_xG,data_all$Home_xG_pred), mae(data_all$Away_xG,data_all$Away_xG_pred))
mae_xG_m
mae_Shots_on_target_m <-sum(mae(data_all$Home_Shots_on_target,data_all$Home_Shots_on_target_pred), mae(data_all$Away_Shots_on_target,data_all$Away_Shots_on_target_pred))
mae_Shots_on_target_m
mae_smart_passes_m <-sum(mae(data_all$Home_smart_passes,data_all$Home_smart_passes_pred), mae(data_all$Away_smart_passes,data_all$Away_smart_passes_pred))
mae_smart_passes_m
mae_crosses_m <-sum(mae(data_all$Home_crosses,data_all$Home_crosses_pred), mae(data_all$Away_crosses,data_all$Away_crosses_pred))
mae_crosses_m
mae_Penalty_area_entries_m <-sum(mae(data_all$Home_Penalty_area_entries,data_all$Home_Penalty_area_entries_pred), mae(data_all$Away_Penalty_area_entries,data_all$Away_Penalty_area_entries_pred))
mae_Penalty_area_entries_m
mae_Touches_in_penalty_area_m <-sum(mae(data_all$Home_Touches_in_penalty_area,data_all$Home_Touches_in_penalty_area_pred), mae(data_all$Away_Touches_in_penalty_area,data_all$Away_Touches_in_penalty_area_pred))
mae_Touches_in_penalty_area_m

#write.csv(data_all, "data_all.csv")
data_all_modified <- read.csv("data_all_modified.csv")
#view(data_all_modified)

mae_Shots_b <-sum(mae(data_all_modified$Home_Shots,data_all_modified$Home_Shots_mean), mae(data_all_modified$Away_Shots,data_all_modified$Away_Shots_mean))
mae_Shots_b
mae_xG_b <-sum(mae(data_all_modified$Home_xG,data_all_modified$Home_xG_mean), mae(data_all_modified$Away_xG,data_all_modified$Away_xG_mean))
mae_xG_b
mae_Shots_on_target_b <-sum(mae(data_all_modified$Home_Shots_on_target,data_all_modified$Home_Shots_on_target_mean), mae(data_all_modified$Away_Shots_on_target,data_all_modified$Away_Shots_on_target_mean))
mae_Shots_on_target_b
mae_smart_passes_b <-sum(mae(data_all_modified$Home_smart_passes,data_all_modified$Home_smart_passes_mean), mae(data_all_modified$Away_smart_passes,data_all_modified$Away_smart_passes_mean))
mae_smart_passes_b
mae_crosses_b <-sum(mae(data_all_modified$Home_crosses,data_all_modified$Home_crosses_mean), mae(data_all_modified$Away_crosses,data_all_modified$Away_crosses_mean))
mae_crosses_b
mae_Penalty_area_entries_b <-sum(mae(data_all_modified$Home_Penalty_area_entries,data_all_modified$Home_Penalty_area_entries_mean), mae(data_all_modified$Away_Penalty_area_entries,data_all_modified$Away_Penalty_area_entries_mean))
mae_Penalty_area_entries_b
mae_Touches_in_penalty_area_b <-sum(mae(data_all_modified$Home_Touches_in_penalty_area,data_all_modified$Home_Touches_in_penalty_area_mean), mae(data_all_modified$Away_Touches_in_penalty_area,data_all_modified$Away_Touches_in_penalty_area_mean))
mae_Touches_in_penalty_area_b

R_Shots = mae_Shots_m/mae_Shots_b
R_Shots
R_xG = mae_xG_m/mae_xG_b
R_xG
R_Shots_on_target = mae_Shots_on_target_m/mae_Shots_on_target_b
R_Shots_on_target
R_smart_passes = mae_smart_passes_m/mae_smart_passes_b
R_smart_passes
R_crosses = mae_crosses_m/mae_crosses_b
R_crosses
R_penalty_area_entries = mae_Penalty_area_entries_m/mae_Penalty_area_entries_b
R_penalty_area_entries
R_Touches_in_penalty_area = mae_Touches_in_penalty_area_m/mae_Touches_in_penalty_area_b
R_Touches_in_penalty_area

## fit model

data_all=data_all[which(data_all[["Days_into_season"]]>30),] ## only fit model to games played over 30 days into the season
view(data_all)

model_home <- #model for home win prob with pred stat
	rbind(
	data.frame(outcomes=data_all$Home_win==1,
					xGDiff=data_all$Home_xG_pred-data_all$Away_xG_pred,
					ShotsDiff=data_all$Home_Shots_pred-data_all$Away_Shots_pred,
					Shots_on_targetDiff=data_all$Home_Shots_on_target_pred-data_all$Away_Shots_on_target_pred,
					smart_passesDiff=data_all$Home_smart_passes_pred-data_all$Away_smart_passes_pred,
					crossesDiff=data_all$Home_crosses_pred-data_all$Away_crosses_pred,
					Penalty_area_entriesDiff=data_all$Home_Penalty_area_entries_pred-data_all$Away_Penalty_area_entries_pred,
					Touches_in_penalty_areaDiff=data_all$Home_Touches_in_penalty_area_pred-data_all$Away_Touches_in_penalty_area_pred)) %>%
	glm(outcomes ~ 1 + xGDiff+ShotsDiff+Shots_on_targetDiff+smart_passesDiff+crossesDiff+Penalty_area_entriesDiff+Touches_in_penalty_areaDiff, family=binomial, data=.)

summary(model_home)

# combination of variables

model_home_list <- list()

model_home_list[[1]] <- 
  rbind(
    data.frame(outcomes=data_all$Home_win==1,
               xGDiff=data_all$Home_xG_pred-data_all$Away_xG_pred)) %>%
  glm(outcomes ~ 1 + xGDiff, family=binomial, data=.)


model_home_list[[2]] <- 
  rbind(
    data.frame(outcomes=data_all$Home_win==1,
             Shots_on_targetDiff=data_all$Home_Shots_on_target_pred-data_all$Away_Shots_on_target_pred)) %>%
  glm(outcomes ~ 1 +Shots_on_targetDiff, family=binomial, data=.)

model_home_list[[3]] <- 
  rbind(
    data.frame(outcomes=data_all$Home_win==1,
               smart_passesDiff=data_all$Home_smart_passes_pred-data_all$Away_smart_passes_pred)) %>%
  glm(outcomes ~ 1 +smart_passesDiff, family=binomial, data=.)

model_home_list[[4]] <- 
  rbind(
    data.frame(outcomes=data_all$Home_win==1,
               Touches_in_penalty_areaDiff=data_all$Home_Touches_in_penalty_area_pred-data_all$Away_Touches_in_penalty_area_pred)) %>%
  glm(outcomes ~ 1 +Touches_in_penalty_areaDiff, family=binomial, data=.)

model_home_list[[5]] <-
  rbind(
    data.frame(outcomes=data_all$Home_win==1,
               xGDiff=data_all$Home_xG_pred-data_all$Away_xG_pred,
               Shots_on_targetDiff=data_all$Home_Shots_on_target_pred-data_all$Away_Shots_on_target_pred)) %>%
  glm(outcomes ~ 1 + xGDiff+Shots_on_targetDiff, family=binomial, data=.)

model_home_list[[6]] <-
  rbind(
    data.frame(outcomes=data_all$Home_win==1,
               xGDiff=data_all$Home_xG_pred-data_all$Away_xG_pred,
               smart_passesDiff=data_all$Home_smart_passes_pred-data_all$Away_smart_passes_pred)) %>%
  glm(outcomes ~ 1 + xGDiff+smart_passesDiff, family=binomial, data=.)

model_home_list[[7]] <-
  rbind(
    data.frame(outcomes=data_all$Home_win==1,
               xGDiff=data_all$Home_xG_pred-data_all$Away_xG_pred,
               Touches_in_penalty_areaDiff=data_all$Home_Touches_in_penalty_area_pred-data_all$Away_Touches_in_penalty_area_pred)) %>%
  glm(outcomes ~ 1 + xGDiff+Touches_in_penalty_areaDiff, family=binomial, data=.)

model_home_list[[8]] <-
  rbind(
    data.frame(outcomes=data_all$Home_win==1,
             Shots_on_targetDiff=data_all$Home_Shots_on_target_pred-data_all$Away_Shots_on_target_pred,
             smart_passesDiff=data_all$Home_smart_passes_pred-data_all$Away_smart_passes_pred)) %>%
  glm(outcomes ~ 1 + Shots_on_targetDiff+smart_passesDiff, family=binomial, data=.)

model_home_list[[9]] <-
  rbind(
    data.frame(outcomes=data_all$Home_win==1,
               Shots_on_targetDiff=data_all$Home_Shots_on_target_pred-data_all$Away_Shots_on_target_pred,
               Touches_in_penalty_areaDiff=data_all$Home_Touches_in_penalty_area_pred-data_all$Away_Touches_in_penalty_area_pred)) %>%
  glm(outcomes ~ 1 + Shots_on_targetDiff+Touches_in_penalty_areaDiff, family=binomial, data=.)

model_home_list[[10]] <-
  rbind(
    data.frame(outcomes=data_all$Home_win==1,
               smart_passesDiff=data_all$Home_smart_passes_pred-data_all$Away_smart_passes_pred,
               Touches_in_penalty_areaDiff=data_all$Home_Touches_in_penalty_area_pred-data_all$Away_Touches_in_penalty_area_pred)) %>%
  glm(outcomes ~ 1 + smart_passesDiff+Touches_in_penalty_areaDiff, family=binomial, data=.)

model_home_list[[11]] <-
  rbind(
    data.frame(outcomes=data_all$Home_win==1,
               xGDiff=data_all$Home_xG_pred-data_all$Away_xG_pred,
               Shots_on_targetDiff=data_all$Home_Shots_on_target_pred-data_all$Away_Shots_on_target_pred,
               smart_passesDiff=data_all$Home_smart_passes_pred-data_all$Away_smart_passes_pred)) %>%
  glm(outcomes ~ 1 + xGDiff+Shots_on_targetDiff+smart_passesDiff, family=binomial, data=.)

model_home_list[[12]] <-
  rbind(
    data.frame(outcomes=data_all$Home_win==1,
               Shots_on_targetDiff=data_all$Home_Shots_on_target_pred-data_all$Away_Shots_on_target_pred,
               smart_passesDiff=data_all$Home_smart_passes_pred-data_all$Away_smart_passes_pred,
               Touches_in_penalty_areaDiff=data_all$Home_Touches_in_penalty_area_pred-data_all$Away_Touches_in_penalty_area_pred)) %>%
  glm(outcomes ~ 1 + Shots_on_targetDiff+smart_passesDiff+Touches_in_penalty_areaDiff, family=binomial, data=.)

model_home_list[[13]] <-
  rbind(
    data.frame(outcomes=data_all$Home_win==1,
               xGDiff=data_all$Home_xG_pred-data_all$Away_xG_pred,
               Shots_on_targetDiff=data_all$Home_Shots_on_target_pred-data_all$Away_Shots_on_target_pred,
               smart_passesDiff=data_all$Home_smart_passes_pred-data_all$Away_smart_passes_pred,
               Touches_in_penalty_areaDiff=data_all$Home_Touches_in_penalty_area_pred-data_all$Away_Touches_in_penalty_area_pred)) %>%
  glm(outcomes ~ 1 + xGDiff+Shots_on_targetDiff+smart_passesDiff+Touches_in_penalty_areaDiff, family=binomial, data=.)

summary(model_home_list[[13]])
##check c-hat for global model 
#c_hat(model_home_list[[1]], method = "pearson")

#summaryOD(model_home_list[[1]], c.hat = 1.04)

##assign names to each model 
Modnames <- c("xG","Shots_on_target","smart_passes","Touches_in_penalty_area","xG+Shots_on_target","xG+smart_passes", "xG+Touches_in_penalty_area",
              "Shots_on_target+smart_passes", "Shots_on_target+Touches_in_penalty_area", "smart_passes+Touches_in_penalty_area", "xG+Shots_on_target+smart_passes",
              "Shots_on_target+smart_passes+Touches_in_penalty_area", "xG+Shots_on_target+smart_passes+Touches_in_penalty_area")

##model selection table based on AICc 
aictab(cand.set = model_home_list, modnames = Modnames) 

##compute evidence ratio 
evidence(aictab(cand.set = model_home_list, modnames = Modnames)) 

##compute confidence set based on 'raw' method 
confset(cand.set = model_home_list, modnames = Modnames, second.ord = TRUE, method = "raw")

model_away <- #model for away win prob with pred stat
	rbind(
	data.frame(outcomes=data_all$Away_win==1,
					xGDiff=data_all$Home_xG_pred-data_all$Away_xG_pred,
					#ShotsDiff=data_all$Home_Shots_pred-data_all$Away_Shots_pred,
					Shots_on_targetDiff=data_all$Home_Shots_on_target_pred-data_all$Away_Shots_on_target_pred,
					smart_passesDiff=data_all$Home_smart_passes_pred-data_all$Away_smart_passes_pred,
					#crossesDiff=data_all$Home_crosses_pred-data_all$Away_crosses_pred,
					#Penalty_area_entriesDiff=data_all$Home_Penalty_area_entries_pred-data_all$Away_Penalty_area_entries_pred,
					Touches_in_penalty_areaDiff=data_all$Home_Touches_in_penalty_area_pred-data_all$Away_Touches_in_penalty_area_pred)) %>%
	glm(outcomes ~ 1 + xGDiff++Shots_on_targetDiff+smart_passesDiff+Touches_in_penalty_areaDiff, family=binomial, data=.)

summary(model_away)

##### Add forecasts to dataframe.

data_all$p_home=predict(model_home_list[[13]],data.frame(xGDiff=data_all$Home_xG_pred-data_all$Away_xG_pred,
                                              ShotsDiff=data_all$Home_Shots_pred-data_all$Away_Shots_pred,
                                              Shots_on_targetDiff=data_all$Home_Shots_on_target_pred-data_all$Away_Shots_on_target_pred,
                                              smart_passesDiff=data_all$Home_smart_passes_pred-data_all$Away_smart_passes_pred,
                                              crossesDiff=data_all$Home_crosses_pred-data_all$Away_crosses_pred,
                                              Penalty_area_entriesDiff=data_all$Home_Penalty_area_entries_pred-data_all$Away_Penalty_area_entries_pred,
                                              Touches_in_penalty_areaDiff=data_all$Home_Touches_in_penalty_area_pred-data_all$Away_Touches_in_penalty_area_pred),type="response")

rps(data_all$Home_win,data.matrix(data.frame(zeros = as.numeric(1 - data_all$p_home), ones = as.numeric(data_all$p_home))))

data_all$p_away=predict(model_away,data.frame(xGDiff=data_all$Home_xG_pred-data_all$Away_xG_pred,
                                              ShotsDiff=data_all$Home_Shots_pred-data_all$Away_Shots_pred,
                                              Shots_on_targetDiff=data_all$Home_Shots_on_target_pred-data_all$Away_Shots_on_target_pred,
                                              smart_passesDiff=data_all$Home_smart_passes_pred-data_all$Away_smart_passes_pred,
                                              crossesDiff=data_all$Home_crosses_pred-data_all$Away_crosses_pred,
                                              Penalty_area_entriesDiff=data_all$Home_Penalty_area_entries_pred-data_all$Away_Penalty_area_entries_pred,
                                              Touches_in_penalty_areaDiff=data_all$Home_Touches_in_penalty_area_pred-data_all$Away_Touches_in_penalty_area_pred),type="response")

data_all$p_draw=1-data_all$p_home-data_all$p_away

data_all$predicted_classes_home <- ifelse(data_all$p_home > 0.5, 1, 0)
data_all$predicted_classes_away <- ifelse(data_all$p_away > 0.5, 1, 0)
data_all$predicted_classes_draw <- ifelse(data_all$p_draw > 0.5, 1, 0)

view(data_all)

# rps(data_all$Home_win,data_all$predicted_classes_home)


rps(data_all$Away_win,data.matrix(data.frame(zeros = as.numeric(1 - data_all$p_away), ones = as.numeric(data_all$p_away))))
rps(data_all$Draw,data.matrix(data.frame(zeros = as.numeric(1 - data_all$p_draw), ones = as.numeric(data_all$p_draw))))

data(Loss)
MCS <- MCSprocedure(Loss=Loss[,1:5],alpha=0.2,B=5000,statistic='Tmax',cl=NULL)

typeof(Loss[,1:5])

Model_13_LL <- LossLevel(se(as.numeric(data_all$Home_win),as.numeric(data_all$p_home)),ae(as.numeric(data_all$Home_win),as.numeric(data_all$p_home)))
Model_12_LL <- LossLevel(se(as.numeric(data_all$Home_win),as.numeric(data_all$p_home)),ae(as.numeric(data_all$Home_win),as.numeric(data_all$p_home)))
Model_11_LL <- LossLevel(se(as.numeric(data_all$Home_win),as.numeric(data_all$p_home)),ae(as.numeric(data_all$Home_win),as.numeric(data_all$p_home)))
Model_10_LL <- LossLevel(se(as.numeric(data_all$Home_win),as.numeric(data_all$p_home)),ae(as.numeric(data_all$Home_win),as.numeric(data_all$p_home)))
Model_9_LL <- LossLevel(se(as.numeric(data_all$Home_win),as.numeric(data_all$p_home)),ae(as.numeric(data_all$Home_win),as.numeric(data_all$p_home)))
Model_8_LL <- LossLevel(se(as.numeric(data_all$Home_win),as.numeric(data_all$p_home)),ae(as.numeric(data_all$Home_win),as.numeric(data_all$p_home)))
Model_7_LL <- LossLevel(se(as.numeric(data_all$Home_win),as.numeric(data_all$p_home)),ae(as.numeric(data_all$Home_win),as.numeric(data_all$p_home)))
Model_6_LL <- LossLevel(se(as.numeric(data_all$Home_win),as.numeric(data_all$p_home)),ae(as.numeric(data_all$Home_win),as.numeric(data_all$p_home)))
Model_5_LL <- LossLevel(se(as.numeric(data_all$Home_win),as.numeric(data_all$p_home)),ae(as.numeric(data_all$Home_win),as.numeric(data_all$p_home)))
Model_4_LL <- LossLevel(se(as.numeric(data_all$Home_win),as.numeric(data_all$p_home)),ae(as.numeric(data_all$Home_win),as.numeric(data_all$p_home)))
Model_3_LL <- LossLevel(se(as.numeric(data_all$Home_win),as.numeric(data_all$p_home)),ae(as.numeric(data_all$Home_win),as.numeric(data_all$p_home)))
Model_2_LL <- LossLevel(se(as.numeric(data_all$Home_win),as.numeric(data_all$p_home)),ae(as.numeric(data_all$Home_win),as.numeric(data_all$p_home)))
Model_1_LL <- LossLevel(se(as.numeric(data_all$Home_win),as.numeric(data_all$p_home)),ae(as.numeric(data_all$Home_win),as.numeric(data_all$p_home)))

Model_LL <- data.frame(Model_12_LL,Model_13_LL,Model_11_LL,Model_10_LL,Model_9_LL,Model_8_LL,Model_7_LL,Model_6_LL,Model_5_LL,Model_4_LL,Model_3_LL,Model_2_LL,Model_1_LL)
view(Model_LL)
MCS <- MCSprocedure(Model_LL,alpha=0.2,B=5000,statistic='Tmax',cl=NULL)


rankProbScore(as.vector(data_all$Home_win),data.matrix(data.frame(zeros = as.numeric(1 - data_all$p_home), ones = as.numeric(data_all$p_home))))
data_all$Home_win
as.vector(data_all$Home_win)
dim(data.matrix(data.frame(zeros = as.numeric(1 - data_all$p_home), ones = as.numeric(data_all$p_home))))
str(as.numeric(1 - data_all$p_home))


data_all$predicted_classes_home <- as.factor(data_all$predicted_classes_home)
data_all$Home_win <- as.factor(data_all$Home_win)
data_all$predicted_classes_away <- as.factor(data_all$predicted_classes_away)
data_all$Away_win <- as.factor(data_all$Away_win)
data_all$predicted_classes_draw <- as.factor(data_all$predicted_classes_draw)
data_all$Draw <- as.factor(data_all$Draw)

#Creating confusion matrix
Home_win_cf <- confusionMatrix(data = data_all$predicted_classes_home, reference = data_all$Home_win)
Home_win_cf
Away_win_cf <- confusionMatrix(data = data_all$predicted_classes_away, reference = data_all$Away_win)
Away_win_cf


view(data_all)

model_home_obsv <- list()

model_home_obsv[[1]] <- #model for home win prob with obsv stat
  rbind(
    data.frame(outcomes=data_all$Home_win==1,
               xGDiff=data_all$Home_xG-data_all$Away_xG,
               ShotsDiff=data_all$Home_Shots-data_all$Away_Shots,
               Shots_on_targetDiff=data_all$Home_Shots_on_target-data_all$Away_Shots_on_target,
               smart_passesDiff=data_all$Home_smart_passes-data_all$Away_smart_passes,
               crossesDiff=data_all$Home_crosses-data_all$Away_crosses,
               Penalty_area_entriesDiff=data_all$Home_Penalty_area_entries-data_all$Away_Penalty_area_entries,
               Touches_in_penalty_areaDiff=data_all$Home_Touches_in_penalty_area_pred-data_all$Away_Touches_in_penalty_area_pred)) %>%
  glm(outcomes ~ 1 + xGDiff+ShotsDiff+Shots_on_targetDiff+smart_passesDiff+crossesDiff+Penalty_area_entriesDiff+Touches_in_penalty_areaDiff, family=binomial, data=.)

summary(model_home_obsv[[1]])

model_home_obsv[[2]] <- #model for home win prob with obsv stat
  rbind(
    data.frame(outcomes=data_all$Home_win==1,
               xGDiff=data_all$Home_xG-data_all$Away_xG,
               ShotsDiff=data_all$Home_Shots-data_all$Away_Shots,
               Shots_on_targetDiff=data_all$Home_Shots_on_target-data_all$Away_Shots_on_target,
               smart_passesDiff=data_all$Home_smart_passes-data_all$Away_smart_passes,
               crossesDiff=data_all$Home_crosses-data_all$Away_crosses,
               Touches_in_penalty_areaDiff=data_all$Home_Touches_in_penalty_area_pred-data_all$Away_Touches_in_penalty_area_pred)) %>%
  glm(outcomes ~ 1 + xGDiff+ShotsDiff+Shots_on_targetDiff+smart_passesDiff+crossesDiff+Touches_in_penalty_areaDiff, family=binomial, data=.)

summary(model_home_obsv[[2]])

model_home_obsv[[3]] <- #model for home win prob with obsv stat
  rbind(
    data.frame(outcomes=data_all$Home_win==1,
               xGDiff=data_all$Home_xG-data_all$Away_xG,
               Shots_on_targetDiff=data_all$Home_Shots_on_target-data_all$Away_Shots_on_target,
               smart_passesDiff=data_all$Home_smart_passes-data_all$Away_smart_passes,
               Touches_in_penalty_areaDiff=data_all$Home_Touches_in_penalty_area_pred-data_all$Away_Touches_in_penalty_area_pred)) %>%
  glm(outcomes ~ 1 + xGDiff+Shots_on_targetDiff+smart_passesDiff+Touches_in_penalty_areaDiff, family=binomial, data=.)

model_home_obsv[[4]] <- #model for home win prob with obsv stat
  rbind(
    data.frame(outcomes=data_all$Home_win==1,
               xGDiff=data_all$Home_xG-data_all$Away_xG,
               Shots_on_targetDiff=data_all$Home_Shots_on_target-data_all$Away_Shots_on_target)) %>%
  glm(outcomes ~ 1 + xGDiff+Shots_on_targetDiff, family=binomial, data=.)

Modnames2 <- c("xG + Shots + Shots_on_target + smart_passes + crosses + Penalty_area_entries + Touches_in_penalty_area",
               "xG + Shots + Shots_on_target + smart_passes + crosses + Touches_in_penalty_area",
               "xG + Shots_on_target + smart_passes + Touches_in_penalty_area", 
               "xG + Shots_on_target")
##model selection table based on AICc 
aictab(cand.set = model_home_obsv, modnames = Modnames2)

##compute confidence set based on 'raw' method 
confset(cand.set = model_home_obsv, modnames = Modnames2, second.ord = TRUE, method = "raw")


model_away_obsv <- list()

model_away_obsv[[1]] <- #model for home win prob with obsv stat
  rbind(
    data.frame(outcomes=data_all$Away_win==1,
               xGDiff=data_all$Home_xG-data_all$Away_xG,
               ShotsDiff=data_all$Home_Shots-data_all$Away_Shots,
               Shots_on_targetDiff=data_all$Home_Shots_on_target-data_all$Away_Shots_on_target,
               smart_passesDiff=data_all$Home_smart_passes-data_all$Away_smart_passes,
               crossesDiff=data_all$Home_crosses-data_all$Away_crosses,
               Penalty_area_entriesDiff=data_all$Home_Penalty_area_entries-data_all$Away_Penalty_area_entries,
               Touches_in_penalty_areaDiff=data_all$Home_Touches_in_penalty_area_pred-data_all$Away_Touches_in_penalty_area_pred)) %>%
  glm(outcomes ~ 1 + xGDiff+ShotsDiff+Shots_on_targetDiff+smart_passesDiff+crossesDiff+Penalty_area_entriesDiff+Touches_in_penalty_areaDiff, family=binomial, data=.)

summary(model_away_obsv[[1]])

model_home_goal <- #model for home goal with pred stat
  rbind(
    data.frame(outcomes=data_all$Home_Goals,
               xGDiff=data_all$Home_xG_pred-data_all$Away_xG_pred,
               ShotsDiff=data_all$Home_Shots_pred-data_all$Away_Shots_pred,
               Shots_on_targetDiff=data_all$Home_Shots_on_target_pred-data_all$Away_Shots_on_target_pred,
               smart_passesDiff=data_all$Home_smart_passes_pred-data_all$Away_smart_passes_pred,
               crossesDiff=data_all$Home_crosses_pred-data_all$Away_crosses_pred,
               Penalty_area_entriesDiff=data_all$Home_Penalty_area_entries_pred-data_all$Away_Penalty_area_entries_pred,
               Touches_in_penalty_areaDiff=data_all$Home_Touches_in_penalty_area_pred-data_all$Away_Touches_in_penalty_area_pred)) %>%
  glm(outcomes ~ 1 + xGDiff+ShotsDiff+Shots_on_targetDiff+smart_passesDiff+crossesDiff+Penalty_area_entriesDiff+Touches_in_penalty_areaDiff, family='gaussian', data=.)

summary(model_home_goal)



model_away_goal <- #model for away goal with pred stat
  rbind(
    data.frame(outcomes=data_all$Away_Goals,
               xGDiff=data_all$Home_xG_pred-data_all$Away_xG_pred,
               ShotsDiff=data_all$Home_Shots_pred-data_all$Away_Shots_pred,
               Shots_on_targetDiff=data_all$Home_Shots_on_target_pred-data_all$Away_Shots_on_target_pred,
               smart_passesDiff=data_all$Home_smart_passes_pred-data_all$Away_smart_passes_pred,
               crossesDiff=data_all$Home_crosses_pred-data_all$Away_crosses_pred,
               Penalty_area_entriesDiff=data_all$Home_Penalty_area_entries_pred-data_all$Away_Penalty_area_entries_pred,
               Touches_in_penalty_areaDiff=data_all$Home_Touches_in_penalty_area_pred-data_all$Away_Touches_in_penalty_area_pred)) %>%
  glm(outcomes ~ 1 + xGDiff+ShotsDiff+Shots_on_targetDiff+smart_passesDiff+crossesDiff+Penalty_area_entriesDiff+Touches_in_penalty_areaDiff, family='gaussian', data=.)

summary(model_away_goal)

model_home_goal_obsv <- #model for home goal with obsv stat
  rbind(
    data.frame(outcomes=data_all$Home_Goals,
               xGDiff=data_all$Home_xG-data_all$Away_xG,
               ShotsDiff=data_all$Home_Shots-data_all$Away_Shots,
               Shots_on_targetDiff=data_all$Home_Shots_on_target-data_all$Away_Shots_on_target,
               smart_passesDiff=data_all$Home_smart_passes-data_all$Away_smart_passes,
               crossesDiff=data_all$Home_crosses-data_all$Away_crosses,
               Penalty_area_entriesDiff=data_all$Home_Penalty_area_entries-data_all$Away_Penalty_area_entries,
               Touches_in_penalty_areaDiff=data_all$Home_Touches_in_penalty_area-data_all$Away_Touches_in_penalty_area)) %>%
  glm(outcomes ~ 1 + xGDiff+ShotsDiff+Shots_on_targetDiff+smart_passesDiff+crossesDiff+Penalty_area_entriesDiff+Touches_in_penalty_areaDiff, family='gaussian', data=.)

summary(model_home_goal_obsv)

data_all$p_home_goal_obsv=predict(model_home_goal_obsv,data.frame(xGDiff=data_all$Home_xG_pred-data_all$Away_xG_pred,
                                              ShotsDiff=data_all$Home_Shots_pred-data_all$Away_Shots_pred,
                                              Shots_on_targetDiff=data_all$Home_Shots_on_target_pred-data_all$Away_Shots_on_target_pred,
                                              smart_passesDiff=data_all$Home_smart_passes_pred-data_all$Away_smart_passes_pred,
                                              crossesDiff=data_all$Home_crosses_pred-data_all$Away_crosses_pred,
                                              Penalty_area_entriesDiff=data_all$Home_Penalty_area_entries_pred-data_all$Away_Penalty_area_entries_pred,
                                              Touches_in_penalty_areaDiff=data_all$Home_Touches_in_penalty_area_pred-data_all$Away_Touches_in_penalty_area_pred),type="response")

data_all$p_home_goal=predict(model_home_goal,data.frame(xGDiff=data_all$Home_xG_pred-data_all$Away_xG_pred,
                                              ShotsDiff=data_all$Home_Shots_pred-data_all$Away_Shots_pred,
                                              Shots_on_targetDiff=data_all$Home_Shots_on_target_pred-data_all$Away_Shots_on_target_pred,
                                              smart_passesDiff=data_all$Home_smart_passes_pred-data_all$Away_smart_passes_pred,
                                              crossesDiff=data_all$Home_crosses_pred-data_all$Away_crosses_pred,
                                              Penalty_area_entriesDiff=data_all$Home_Penalty_area_entries_pred-data_all$Away_Penalty_area_entries_pred,
                                              Touches_in_penalty_areaDiff=data_all$Home_Touches_in_penalty_area_pred-data_all$Away_Touches_in_penalty_area_pred),type="response")

view(data_all)



#Corr Obsv - Goals & Obsv Stat

cor(data_all$Home_Goals,data_all$Home_Goals)
cor(data_all$Home_Goals,data_all$Home_xG)
cor(data_all$Home_Goals,data_all$Home_Shots)
cor(data_all$Home_Goals,data_all$Home_Shots_on_target)
cor(data_all$Home_Goals,data_all$Home_smart_passes)
cor(data_all$Home_Goals,data_all$Home_Penalty_area_entries)
cor(data_all$Home_Goals,data_all$Home_Touches_in_penalty_area)

#Corr Obsv - Goals & Pred Stat

cor(data_all$Home_Goals,data_all$p_home_goal_obsv)
cor(data_all$Home_Goals,data_all$p_home_goal)
cor(data_all$Home_Goals,data_all$Home_xG_pred)
cor(data_all$Home_Goals,data_all$Home_Shots_pred)
cor(data_all$Home_Goals,data_all$Home_Shots_on_target_pred)
cor(data_all$Home_Goals,data_all$Home_smart_passes_pred)
cor(data_all$Home_Goals,data_all$Home_Penalty_area_entries_pred)
cor(data_all$Home_Goals,data_all$Home_Touches_in_penalty_area_pred)

#Corr Obsv - Goals & Obsv Stat (MSE)

mse(data_all$Home_Goals,data_all$Home_Goals)
mse(data_all$Home_Goals,data_all$Home_xG)
mse(data_all$Home_Goals,data_all$Home_Shots)
mse(data_all$Home_Goals,data_all$Home_Shots_on_target)
mse(data_all$Home_Goals,data_all$Home_smart_passes)
mse(data_all$Home_Goals,data_all$Home_Penalty_area_entries)
mse(data_all$Home_Goals,data_all$Home_Touches_in_penalty_area)

#Corr Obsv - Goals & Pred Stat (MSE)

mse(data_all$Home_Goals,data_all$p_home_goal_obsv)
mse(data_all$Home_Goals,data_all$p_home_goal)
mse(data_all$Home_Goals,data_all$Home_xG_pred)
mse(data_all$Home_Goals,data_all$Home_Shots_pred)
mse(data_all$Home_Goals,data_all$Home_Shots_on_target_pred)
mse(data_all$Home_Goals,data_all$Home_smart_passes_pred)
mse(data_all$Home_Goals,data_all$Home_Penalty_area_entries_pred)
mse(data_all$Home_Goals,data_all$Home_Touches_in_penalty_area_pred)




model_away_goal_obsv <- #model for away goal with obsv stat
  rbind(
    data.frame(outcomes=data_all$Away_Goals,
               xGDiff=data_all$Home_xG-data_all$Away_xG,
               ShotsDiff=data_all$Home_Shots-data_all$Away_Shots,
               Shots_on_targetDiff=data_all$Home_Shots_on_target-data_all$Away_Shots_on_target,
               smart_passesDiff=data_all$Home_smart_passes-data_all$Away_smart_passes,
               crossesDiff=data_all$Home_crosses-data_all$Away_crosses,
               Penalty_area_entriesDiff=data_all$Home_Penalty_area_entries-data_all$Away_Penalty_area_entries,
               Touches_in_penalty_areaDiff=data_all$Home_Touches_in_penalty_area-data_all$Away_Touches_in_penalty_area)) %>%
  glm(outcomes ~ 1 + xGDiff+ShotsDiff+Shots_on_targetDiff+smart_passesDiff+crossesDiff+Penalty_area_entriesDiff+Touches_in_penalty_areaDiff, family='gaussian', data=.)

summary(model_away_goal_obsv)
