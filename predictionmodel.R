library(fplscrapR)
library(tidyverse)
library(here)
library(brms)
library(rvest)
library(stringr)
library(lpSolve)


#
help(package = fplscrapR)

#
setwd("C:/Users/User/Documents/R/Fantasy football")
getwd()


######## get data ######################

hist <- get_player_details(1:423)
hist18 <- get_player_details(season = 18)

histfiltered <- select(hist,round,was_home,opponent_team,total_points,minutes,playername)
playerteam <- get_player_info() %>% select(playername,team) 
histfiltered <- left_join(histfiltered,playerteam,by="playername")

############## get team names


#Specifying the url for desired website to be scraped
url <- 'https://www.premierleague.com/clubs'

#Reading the HTML code from the website
webpage <- read_html(url)

#Using CSS selectors to scrape the rankings section
teams_html <- html_nodes(webpage,'.clubName')

#Converting the ranking data to text
teams <- html_text(teams_html)
teams <- as.data.frame(teams)

########## recode teams to names

histfiltered$team <- as.factor(histfiltered$team)
levels(histfiltered$team) <- as.factor(teams$teams)

histfiltered$opponent_team <- as.factor(histfiltered$opponent_team)
levels(histfiltered$opponent_team) <- as.factor(teams$teams)


########## add 2018 data

hist18filtered <- select(hist18,round,was_home,opponent_team,total_points,minutes,playername)
playerteam18 <- get_player_info(season=18) %>% select(playername,team) 
hist18filtered <- left_join(hist18filtered,playerteam18,by="playername")


########## recode teams to names
teams2018 <- read.csv("~/R/Fantasy football/teams2018.txt", header=FALSE) %>% .[,1]

hist18filtered$team <- as.factor(hist18filtered$team)
levels(hist18filtered$team) <- teams2018

hist18filtered$opponent_team <- as.factor(hist18filtered$opponent_team)
levels(hist18filtered$opponent_team) <- as.factor(teams2018)

################

histfiltered$round <- histfiltered$round + 38

###########

histfiltered$team <- as.character(histfiltered$team)
histfiltered$opponent_team <- as.character(histfiltered$opponent_team)

hist18filtered$team <- as.character(hist18filtered$team)
hist18filtered$opponent_team <- as.character(hist18filtered$opponent_team)

total <- rbind(histfiltered,hist18filtered,by="playername")

total$team <- as.factor(total$team)
total$opponent_team <- as.factor(total$opponent_team)
total$round <- as.numeric(total$round)

#### add weights ####

round <- unique(total$round)
round <- sort(round)
weights <- exp(round/150)
plot(weights)
weight.frame <- data.frame(round,weights)

data <- left_join(total,weight.frame,by="round")

data$was_home <- as.factor(data$was_home)
data$total_points <- as.numeric(data$total_points)
data$minutes <- as.numeric(data$minutes)
data$playername <- as.factor(data$playername)

data <- filter(data,!is.na(weights))
  

data <- group_by(data,playername) %>% arrange(desc(round)) %>% mutate(form = zoo::rollmean(total_points,k=10,fill=NA,align="left")) %>% ungroup() %>% as.data.frame()

data <- mutate(data,points_grouped = cut(total_points,breaks = c(-Inf,0,2,4,6,8,Inf), labels=c("Zero or less","0 to 2","2 to 4","4 to 6","6 to 8","8 plus")))

str(data)


######## Random forest attempt
library(randomForestSRC)
library(caret)

set.seed(3456)
trainIndex <- createDataPartition(data$total_points, p = .8, 
                                  list = FALSE, 
                                  times = 1)

head(trainIndex) ##### create data split


training <- data[ trainIndex,] #training set

testing  <- data[-trainIndex,] #test set




#Tune randomforest model
#tune <- tune(total_points ~ playername + opponent_team + was_home + form + minutes +  opponent_team:team, data = data1)
#print(tune$rf)


########### WITH WEIGHTS BUT CANT DO INTERACTION. ACCURACY LOWER
#rf <- ranger::ranger(total_points ~ playername + opponent_team + was_home + minutes , data = data, case.weights = data$weights,
#                     num.trees = 3000) 



start_time <- Sys.time()
rf <- randomForestSRC::rfsrc(total_points ~ playername + opponent_team + was_home + form + minutes +  opponent_team:team, data = training, 
            na.action = "na.omit",ntree=2000,mtry=5,nodesize=9,importance = TRUE,xvar.wt = c(1,1,1,1.5,1,1))
end_time <- Sys.time()

end_time-start_time

plot(rf)
rf



##### out of sample prediction test

prediction1 <-predict.rfsrc(rf, testing)
prediction1


######### Get next week's fixtures ######

fixtures <- get_game_list()
thisweek <- filter(fixtures,GW > max(hist$round) , GW < max(hist$round)+5)

week <- max(hist$round)+39

thisweekfixtures <- rbind(
  thisweek %>% mutate(team=team_h,oppo=team_a,was_home="TRUE"),
  thisweek %>% mutate(team=team_a,oppo=team_h,was_home="FALSE"))

thisweekfixtures <- select(thisweekfixtures,round=GW,team,opponent_team=oppo,was_home)
thisweekfixtures$round <- thisweekfixtures$round + 38

thisweekfixtures$was_home <- as.factor(thisweekfixtures$was_home)

thisweekfixtures$team <- as.factor(thisweekfixtures$team)
levels(thisweekfixtures$team) <- as.factor(teams$teams)

thisweekfixtures$opponent_team <- as.factor(thisweekfixtures$opponent_team)
levels(thisweekfixtures$opponent_team) <- as.factor(teams$teams)

thisweekfixtures$team <- as.character(thisweekfixtures$team)
thisweekfixtures$opponent_team <- as.character(thisweekfixtures$opponent_team)


players <- data %>% filter(round==max(data$round)) %>% select(team,playername,form)
injurystatus <- get_player_info() %>% select(playername,status)
players <- full_join(players,injurystatus)
players$team <- as.character(players$team)

data3 <- full_join(thisweekfixtures,players)
data3$team <- as.factor(data3$team)
data3$playername <- as.factor(data3$playername)

data3 <- mutate(data3,minutes = if_else(status=="a",90,if_else(status=="d",60,0)))
data3 <- mutate(data3, total_points=NA)

data3$total_points <- as.numeric(data3$total_points)
data3$opponent_team <- as.factor(data3$opponent_team)

topredict <- select(data3, playername , team, opponent_team , was_home, form, minutes, total_points,round)
topredict <- mutate(topredict,weights=2)
topredict <- mutate(topredict,points_grouped=NA)

data4 <- rbind(data,topredict) 


rfdata <- filter(data4,round < week)
rfdata <- filter(data4,round >20)

rf1 <- randomForestSRC::rfsrc(total_points ~ playername + opponent_team + was_home + form + minutes +  opponent_team:team, data = rfdata, 
                             na.action = "na.omit",ntree=2000,mtry=5,nodesize=9, importance = TRUE,xvar.wt = c(1,1,1,1.5,1,1))


predset <- filter(data4,round >= week)
  
prediction <-predict.rfsrc(rf1, predset,na.action = "na.impute")
prediction            

predset$predictedpoints <- prediction$predicted




######## 10 game prediction

fixtures <- get_game_list()
thisweek <- filter(fixtures,GW > max(hist$round) , GW < max(hist$round)+11)

week <- max(hist$round)+39

thisweekfixtures <- rbind(
  thisweek %>% mutate(team=team_h,oppo=team_a,was_home="TRUE"),
  thisweek %>% mutate(team=team_a,oppo=team_h,was_home="FALSE"))

thisweekfixtures <- select(thisweekfixtures,round=GW,team,opponent_team=oppo,was_home)
thisweekfixtures$round <- thisweekfixtures$round + 38

thisweekfixtures$was_home <- as.factor(thisweekfixtures$was_home)

thisweekfixtures$team <- as.factor(thisweekfixtures$team)
levels(thisweekfixtures$team) <- as.factor(teams$teams)

thisweekfixtures$opponent_team <- as.factor(thisweekfixtures$opponent_team)
levels(thisweekfixtures$opponent_team) <- as.factor(teams$teams)

thisweekfixtures$team <- as.character(thisweekfixtures$team)
thisweekfixtures$opponent_team <- as.character(thisweekfixtures$opponent_team)


players <- data %>% filter(round==max(data$round)) %>% select(team,playername,form)
injurystatus <- get_player_info() %>% select(playername,status)
players <- full_join(players,injurystatus)
players$team <- as.character(players$team)

data3 <- full_join(thisweekfixtures,players)
data3$team <- as.factor(data3$team)
data3$playername <- as.factor(data3$playername)

data3 <- mutate(data3,minutes = if_else(status=="a",90,if_else(status=="d",60,0)))
data3 <- mutate(data3, total_points=NA)

data3$total_points <- as.numeric(data3$total_points)
data3$opponent_team <- as.factor(data3$opponent_team)

topredict <- select(data3, playername , team, opponent_team , was_home, form, minutes, total_points,round)
topredict <- mutate(topredict,weights=2)
topredict <- mutate(topredict,points_grouped=NA)

data4 <- rbind(data,topredict) 


rf1 <- randomForestSRC::rfsrc(total_points ~ playername + opponent_team + was_home + form + minutes +  opponent_team:team, data = rfdata, 
                              na.action = "na.omit",ntree=2000,mtry=5,nodesize=9, importance = TRUE,xvar.wt = c(1,1,1,1.5,1,1))


predset <- filter(data4,round >= week)

prediction <-predict.rfsrc(rf1, predset,na.action = "na.impute")
prediction            

predset$predictedpoints <- prediction$predicted



seasonpred <- group_by(predset,playername) %>% summarise(seasonpoints = sum(predictedpoints))

predseason <- left_join(predset,seasonpred)
predseason <- get_player_info() %>% select(playername,now_cost,element_type) %>% left_join(predseason) %>% 
  select(playername,now_cost,element_type,team) %>% left_join(seasonpred) %>% distinct()

predseason <- mutate(predseason,now_cost=now_cost/10)
predseason <- mutate(predseason,seasonpoints=if_else(is.na(seasonpoints)==TRUE,0,seasonpoints))


write.csv(predseason,"10matchprediction.csv")


### linear optimisation

#Create the constraints
num_gk = 2
num_def = 5
num_mid = 5
num_fwd = 3
max_cost = 100
# Create vectors to constrain by position
predseason$Goalkeeper = ifelse(predseason$element_type == 1, 1, 0)
predseason$Defender = ifelse(predseason$element_type == 2, 1, 0)
predseason$Midfielder = ifelse(predseason$element_type == 3, 1, 0)
predseason$Forward = ifelse(predseason$element_type == 4, 1, 0)
# Create vector to constrain by max number of players allowed per team
team_constraint = unlist(lapply(unique(predseason$team), function(x, predseason){
  ifelse(predseason$team==x, 1, 0)
}, predseason=predseason))

# next we need the constraint directions
const_dir <- c("=", "=", "=", "=", rep("<=", 21))

objective = round(predseason$seasonpoints,2)

# Put the complete matrix together
const_mat = matrix(c(predseason$Goalkeeper, predseason$Defender, predseason$Midfielder, predseason$Forward,
                     predseason$now_cost, team_constraint),
                   nrow=(5 + length(unique(predseason$team))),
                   byrow=TRUE)
const_rhs = c(num_gk, num_def, num_mid, num_fwd, max_cost, rep(3, 21))
# And solve the linear system
x = lp ("max", objective, const_mat, const_dir, const_rhs, all.bin=TRUE, all.int=TRUE)
print(arrange(predseason[which(x$solution==1),], desc(Goalkeeper), desc(Defender), desc(Midfielder), desc(Forward), desc(seasonpoints)))



### linear optimisation for frst 11 only

#Create the constraints
num_gk = 1
min_def = 3
max_def = 5
min_mid = 3
max_mid = 5
min_fwd = 2
max_fwd = 3
max_cost = 84
max_players = 11
# Create vectors to constrain by position
predseason$Goalkeeper = ifelse(predseason$element_type == 1, 1, 0)
predseason$Defender = ifelse(predseason$element_type == 2, 1, 0)
predseason$Midfielder = ifelse(predseason$element_type == 3, 1, 0)
predseason$Forward = ifelse(predseason$element_type == 4, 1, 0)
predseason$players = 1
# Create vector to constrain by max number of players allowed per team
team_constraint = unlist(lapply(unique(predseason$team), function(x, predseason){
  ifelse(predseason$team==x, 1, 0)
}, predseason=predseason))

# next we need the constraint directions
const_dir <- c("=", ">=", "<=",">=", "<=", ">=", "<=", "<=","=", rep("<=", 21))

objective = round(predseason$seasonpoints,2)

# Put the complete matrix together
const_mat = matrix(c(predseason$Goalkeeper, predseason$Defender, predseason$Defender, predseason$Midfielder, predseason$Midfielder,
                     predseason$Forward,predseason$Forward,
                     predseason$now_cost,predseason$players, team_constraint),
                   nrow=(9 + length(unique(predseason$team))),
                   byrow=TRUE)
const_rhs = c(num_gk, min_def, max_def, min_mid, max_mid, min_fwd,max_fwd, max_cost,max_players, rep(3, 21))
# And solve the linear system
x = lp ("max", objective, const_mat, const_dir, const_rhs, all.bin=TRUE, all.int=TRUE)
print(arrange(predseason[which(x$solution==1),], desc(Goalkeeper), desc(Defender), desc(Midfielder), desc(Forward), desc(seasonpoints)))


##################################################################################

#############

### COMPUTER NOT POWERFUL ENOUGH TO FIT THIS MODEL

### specify formula 
##f <- total_points | weights(weights) ~ 
##  playername + opponent_team + was_home + minutes
##
##get_prior(f,data)
##
### specify priors
##prior_rec <- get_prior(f, data)
##prior <- c(
##  brms::set_prior("normal(0, 0.5)", class = "b"),
##  set_prior("student_t(3, 1, 5)", class = "Intercept"),
##  set_prior("student_t(3, 0, 5)", class = "sigma"),
##  set_prior("student_t(3, 0, 5)", class = "sd")
##)
##
##
##data1  <- filter(data,round>=35)
##
### train model - RUN OUTSIDE OF R PROJECT
##m_gw8 <- brms::brm(
##  f,
##  data,
##  family = gaussian(),
##  chains = 1
##)
##
### meanfield check
##m_gw8 <- brms::brm(
##  f,
##  data,
##  family = gaussian(),
##  prior = prior,
##  algorithm = "meanfield"
##)
##
##
##
##saveRDS(m_gw7, file = "main_model_gw7.rds")
##
##
##
##
##
##overtime <- fitted(m_gw6, newdata = fpl_all)
##overtime <- cbind(overtime, fpl_all)
##
##overtime_mané <- overtime %>%
##  filter(player == "Sadio Mané")
##overtime_son <- overtime %>%
##  filter(player == "Heung-Min Son")
##overtime_salah <- overtime %>%
##  filter(player == "Mohamed Salah")
##
