#if (!require(remotes)) {
#  install.packages("remotes") 
#}
#
#remotes::install_github("wiscostret/fplscrapR")

library(fplscrapR)
library(tidyverse)

help(package = fplscrapR)

############## players with most points per 90 ############
df <- get_player_info(season=18)

df <- df %>% 
  filter(total_points >= 30) %>% # filtering for players with more than 30 points scored
  mutate(pp90 = total_points / minutes * 90) %>% # creating the 'pp90' variable
  select(playername,pp90) %>% # selecting player name and pp90 for our table
  arrange(-pp90) %>% # ordering (arranging) by pp90
  slice(1:20) # showing just the top20
df

ggplot(df,aes(reorder(playername,pp90),pp90),pp90) + geom_col() + coord_flip() +
  xlab("Points per 90 minutes") +
  ylab("Player")

############## get my league ####################
get_league(leagueid = 419768, leaguetype = "classic")

############## my expected points ##############
picks <- get_entry_picks(entryid=5417688,gw=3)$picks

df <- get_player_info() %>% 
  select(id,playername,ep_next) %>% 
  mutate("element"=id)

df$ep_next <- as.numeric(df$ep_next)


df2 <- left_join(picks,df,by="element") %>% select(playername,is_captain,is_vice_captain,ep_next)

df2


########## most generous teams ###############
df <- get_player_details(season=18) # this may take a while to load as it fetches ALL player details

df %>% 
  filter(round %in% 1:25) %>% # filter out the gameweeks we are interested in
  group_by(opponent_team) %>% # transformation to group and summarize the total_points (scored by a given player in a given round) by the opponent ('opponent_team') variable level
  summarize(sum(total_points)) %>% 
  ggplot(aes(x=reorder(opponent_team,`sum(total_points)`),y=`sum(total_points)`,fill=factor(opponent_team))) + 
  geom_col(colour="black") +
  theme_bw() +
  coord_flip() +
  scale_x_discrete(labels=c("1"="ARS","2"="BOU","3"="BHA","4"="BUR","5"="CAR","6"="CHE","7"="CRY","8"="EVE","9"="FUL","10"="HUD","11"="LEI","12"="LIV","13"="MCI","14"="MUN","15"="NEW","16"="SOU","17"="TOT","18"="WAT","19"="WHU","20"="WOL")) + # here we transform the numbered team labels to the FPL short letter names
  labs(x="Team",y="Total FPL points allowed",title="Alternative FDR ranking - by total FPL points allowed",subtitle="Season 2018/2019 - Gameweeks 1-25",caption=paste("Data from fplscrapR | ",Sys.Date(),sep="")) +
  scale_fill_manual(values=c("1"="#EF0107","2"="#000000","3"="#0057B8","4"="#6C1D45","5"="#0070B5","6"="#034694","7"="#1B458F","8"="#003399","9"="#FFFFFF","10"="#0E63AD","11"="#003090","12"="#C8102E","13"="#6CABDD","14"="#DA291C","15"="#241F20","16"="#D71920","17"="#132257","18"="#FBEE23","19"="#7A263A","20"="#FDB913"),guide=F) # here we add the team-specific colours to the plot

######## fixture difficulty ##############

df <- get_player_details(season = 18) # this may take a while to load as it fetches ALL player details

afdrranks <- df %>% 
  filter(round %in% 1:25) %>% # filtering for the gameweeks we are interested in.
  group_by(opponent_team) %>% # transformation to group and summarize the total_points (scored by a given player in a given round) by the opponent ('opponent_team') variable level
  summarize(sum(total_points)) %>% 
  arrange(`sum(total_points)`) %>% # ordering (arranging) by total_points allowed
  mutate(oppo=recode(opponent_team,"1"="ARS","2"="BOU","3"="BHA","4"="BUR","5"="CAR","6"="CHE","7"="CRY","8"="EVE","9"="FUL","10"="HUD","11"="LEI","12"="LIV","13"="MCI","14"="MUN","15"="NEW","16"="SOU","17"="TOT","18"="WAT","19"="WHU","20"="WOL")) # here we transform the numbered team labels to the FPL short letter names



gamelist <- get_game_list(season = 18)

afdrfixtures <- rbind(
  gamelist %>% mutate(team=home,oppo=away,homeaway="home"),
  gamelist %>% mutate(team=away,oppo=tolower(home),homeaway="away"))

for (i in 1:nrow(afdrfixtures)){
  afdrfixtures$afdr[i] <- afdrranks$`sum(total_points)`[which(afdrranks$oppo==toupper(afdrfixtures$oppo[i]))]
}


afdrfixtures %>% 
  filter(GW %in% 1:25) %>% # filtering for the gameweeks we are interested in 
  ggplot() +
  geom_tile(aes(x=GW,y=team,fill=afdr),colour="lightgrey") +
  geom_text(aes(x=GW,y=team,label=oppo),size=2) +
  theme_void() +
  theme(axis.text = element_text(face = "bold")) +
  theme(axis.text.y = element_text(margin=margin(0,-20,0,0))) + # fixing the margins
  scale_x_continuous(position="top",breaks=1:25) +
  labs(caption=paste("Data from fplscrapR | ",Sys.Date(),sep="")) +
  scale_fill_gradient2(guide=F,low="#7F002D",mid="#D6DCD8",high="#00FF87",midpoint=median(afdrfixtures$afdr)) # creating a gradient colour-coding that spans from lowest aFDR ranking (coloured red) to highest (coloured green)


###### key players by team ###########

gamelist <- get_game_list(season = 18)


df2 <- 
  df %>% 
  mutate( # creating the 'non-appearance points' variable based on goals, assists, saves, penalty saves, and bonus points, with the exact scoring diferent for each position (element_type)
    napts = case_when(
      element_type == 1 ~ round(goals_scored * 6 + assists * 3 + saves/3 + penalties_saved*6 + bonus,0),
      element_type == 2 ~ goals_scored * 6 + assists * 3 + bonus,
      element_type == 3 ~ goals_scored * 5 + assists * 3 + bonus,
      element_type == 4 ~ goals_scored * 4 + assists * 3 + bonus)) %>% 
  group_by(team) %>% 
  mutate(teamtotal = sum(napts)) %>% # summarising team totals
  mutate(naptsprop = round(napts/teamtotal,2)) %>% # calculating the player proportion of the team total (and rounding) 
  top_n(n=1) # selecting the top player from each team

df2 %>% 
  ggplot() + 
  geom_col(aes(x=reorder(second_name,naptsprop),y=naptsprop)) +
  theme_bw() +
  coord_flip() +
  labs(title="Who Got The Assists?'s Talisman Theory", x="Player",y="Proportion of team's non-appearance points scored",caption=paste("Data from fplscrapR | ",Sys.Date(),sep=""))

