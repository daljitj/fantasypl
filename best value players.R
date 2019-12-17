library(fplscrapR)
library(tidyverse)
library(ggrepel)
library(ggthemes)


#help(package = fplscrapR)


######## get player info ######################
df <- get_player_info()

#### create player cost variable ############
df <- mutate(df, cost = df$now_cost/10) 

############## calculate points per 90 ############

df <- df %>% 
  mutate(pp90 = total_points / minutes * 90) # creating the 'pp90' variable


############### filterby minutes played ####################

df1 <- df %>% filter(minutes >= 420)  # filtering for players with 420 or more minutes played
  

######## plot cost against pp90 ##############

ggplot(df1,aes(x=cost,y=pp90,label=playername,colour=factor(element_type,
              labels= c("Goalkeeper","Defender","Midfielder","Attacker")))) + geom_point() +
  geom_text_repel(data=subset(df1, pp90 > 5 | cost > 6.5),
                  aes(cost,pp90)) +
  labs(color = "Position",title="Best value players to GW8",subtitle = "Players with more than 420 minutes played") +
  ylab("Points per 90 minutes") +
  xlab("Player cost") +
  geom_smooth(method = "lm",aes(colour = NA),se=FALSE,color="black") +
  theme_classic()


############### filter by total points ###################

df2 <- df %>% filter(total_points >= 30) #filtering for players with more than 35 points


######## plot cost against total points ##############

ggplot(df2,aes(x=cost,y=total_points,label=playername,colour=factor(element_type,
                                                           labels= c("Goalkeeper","Defender","Midfielder","Attacker")))) + geom_point() +
  geom_text_repel(data=subset(df1, total_points > 50 | cost > 6.5),
                  aes(cost,total_points,colour=factor(element_type,
                                                      labels= c("Goalkeeper","Defender","Midfielder","Attacker")))) +
  labs(color = "Position",title="Highest scoring players against player cost to GW8",subtitle = "Players with more than 30 points only") +
  ylab("Total points") +
  xlab("Player cost") +
  geom_smooth(method = "lm",aes(colour = NA),se=FALSE,color="black") +
  theme_classic()




########### plot points against pp90 #################


ggplot(df1,aes(x=pp90,y=total_points,label=playername,colour=factor(element_type,
                                                                    labels= c("Goalkeeper","Defender","Midfielder","Attacker")))) + geom_point() +
  geom_text_repel(data=subset(df1, total_points > 40 | pp90 > 5),
                  aes(pp90,total_points)) +
  labs(color = "Position",title="Highest scoring players against points per 90",subtitle = "") +
  ylab("Total points") +
  xlab("Points per 90 minutes") +
  geom_smooth(method = "lm",aes(colour = NA),se=FALSE,color="black") +
  theme_classic()



########## playing time viz ####

timeviz <- get_player_details()


timeviz %>% 
  filter(element %in% 181:201) %>%  # selecting the 'elements' (players) from Liverpool
  ggplot() +
  geom_tile(aes(x=round,y=reorder(playername,-element),fill=minutes)) +
  theme_bw() +
  scale_x_continuous(position="top") +
  labs(x="Gameweek",y="Player",caption=paste("Data from fplscrapR | ",Sys.Date(),sep="")) +
  scale_fill_gradient2(guide=F,low="#FFFFFF",high="#132257") # filling each tile based on the 'minutes' value, from 'low' (white) to 'high'
