library(trueskill) 
library(readr)
library(bindr)
library(dplyr)

matches_with_mex <- read_csv("/Users/rodolfoocampo/Documents/Datalab/Proyectos/Causal change/Data/pp2_mexico.csv")
matches_with_mex2 <- read_csv("/Users/rodolfoocampo/Documents/Datalab/Proyectos/Causal change/Data/pp2_mexico2.csv")

all_matches_with_mex <- rbind(matches_with_mex,matches_with_mex2)


full_data <- all_matches_with_mex[3:10]

full_data <- mutate(full_data, match_id = paste(left_lat, left_long, right_lat, right_long))

left <- full_data$left_id

full_data2 <- full_data

full_data2$left_id <- full_data$right_id 
full_data2$right_id <- left

left_winners <- full_data2$winner == "left"
right_winners <- full_data2$winner == "right"

full_data2$winner[left_winners] <- "right"
full_data2$winner[right_winners] <- "left"

all_data <- rbind(full_data,full_data2)

all_data <- all_data[c(-4,-5,-6,-7)]

all_data$margin[all_data$winner == "left"] <- 1
all_data$margin[all_data$winner != "left"] <- -1

data("ausopen2012")
data <- data[c("Winner", "Loser", "Round", "WRank", "LRank")]
data <- reshape(data,
                idvar = "match_id",
                varying = list(c(1, 2), c(2, 1), c(4, 5), c(5,4)),
                v.names = c("Player", "Opponent", "WRank", "LRank"),
                new.row.names = 1:1000, 
                timevar = "t",
                direction = "long")



# data comes preformatted with winner in Player column
# set margin to 1 for win and -1 for loss.

data$margin[data$t == "1"] <- 1
data$margin[data$t != "1"] <- -1
data$t <- NULL

data$mu1 <- NA
data$sigma1 <- NA
data$mu2 <- NA
data$sigma2 <- NA

# For the first round, set Mu to 300 less the ATP rank
# Skill tends to be stable at the higher rankings (descending from 1), 
# so set sigma at mu less mu / 3, rather than the recommended mu / 3

data[c("mu1","sigma1")] <- c(300 - data$WRank, 
                             round(300 - data$WRank - ((300 - data$WRank) / 3), 1))
data[c("mu2","sigma2")] <- c(300 - data$LRank, 
                             round(300 - data$LRank - ((300 - data$WRank) / 3), 1)) 

data[!data$Round == "1st Round",][c("mu1","sigma1")] <- c(NA, NA)
data[!data$Round == "1st Round",][c("mu2","sigma2")] <- c(NA, NA)

parameters <- Parameters()

# Trueskill expects data with columns mu1, sigma1, mu2 and sigma2, 
# will set mu and sigma to 25 and 25 / 3 if NA.

# data <- Trueskill(data, parameters)
# top4 <- subset(data, Player == "Djokovic N." | Player == "Nadal R." | 
#                      Player == "Federer R." | Player == "Murray A." )
# top4 <- top4[order(top4$Player,top4$Round),]

# subset(top4, Player == "Djokovic N.")      

# For a visualisation, load up our favourite package ggplot2...  
# library(ggplot2)
# g1 <- ggplot(top4, aes(x = Round, y = mu1, group = Player, colour = Player)) + 
# geom_point(aes(colour=factor(Player))) + geom_line(aes())       
# g1

# Without having adjusted the input parameters, Trueskill does not predict 
# match outcomes well, as it appears that facing stiffer opposition
# (higher skilled players) tends to diminish a player's chances of
# progressing in the subsequent round.

# This is consistent with commentators describing players with softer draws and 
# playing shorter matches (3 sets as opposed to 5 sets) as being 
# fresher in later rounds.          

# The other feature is that the skill of the better players is weighted 
# towards the losing player even if the better player wins, so we have
# this effect of the 4 semifinalists having their skills dropping as the 
# tournament progresses. This could be symptomatic of high starting values,
# which is necessary due to some of the very low rankings. 
# E.g Lleyton Hewitt with 181.