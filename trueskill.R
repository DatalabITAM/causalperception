library(trueskill) 
library(readr)
library(bindr)
library(dplyr)

matches_with_mex <- read_csv("/Users/macbook/R/causalperception/pp2_mexico.csv")
matches_with_mex2 <- read_csv("/Users/macbook/R/causalperception/pp2_mexico2.csv")

all_matches_with_mex <- rbind(matches_with_mex,matches_with_mex2)


full_data <- all_matches_with_mex[3:10]

full_data <- mutate(full_data, match_id = paste(left_id, right_id))

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

all_data$Lrank <- 1
all_data$Rrank <- 1

all_data <- all_data[which(all_data$winner != "equal"),]

all_data$t <- NULL
all_data$mu1 <- NA
all_data$sigma1 <- NA
all_data$mu2 <- NA
all_data$sigma2 <- NA

colnames(all_data)[4] <- "Round"


# For the first round, set Mu to 300 less the ATP rank
# Skill tends to be stable at the higher rankings (descending from 1), 
# so set sigma at mu less mu / 3, rather than the recommended mu / 3

# data[c("mu1","sigma1")] <- c(300 - data$WRank, 
#                              round(300 - data$WRank - ((300 - data$WRank) / 3), 1))
# data[c("mu2","sigma2")] <- c(300 - data$LRank, 
#                              round(300 - data$LRank - ((300 - data$WRank) / 3), 1)) 
# 
# data[!data$Round == "1st Round",][c("mu1","sigma1")] <- c(NA, NA)
# data[!data$Round == "1st Round",][c("mu2","sigma2")] <- c(NA, NA)

all_data[c("mu1","sigma1")] <- c(all_data$Lrank, round(all_data$Lrank/3,1))
all_data[c("mu2","sigma2")] <- c(all_data$Rrank, round(all_data$Rrank/3,1))

parameters <- Parameters()
all_data$Round <-  as.factor(all_data$Round)

# Trueskill expects data with columns mu1, sigma1, mu2 and sigma2, 
# will set mu and sigma to 25 and 25 / 3 if NA.

all_data <- Trueskill(all_data, parameters)

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