library(trueskill)
library(bindr)
library(dplyr)

matches_with_mex <- read_csv("/Users/macbook/R/causalperception/pp2_mexico.csv")
matches_with_mex2 <- read_csv("/Users/macbook/R/causalperception/pp2_mexico2.csv")
all_matches_with_mex <- rbind(matches_with_mex,matches_with_mex2)

all_matches_with_mex$match_id = row.names(all_matches_with_mex)

all_matches_with_mex <- all_matches_with_mex[c('left_id','right_id','winner','match_id','category')]
all_matches_with_mex$category <-  as.factor(all_matches_with_mex$category)
colnames(all_matches_with_mex) <- c('Player', 'Opponent','winner','match_id','Round')

all_matches_with_mex = all_matches_with_mex %>% filter(winner!='equal')

wiwi <- function(x){ifelse(x=='left',1,-1)}

all_mutate <- all_matches_with_mex %>% mutate(win = wiwi(winner))
all_matches_with_mex <- all_mutate[-3]
all_matches_with_mex <- all_matches_with_mex[c("Player","Opponent","Round","match_id","win")]

all_matches_with_mex_2 <- cbind(all_matches_with_mex[c("Opponent","Player","Round","match_id")],
                                sapply(all_matches_with_mex$win, function(x){-x}))
names(all_matches_with_mex_2) <- names(all_matches_with_mex)

all_data_mex <- rbind(all_matches_with_mex,all_matches_with_mex_2) 
all_data_mex <- all_data_mex[c("Round","Player","Opponent","match_id","win")]
names(all_data_mex) <- c(names(all_data_mex[,-5]),"margin")

all_data_mex$mu1 <- NA
all_data_mex$mu2 <- NA
all_data_mex$sigma1 <- NA
all_data_mex$sigma2 <- NA

PlayerRanks <- data.frame(cbind(unique(all_data_mex$Player),1:length(unique(all_data_mex$Player))))
colnames(PlayerRanks) <- c('Player',"player_rank")
PlayerRanks <- data.frame(PlayerRanks$Player,PlayerRanks$Player,PlayerRanks$player_rank)
colnames(PlayerRanks) <- c('Player','Opponent',"player_rank")

all_data <- all_data_mex %>% left_join(PlayerRanks[c('Player','player_rank')], by = "Player")
all_data <- all_data %>% left_join(PlayerRanks[c('Opponent','player_rank')], by = "Opponent")
names(all_data) <- c(names(all_data)[1:9],'WRank', 'LRank')
all_data <- all_data[c(1:3,10,11,4:9)]

parameters <- Parameters()

mexdata <- Trueskill(all_data_mex, parameters)
