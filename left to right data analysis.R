rm(list = ls())
library(tidyverse)


## ------------------- import data --------------------##
setwd("C:/Users/Joshua Engels/Desktop/A-Voting-Folder/data")
votes <- read_delim("race_summary.csv",col_names=TRUE, delim=",")


## ------------------- add columns --------------------##

# Add a new binary column whether this race was skipped or not
votes$yesVoted <- votes$votedOn >= 0

# Add new bins
votes$bins <- floor(votes$yPos / 10)


## ------------------ calculate values to graph ------##

averageError = mean(votes$yesVoted)

# Eror rate by various spacing variables
raceSpaceError <- votes %>% group_by(raceSpace) %>% summarise(
  percentVotedOn = mean(yesVoted)
)
titleSpaceError <- votes %>% group_by(titleSpace) %>% summarise(
  percentVotedOn = mean(yesVoted)
)
candidateSpaceError <- votes %>% group_by(candidateSpace) %>% summarise(
  percentVotedOn = mean(yesVoted)
)

# Error rate by column
columnError <- votes %>% group_by(column) %>% summarise(
  percentVotedOn = mean(yesVoted)
)

# Error rate by y column bin
yError <- votes %>% group_by(bins) %>% summarise(
  percentVotedOn = mean(yesVoted))

## --------------------- show graphs -------------- ##

ggplot(
  raceSpaceError,
  aes(x=raceSpace, y=1 - percentVotedOn)) + geom_point() + 
  theme_minimal() + 
  labs(title="Voting Error by Space Between Races", 
       x="Pixels Between Races", 
       y = "Percent Error") +
  geom_hline(yintercept=1 - averageError, linetype="dashed", color = "red") +
  geom_smooth(method='lm') +
  coord_cartesian(ylim=c(0,0.25))


# Y position start frequency
ggplot(votes, aes(bins * 10)) +
  geom_bar(width = 8, fill="steelblue") +
  theme_minimal() + 
  labs(title="Y Position Histogram", 
       x="Y Position (bin size = 10 pixels, from the top)", 
       y = "Number of Races")

# Error rate by y position
ggplot(
  yError,
  aes(x=bins * 10, y = 1 - percentVotedOn)) +
  geom_bar(stat="identity", fill="steelblue", width=8) +
  theme_minimal() + 
  labs(title="Voting Error by Y Position (pixels from screen top)", 
       x="Y Position", 
       y = "Percent Error")