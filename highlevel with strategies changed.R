rm(list = ls())
#install.packages("stringi")
require(plyr)
require(ggplot2)
library(dplyr)
library(stringr)

#---------------------- high activations ----------------------
## ------------------- import data --------------------##
setwd("C:/Users/Joshua Engels/Desktop/A-Voting-Folder/data")
votes <- read.delim("2019-04-10.tsv",sep="\t")

votes <- data.frame(lapply(votes, function(x) {gsub("VG-SERIAL-RETRIEVE-RECOGNIZE-PARTY", "Serial_Retrieve-Recognize-Party_VG", x)}))
votes <- data.frame(lapply(votes, function(x) {gsub("VG-RANDOM-RETRIEVE-RECOGNIZE-PARTY", "Random_Retrieve-Recognize-Party_VG", x)}))

votes <- data.frame(lapply(votes, function(x) {gsub("VG-RANDOM-RETRIEVEPARTY-PARTY", "Random_Party_VG", x)}))
votes <- data.frame(lapply(votes, function(x) {gsub("VG-SERIAL-RETRIEVEPARTY-PARTY", "Serial_Party_VG", x)}))

votes <- data.frame(lapply(votes, function(x) {gsub("VG-RANDOM-RECOGNIZE-PARTY", "Random_Recognize-Party_VG", x)}))
votes <- data.frame(lapply(votes, function(x) {gsub("VG-SERIAL-RECOGNIZE-PARTY", "Serial_Recognize-Party_VG", x)}))

votes <- data.frame(lapply(votes, function(x) {gsub("VG-RANDOM-RETRIEVE-PARTY", "Random_Retrieve-Party_VG", x)}))
votes <- data.frame(lapply(votes, function(x) {gsub("VG-SERIAL-RETRIEVE-PARTY", "Serial_Retrieve-Party_VG", x)}))




## -------------------analyze data --------------------##
votes$search = sapply(strsplit(as.character(votes$model),"_",fixed=TRUE),"[[",1)
votes$strat = sapply(strsplit(as.character(votes$model),"_",fixed=TRUE),"[[",2)


desired = c("GordonBearce","CoreyDery","RobertMettler","RickStickles","CassiePrincipe",
            "RickOrgan","ThereseGustin","EliseEllzey","PollyRylander","JillianBalas",
            "WesleyStevenMillette","SusanneRael","PeterVarga","TimGrasty","DerrickMelgar",
            "CoreyBehnke","DeanCaffee","JasonValle","HowardGrady","ClydeGaytonJr",
            "LewisShine", "NIL", "NIL")

votes$votenames = as.character(votes$data)
votes$votenames = unlist(sapply(strsplit(votes$votenames,") (", fixed = TRUE), "[[", 1))

##cleaning the text
votes$votenames = gsub(")", "", votes$votenames, fixed = TRUE)
votes$votenames = gsub("(", "", votes$votenames, fixed = TRUE)
votes$votenames = gsub("\"", "", votes$votenames, fixed = TRUE)

votes$votenames = strsplit(votes$votenames," ")

votes$R01 = ifelse(sapply(votes$votenames, "[[", 1) == desired[1], 1, 0)
votes$R02 = ifelse(sapply(votes$votenames, "[[", 2) == desired[2], 1, 0)
votes$R03 = ifelse(sapply(votes$votenames, "[[", 3) == desired[3], 1, 0)
votes$R04 = ifelse(sapply(votes$votenames, "[[", 4) == desired[4], 1, 0)
votes$R05 = ifelse(sapply(votes$votenames, "[[", 5) == desired[5], 1, 0)
votes$R06 = ifelse(sapply(votes$votenames, "[[", 6) == desired[6], 1, 0)
votes$R07 = ifelse(sapply(votes$votenames, "[[", 7) == desired[7], 1, 0)
votes$R08 = ifelse(sapply(votes$votenames, "[[", 8) == desired[8], 1, 0)
votes$R09 = ifelse(sapply(votes$votenames, "[[", 9) == desired[9], 1, 0)
votes$R10 = ifelse(sapply(votes$votenames, "[[", 10) == desired[10], 1, 0)
votes$R11 = ifelse(sapply(votes$votenames, "[[", 11) == desired[11], 1, 0)
votes$R12 = ifelse(sapply(votes$votenames, "[[", 12) == desired[12], 1, 0)
votes$R13 = ifelse(sapply(votes$votenames, "[[", 13) == desired[13], 1, 0)
votes$R14 = ifelse(sapply(votes$votenames, "[[", 14) == desired[14], 1, 0)
votes$R15 = ifelse(sapply(votes$votenames, "[[", 15) == desired[15], 1, 0)
votes$R16 = ifelse(sapply(votes$votenames, "[[", 16) == desired[16], 1, 0)
votes$R17 = ifelse(sapply(votes$votenames, "[[", 17) == desired[17], 1, 0)
votes$R18 = ifelse(sapply(votes$votenames, "[[", 18) == desired[18], 1, 0)
votes$R19 = ifelse(sapply(votes$votenames, "[[", 19) == desired[19], 1, 0)
votes$R20 = ifelse(sapply(votes$votenames, "[[", 20) == desired[20], 1, 0)
votes$R21 = ifelse(sapply(votes$votenames, "[[", 21) == desired[21], 1, 0)

votes$correct = with(votes, 
                     R01 + R02 + R03 + R04 + R05 + R06 + R07 + 
                       R08 + R09 + R10 + R11 + R12 + R13 + R14 + 
                       R15 + R16 + R17 + R18 + R19 + R20 + R21)

##count errors with different strategies 
votes$votestrats = as.character(votes$data)
votes$votestrats = unlist(sapply(strsplit(votes$votestrats,") (", fixed = TRUE), "[[", 3))
votes$votestrats = gsub(")", "", votes$votestrats, fixed = TRUE)
votes$votestrats = gsub("\"", "", votes$votestrats, fixed = TRUE)

votes$votestrats = strsplit(votes$votestrats," ")

votes$S01 = ifelse(votes$R01 == 1, 1, sapply(votes$votestrats, "[[", 1))
votes$S02 = ifelse(votes$R02 == 1, 1, sapply(votes$votestrats, "[[", 2))
votes$S03 = ifelse(votes$R03 == 1, 1, sapply(votes$votestrats, "[[", 3))
votes$S04 = ifelse(votes$R04 == 1, 1, sapply(votes$votestrats, "[[", 4))
votes$S05 = ifelse(votes$R05 == 1, 1, sapply(votes$votestrats, "[[", 5))
votes$S06 = ifelse(votes$R06 == 1, 1, sapply(votes$votestrats, "[[", 6))
votes$S07 = ifelse(votes$R07 == 1, 1, sapply(votes$votestrats, "[[", 7))
votes$S08 = ifelse(votes$R08 == 1, 1, sapply(votes$votestrats, "[[", 8))
votes$S09 = ifelse(votes$R09 == 1, 1, sapply(votes$votestrats, "[[", 9))
votes$S10 = ifelse(votes$R10 == 1, 1, sapply(votes$votestrats, "[[", 10))
votes$S11 = ifelse(votes$R11 == 1, 1, sapply(votes$votestrats, "[[", 11))
votes$S12 = ifelse(votes$R12 == 1, 1, sapply(votes$votestrats, "[[", 12))
votes$S13 = ifelse(votes$R13 == 1, 1, sapply(votes$votestrats, "[[", 13))
votes$S14 = ifelse(votes$R14 == 1, 1, sapply(votes$votestrats, "[[", 14))
votes$S15 = ifelse(votes$R15 == 1, 1, sapply(votes$votestrats, "[[", 15))
votes$S16 = ifelse(votes$R16 == 1, 1, sapply(votes$votestrats, "[[", 16))
votes$S17 = ifelse(votes$R17 == 1, 1, sapply(votes$votestrats, "[[", 17))
votes$S18 = ifelse(votes$R18 == 1, 1, sapply(votes$votestrats, "[[", 18))
votes$S19 = ifelse(votes$R19 == 1, 1, sapply(votes$votestrats, "[[", 19))
votes$S20 = ifelse(votes$R20 == 1, 1, sapply(votes$votestrats, "[[", 20))
votes$S21 = ifelse(votes$R21 == 1, 1, sapply(votes$votestrats, "[[", 21))

##two situations
#-----------------1. no abstained races(ALL-PERFECT, ALL-ROLLOFF & FULL-DM) ------------------
votes_ALLROLLOFF <- subset(votes, dm =="ALL-ROLLOFF")
votes_ALLPERFECT <- subset(votes, dm == "ALL-PERFECT")
votes_FULLDM <- subset(votes, dm == "FULL-DM")

votes1 <- rbind(votes_ALLROLLOFF,votes_ALLPERFECT,votes_FULLDM)

votes1$acc = votes1$correct / 21
votes1$err = 1 - votes1$acc

votes1$retrieval <- rowSums(votes1 == "RETRIEVAL")
votes1$recognition <- rowSums(votes1 == "RECOGNITION")
votes1$party <- rowSums(votes1 == "PARTY")

votes1$err_retrieval <- votes1$retrieval / 21
votes1$err_recognition <- votes1$recognition / 21
votes1$err_party <- votes1$party / 21

#-----------------------2. abstained races(MOST-PERFECT & MOST-ROLLOFF) -----------------------
votes_MOSTROLLOFF <- subset(votes, dm =="MOST-ROLLOFF")
votes_MOSTPERFECT <- subset(votes, dm == "MOST-PERFECT")
votes2 <- rbind(votes_MOSTROLLOFF, votes_MOSTPERFECT)
votes2[50:55] <- list(NULL)

votes2$correct = with(votes2, 
                     R01 + R02 + R03 + R04 + R05 + R06 + R07 + 
                       R08 + R09 + R10 + R11 + R12 + R13 + R14 + 
                       R15)

votes2$acc = votes2$correct / 15
votes2$err = 1 - votes2$acc

votes2$retrieval <- rowSums(votes2 == "RETRIEVAL")
votes2$recognition <- rowSums(votes2 == "RECOGNITION")
votes2$party <- rowSums(votes2 == "PARTY")

votes2$err_retrieval <- votes2$retrieval / 15
votes2$err_recognition <- votes2$recognition / 15
votes2$err_party <- votes2$party / 15

## get the full dataset (S01 to S21 removed)

votes1[35:55] <- list(NULL)
votes2[35:49] <- list(NULL)
total<- rbind(votes1,votes2)

## mean error rate
ave = mean(total$err)
total_FULL <- subset(total, dm =="FULL-DM")
ave_FULL = mean(total_FULL$err)
#total_ALLROLLOFF <- subset(total, dm =="ALL-ROLLOFF")
#ave_ALLROLLOFF = mean(total_ALLROLLOFF$err)

## re-organize the dataset to get stacked plots

retri<- total[ ,!(colnames(total) %in% c("err_recognition", "err_party"))]
recog<- total[ ,!(colnames(total) %in% c("err_retrieval", "err_party"))]
party<- total[ ,!(colnames(total) %in% c("err_recognition", "err_retrieval"))]

colnames(retri)[40] <- "suberr"
colnames(recog)[40] <- "suberr"
colnames(party)[40] <- "suberr"

retri$strats_type <- "Retrieval"
recog$strats_type <- "Recognition"
party$strats_type <- "Party"

total2<- rbind(retri,recog,party)

## mean tables with different grouping ways
meanerr_dm <- group_by(total2, dm,strats_type) %>% summarize(mean_error = mean(suberr))
meanerr_strat <- group_by(total2, strat,strats_type) %>% summarize(mean_error = mean(suberr))
meanerr_search <- group_by(total2, search,strats_type) %>% summarize(mean_error = mean(suberr))
meanerr_dmstrat <- group_by(total2, dm,strat,strats_type) %>% summarize(mean_error = mean(suberr))
#  meanerr_dmstrat$strat = gsub("Retrieve-Party", "Retri-Party", meanerr_dmstrat$strat, fixed = TRUE)
#  meanerr_dmstrat$strat = gsub("Retrieve-Recognize-Party", "Retri-Recog-Party", meanerr_dmstrat$strat, fixed = TRUE)
#  meanerr_dmstrat$strat = gsub("Recognize-Party", "Recog-Party", meanerr_dmstrat$strat, fixed = TRUE)
meanerr_dmsearch <- group_by(total2, dm,search,strats_type) %>% summarize(mean_error = mean(suberr))
meanerr_stratsearch <- group_by(total2, strat,search,strats_type) %>% summarize(mean_error = mean(suberr))

meanerr_threeway <- group_by(total2, strat,search,dm,strats_type) %>% summarize(mean_error = mean(suberr))

meanerr_dmstrat <- data.frame(lapply(meanerr_dmstrat, function(x) {gsub("FULL-DM", "FULL-MEMORY", x)}))
meanerr_dmstrat[, 4] <- as.numeric(as.character( meanerr_dmstrat[, 4] ))
#total <- data.frame(lapply(total, function(x) {gsub("FULL-DM", "FULL-MEMORY", x)}))


# anova tests
summary(aov(err~dm, total))
summary(aov(err~strat, total))
summary(aov(err~search, total))
summary(aov(err~dm*strat, total))
summary(aov(err~dm*search, total))
summary(aov(err~strat*search, total))
summary(aov(err~strat*search*dm, total))

#------------------------------stacked bar plots----------------------------------

## 2-way interactions (strat & dm)
ggplot_alternative <- function()
{
  # calculate the percentages
  meanerr_dmstrat = ddply(meanerr_dmstrat, .(strat,dm), transform, percent = mean_error/sum(mean_error) * 100)
  # format the labels and calculate their positions
  meanerr_dmstrat = ddply(meanerr_dmstrat, .(strat,dm), transform, pos = (cumsum(mean_error) - 0.5 * mean_error))
  meanerr_dmstrat$label = paste0(sprintf("%.1f", meanerr_dmstrat$percent), "%")
  #print(meanerr_dmstrat$label)
  meanerr_dmstrat$label = ifelse(meanerr_dmstrat$label== "0.0%", NA, meanerr_dmstrat$label)
  #print(meanerr_dmstrat$label)
  
  # plot
  ggplot(meanerr_dmstrat,aes(x=strat,y=mean_error,fill=strats_type))+
    geom_bar(stat = "identity") +
    facet_wrap(~dm,nrow=1) + 
    geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 3.5) +
    labs(x="Memory Strategy", y="Error Rate") + 
    scale_y_continuous(labels = scales::percent,limits = c(0, 0.2)) +
    theme(panel.background = element_blank(), axis.line = element_line(colour = "black"), 
          axis.title=element_text(size = 15),
          axis.text.x = element_text(angle = 45, hjust = 1,size = 12),
          axis.text.y = element_text(size = 12), panel.spacing = unit(1, "lines")) +
    scale_fill_discrete(name="Error Attribution") 
}
#ggplot_alternative()
#ggsave("stratdm.png",ggplot_alternative(),width = 18,height = 11,dpi = 1200)
ggsave("interactionplot_0314.png",ggplot_alternative(),width = 16,height = 12,dpi = 800)
#ggsave("interactionplot222.png",ggplot_alternative(),dpi = 1200,scale=1.6)
summary(aov(err~dm*strat, total))


## main effects (declarative memory)
ggplot_alternative <- function()
{
# calculate the percentages
meanerr_dm = ddply(meanerr_dm, .(dm), transform, percent = mean_error/sum(mean_error) * 100)
# format the labels and calculate their positions
meanerr_dm = ddply(meanerr_dm, .(dm), transform, pos = (cumsum(mean_error) - 0.5 * mean_error))
meanerr_dm$label = paste0(sprintf("%.1f", meanerr_dm$percent), "%")
# plot
ggplot(meanerr_dm, aes(x = dm, y = mean_error, fill = strats_type)) +
  geom_bar(position = position_stack(), stat = "identity", width = .7) +
  geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 3) +
  labs(x="Memory Strategy", y="Error Rate") + 
  scale_y_continuous(labels = scales::percent,limits = c(0, 0.15)) 
}
ggsave("dm.png",ggplot_alternative(),width = 8,height = 8,dpi = 1200)


## main effects (voting strategy)
ggplot_alternative <- function()
{
  # calculate the percentages
  meanerr_strat = ddply(meanerr_strat, .(strat), transform, percent = mean_error/sum(mean_error) * 100)
  # format the labels and calculate their positions
  meanerr_strat = ddply(meanerr_strat, .(strat), transform, pos = (cumsum(mean_error) - 0.5 * mean_error))
  meanerr_strat$label = paste0(sprintf("%.1f", meanerr_strat$percent), "%")
  # plot
  ggplot(meanerr_strat, aes(x = strat, y = mean_error, fill = strats_type)) +
    geom_bar(position = position_stack(), stat = "identity", width = .7) +
    geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 3) +
    labs(x="Voting Strategy", y="Error Rate") + 
    scale_y_continuous(labels = scales::percent,limits = c(0, 0.08)) 
}
ggsave("strat.png",ggplot_alternative(),width = 8,height = 8,dpi = 1200)


## main effects (search)
ggplot_alternative <- function()
{
  # calculate the percentages
  meanerr_search = ddply(meanerr_search, .(search), transform, percent = mean_error/sum(mean_error) * 100)
  # format the labels and calculate their positions
  meanerr_search = ddply(meanerr_search, .(search), transform, pos = (cumsum(mean_error) - 0.5 * mean_error))
  meanerr_search$label = paste0(sprintf("%.1f", meanerr_search$percent), "%")
  # plot
  ggplot(meanerr_search, aes(x = search, y = mean_error, fill = strats_type)) +
    geom_bar(position = position_stack(), stat = "identity", width = .7) +
    geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 3) +
    labs(x="Search Strategy", y="Error Rate") + 
    scale_y_continuous(labels = scales::percent,limits = c(0, 0.08)) 
}
ggsave("search.png",ggplot_alternative(),width = 8,height = 8,dpi = 1200)


## 2-way interactions (strat & dm) inversed
ggplot_alternative <- function()
{
  # calculate the percentages
  meanerr_dmstrat = ddply(meanerr_dmstrat, .(strat,dm), transform, percent = mean_error/sum(mean_error) * 100)
  # format the labels and calculate their positions
  meanerr_dmstrat = ddply(meanerr_dmstrat, .(strat,dm), transform, pos = (cumsum(mean_error) - 0.5 * mean_error))
  meanerr_dmstrat$label = paste0(sprintf("%.1f", meanerr_dmstrat$percent), "%")
  # plot
  ggplot(meanerr_dmstrat,aes(x=dm,y=mean_error,fill=strats_type))+
    geom_bar(stat = "identity")+
    facet_wrap(~strat,nrow=1) + 
    geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 3) +
    labs(x="Voting Strategy", y="Error Rate") + 
    scale_y_continuous(labels = scales::percent,limits = c(0, 0.2)) 
}
ggsave("dmstrat.png",ggplot_alternative(),width = 18,height = 11,dpi = 1200)



## 2-way interactions (search & dm)
ggplot_alternative <- function()
{
  # calculate the percentages
  meanerr_dmsearch = ddply(meanerr_dmsearch, .(search,dm), transform, percent = mean_error/sum(mean_error) * 100)
  # format the labels and calculate their positions
  meanerr_dmsearch = ddply(meanerr_dmsearch, .(search,dm), transform, pos = (cumsum(mean_error) - 0.5 * mean_error))
  meanerr_dmsearch$label = paste0(sprintf("%.1f", meanerr_dmsearch$percent), "%")
  # plot
  ggplot(meanerr_dmsearch,aes(x=search,y=mean_error,fill=strats_type))+
    geom_bar(stat = "identity")+
    facet_wrap(~dm,nrow=1) + 
    geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 3) +
    labs(x="Search Strategy", y="Error Rate") + 
    scale_y_continuous(labels = scales::percent,limits = c(0, 0.15)) 
}
ggsave("searchdm.png",ggplot_alternative(),width = 10,height = 10,dpi = 1200)


## 2-way interactions (search & strat)
ggplot_alternative <- function()
{
  # calculate the percentages
  meanerr_stratsearch = ddply(meanerr_stratsearch, .(search,strat), transform, percent = mean_error/sum(mean_error) * 100)
  # format the labels and calculate their positions
  meanerr_stratsearch = ddply(meanerr_stratsearch, .(search,strat), transform, pos = (cumsum(mean_error) - 0.5 * mean_error))
  meanerr_stratsearch$label = paste0(sprintf("%.1f", meanerr_stratsearch$percent), "%")
  # plot
  ggplot(meanerr_stratsearch,aes(x=search,y=mean_error,fill=strats_type))+
    geom_bar(stat = "identity")+
    facet_wrap(~strat,nrow=1) + 
    geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 3) +
    labs(x="Search Strategy", y="Error Rate") + 
    scale_y_continuous(labels = scales::percent,limits = c(0, 0.08)) 
}
ggsave("searchstrat.png",ggplot_alternative(),width = 10,height = 10,dpi = 1200)




## 3-way interaction


  # calculate the percentages
  meanerr_threeway = ddply(meanerr_threeway, .(search,strat,dm), transform, percent = mean_error/sum(mean_error) * 100)
  # format the labels and calculate their positions
  meanerr_threeway = ddply(meanerr_threeway, .(search,strat,dm), transform, pos = (cumsum(mean_error) - 0.5 * mean_error))
  meanerr_threeway$label = paste0(sprintf("%.1f", meanerr_threeway$percent), "%")
  # plot
  ggplot(meanerr_threeway,aes(x=interaction(strat,dm),y=mean_error,fill=strats_type))+
    geom_bar(stat = "identity", position='stack')+
    facet_wrap(~search,nrow=2) + 
    geom_text(aes(label = label), position = position_stack(vjust = 0.5), size = 3) +
    labs(x="Search Strategy", y="Error Rate") + 
    scale_y_continuous(labels = scales::percent,limits = c(0, 0.2)) 




