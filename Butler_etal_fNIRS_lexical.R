#Load libraries:
library(ggplot2)
library(dplyr)
library(lme4)
library(ggrepel)
library(tidyverse)
library(MatchIt)
library(tidyr)
require(ggiraph)
require(ggiraphExtra)
require(plyr)
library(sjmisc)
library(Hmisc)
require(ggiraph)
require(ggiraphExtra)
require(plyr)
library(devtools)
library(RColorBrewer)
library(corrr)
library(viridis)
library(pwr)
library(tidyverse)
library(rstatix)
library(ggpubr)
library(magrittr)
library(forcats)

#Set working drive
setwd("/Users/lindsaybutler/Dropbox/2018BUPostdoc/2022fNIRS_Brain_Sci/For_open_access")

#Read in csv file
t = read.csv(file = "long_d230314.csv", header = TRUE)

#Summarize the data
summary(t)

#Set variables that are factors
t$Subject <- as.factor(t$Subject)
t$exp_cond <- as.factor(t$exp_cond)
t$Experiment <- as.factor(t$Experiment)
t$Condition <- as.factor(t$Condition)
t$Base_task <- as.factor(t$Base_task)
t$channel <- as.factor(t$channel)

########rCeate a dataset of just the production experiment, phonological condition

tpp <- subset(t, Experiment %in% c("Production") & Condition %in% c("Phonological"))
summary(tpp)


#Pairwise t-tests for task over baseline for each channel in the production experiment, phonological condition

lapply(unique(tpp$channel), 
       function(x) pairwise.t.test(tpp[tpp$channel == x,"value"],
                                   tpp[tpp$channel == x,"Base_task"],
                                   p.adjust.method = "bon",
                                   paired = TRUE))


#rename the channels to include the p-values resulting from the pariwise t-tests

levels(tpp$channel) <- c("Channel 1 p=.21", "Channel 2 p=.3", "Channel 3 p=.0053", "Channel 4 p=.49", "Channel 6 p=.82", "Channel 7 p=.074", "Channel 8 p=.43", "Channel 9 p=.26", "Channel 10 p=.0062", "Channel 11 p=.067", "Channel 12 p=.5", "Channel 13 p=.45", "Channel 15 p=.59", "Channel 16 p=.41", "Channel 17 p=.55", "Channel 18 p=.76", "Channel 19 p=.037", "Channel 20 p=.89", "Channel 21 p=.015", "Channel 22 p=.37", "Channel 24 p=.15", "Channel 25 p=.14", "Channel 26 p=.32", "Channel 27 p=.28", "Channel 28 p=.16", "Channel 29 p=.0079", "Channel 30 p=.57", "Channel 31 p=.48", "Channel 33 p=.56", "Channel 34 p=.74", "Channel 35 p=.58", "Channel 36 p=.67")

#re-ordere the channels to: right IFG > right PFC > left PFC > left IFG
tpp$channel <- factor(tpp$channel, ordered = TRUE, levels = c("Channel 36 p=.67", "Channel 35 p=.58", "Channel 34 p=.74", 
                                                              "Channel 33 p=.56", "Channel 31 p=.48", "Channel 30 p=.57", 
                                                              "Channel 29 p=.0079", "Channel 27 p=.28", "Channel 28 p=.16", 
                                                              "Channel 26 p=.32", "Channel 25 p=.14", "Channel 24 p=.15", 
                                                              "Channel 22 p=.37", "Channel 21 p=.015", "Channel 20 p=.89", 
                                                              "Channel 19 p=.037", "Channel 1 p=.21", "Channel 2 p=.3", 
                                                              "Channel 3 p=.0053", "Channel 4 p=.49", "Channel 6 p=.82", 
                                                              "Channel 7 p=.074", "Channel 8 p=.43", "Channel 10 p=.0062", 
                                                              "Channel 9 p=.26", "Channel 11 p=.067", "Channel 12 p=.5", 
                                                              "Channel 13 p=.45", "Channel 15 p=.59", "Channel 16 p=.41", 
                                                              "Channel 17 p=.55", "Channel 18 p=.76"))


#########Create a boxplot to show each subject’s mean with points and the group mean

tppo2 <- ggplot(tpp, aes(y=value, x=channel, color=Base_task)) +
  geom_boxplot(outlier.shape=NA)+
  geom_jitter(size=1, alpha=.25)+
  scale_x_discrete("Channel-wise mean HbO during the production of phonologically related words", 
                   breaks=c("Channel 36 p=.67", "Channel 35 p=.58", "Channel 34 p=.74", "Channel 33 p=.56", "Channel 31 p=.48", "Channel 30 p=.57", "Channel 29 p=.0079", "Channel 27 p=.28", "Channel 28 p=.16", "Channel 26 p=.32", "Channel 25 p=.14", "Channel 24 p=.15", "Channel 22 p=.37", "Channel 21 p=.015", "Channel 20 p=.89", "Channel 19 p=.037", "Channel 1 p=.21", "Channel 2 p=.3", "Channel 3 p=.0053", "Channel 4 p=.49", "Channel 6 p=.82", "Channel 7 p=.074", "Channel 8 p=.43", "Channel 10 p=.0062", "Channel 9 p=.26", "Channel 11 p=.067", "Channel 12 p=.5", "Channel 13 p=.45", "Channel 15 p=.59", "Channel 16 p=.41", "Channel 17 p=.55", "Channel 18 p=.76"),
                   labels=c("Channel 36 p=.67", "Channel 35 p=.58", "Channel 34 p=.74", "Channel 33 p=.56", "Channel 31 p=.48", "Channel 30 p=.57", "Channel 29 p=.0079", "Channel 27 p=.28", "Channel 28 p=.16", "Channel 26 p=.32", "Channel 25 p=.14", "Channel 24 p=.15", "Channel 22 p=.37", "Channel 21 p=.015", "Channel 20 p=.89", "Channel 19 p=.037", "Channel 1 p=.21", "Channel 2 p=.3", "Channel 3 p=.0053", "Channel 4 p=.49", "Channel 6 p=.82", "Channel 7 p=.074", "Channel 8 p=.43", "Channel 10 p=.0062", "Channel 9 p=.26", "Channel 11 p=.067", "Channel 12 p=.5", "Channel 13 p=.45", "Channel 15 p=.59", "Channel 16 p=.41", "Channel 17 p=.55", "Channel 18 p=.76"))+
  scale_y_continuous("Mean HbO (micormolars)", limits=c(-.00025,.00025))+
  scale_color_manual("Baseline vs. Task",
                     values=c("darksalmon", "darkred"))+
  annotate("text", label = "**", x = 7, y = .0002, size = 10, colour = "darkred") +
  annotate("text", label = "*", x = 14, y = .0002, size = 10, colour = "darkred") +
  annotate("text", label = "*", x = 16, y = .0002, size = 10, colour = "darkred") +
  annotate("text", label = "**", x = 19, y = .0002, size = 10, colour = "darkred") +
  annotate("text", label = "**", x = 24, y = .0002, size = 10, colour = "darkred") +
  annotate("text", label = "Right IFG", x = 4, y = -.00025, size = 5, colour = "steelblue4") +
  annotate("text", label = "Right MFG", x = 12, y = -.00025, size = 5, colour = "steelblue4") +
  annotate("text", label = "Left MFG", x = 20, y = -.00025, size = 5, colour = "steelblue4") +
  annotate("text", label = "Left IFG", x = 28, y = -.00025, size = 5, colour = "steelblue4") +
  geom_vline(xintercept=8.5, color = "steelblue4")+
  geom_vline(xintercept=16.5, color = "steelblue4")+
  geom_vline(xintercept=24.5, color = "steelblue4")+
  theme_bw()+
  theme(legend.position = "top")+
  theme(axis.text.x = element_text(size=12, angle = 55, hjust=1))
tppo2
ggsave(tppo2, file="plot_prod_phon_task_baseline_hbo.jpg", width=12, height=8, dpi=300)


#########Create a dataset of just the production experiment, semantic condition

tps <- subset(t, Experiment %in% c("Production") & Condition %in% c("Semantic"))
summary(tps)

#Pairwise t-tests
lapply(unique(tps$channel), 
       function(x) pairwise.t.test(tps[tps$channel == x,"value"],
                                   tpp[tps$channel == x,"Base_task"],
                                   p.adjust.method = "bon",
                                   paired = TRUE))


#rename levels of channel to include p-values resulting from t-tests

levels(tps$channel) <- c("Channel 1 p=.39", "Channel 2 p=59", "Channel 3 p=.46", "Channel 4 p=.21", "Channel 6 p=.76", "Channel 7 p=.52", "Channel 8 p=.12", "Channel 9 p=.89", "Channel 10 p=.75", "Channel 11 p=.88", "Channel 12 p=.19", "Channel 13 p=.43", "Channel 15 p=.96", "Channel 16 p=.39", "Channel 17 p=.8", "Channel 18 p=.8", "Channel 19 p=.0059", "Channel 20 p=.027", "Channel 21 p=.82", "Channel 22 p=.32", "Channel 24 p=.013", "Channel 25 p=.59", "Channel 26 p=.58", "Channel 27 p=.79", "Channel 28 p=.027", "Channel 29 p=.35", "Channel 30 p=.086", "Channel 31 p=.26", "Channel 33 p=.62", "Channel 34 p=.16", "Channel 35 p=.56", "Channel 36 p=.56")


#re-ordere the channels to: right IFG > right PFC > left PFC > left IFG

tps$channel <- factor(tps$channel, ordered = TRUE, levels = c("Channel 36 p=.56", "Channel 35 p=.56", "Channel 34 p=.16", 
                                                              "Channel 33 p=.62", "Channel 31 p=.26", "Channel 30 p=.086", 
                                                              "Channel 29 p=.35", "Channel 27 p=.79", "Channel 28 p=.027", 
                                                              "Channel 26 p=.58", "Channel 25 p=.59", "Channel 24 p=.013", 
                                                              "Channel 22 p=.32", "Channel 21 p=.82",  "Channel 20 p=.027", 
                                                              "Channel 19 p=.0059", "Channel 1 p=.39", "Channel 2 p=.59", 
                                                              "Channel 3 p=.46", "Channel 4 p=.21", "Channel 6 p=.76", 
                                                              "Channel 7 p=.52", "Channel 8 p=.12", "Channel 10 p=.75", 
                                                              "Channel 9 p=.89", "Channel 11 p=.88", "Channel 12 p=.19", 
                                                              "Channel 13 p=.43", "Channel 15 p=.96", "Channel 16 p=.39", 
                                                              "Channel 17 p=.8", "Channel 18 p=.8"))
levels(tps$channel)

#Create a boxplot to show each subject’s mean with points and the group mean

tpso <- ggplot(tps, aes(y=value, x=channel, color=Base_task)) +
  geom_boxplot(outlier.shape=NA)+
  geom_jitter(size=1, alpha=.25)+
  scale_x_discrete("Channel-wise mean HbO during the production of semantically related words", 
                   breaks=c("Channel 36 p=.56", "Channel 35 p=.56", "Channel 34 p=.16", 
                            "Channel 33 p=.62", "Channel 31 p=.26", "Channel 30 p=.086", 
                            "Channel 29 p=.35", "Channel 27 p=.79", "Channel 28 p=.027", 
                            "Channel 26 p=.58", "Channel 25 p=.59", "Channel 24 p=.013", 
                            "Channel 22 p=.32", "Channel 21 p=.82",  "Channel 20 p=.027", 
                            "Channel 19 p=.0059", "Channel 1 p=.39", "Channel 2 p=.59", 
                            "Channel 3 p=.46", "Channel 4 p=.21", "Channel 6 p=.76", 
                            "Channel 7 p=.52", "Channel 8 p=.12", "Channel 10 p=.75", 
                            "Channel 9 p=.89", "Channel 11 p=.88", "Channel 12 p=.19", 
                            "Channel 13 p=.43", "Channel 15 p=.96", "Channel 16 p=.39", 
                            "Channel 17 p=.8", "Channel 18 p=.8"),
                   labels=c("Channel 36 p=.56", "Channel 35 p=.56", "Channel 34 p=.16", 
                            "Channel 33 p=.62", "Channel 31 p=.26", "Channel 30 p=.086", 
                            "Channel 29 p=.35", "Channel 27 p=.79", "Channel 28 p=.027", 
                            "Channel 26 p=.58", "Channel 25 p=.59", "Channel 24 p=.013", 
                            "Channel 22 p=.32", "Channel 21 p=.82",  "Channel 20 p=.027", 
                            "Channel 19 p=.0059", "Channel 1 p=.39", "Channel 2 p=.59", 
                            "Channel 3 p=.46", "Channel 4 p=.21", "Channel 6 p=.76", 
                            "Channel 7 p=.52", "Channel 8 p=.12", "Channel 10 p=.75", 
                            "Channel 9 p=.89", "Channel 11 p=.88", "Channel 12 p=.19", 
                            "Channel 13 p=.43", "Channel 15 p=.96", "Channel 16 p=.39", 
                            "Channel 17 p=.8", "Channel 18 p=.8"))+
  scale_y_continuous("Mean HbO (micormolars)", limits=c(-.00025,.00025))+
  scale_color_manual("Baseline vs. Task",
                     values=c("darksalmon", "darkred"))+
  annotate("text", label = "*", x = 9, y = .00021, size = 10, colour = "darkred") +
  annotate("text", label = "Right IFG", x = 4, y = -.00025, size = 5, colour = "steelblue4") +
  annotate("text", label = "Right MFG", x = 12, y = -.00025, size = 5, colour = "steelblue4") +
  annotate("text", label = "Left MFG", x = 20, y = -.00025, size = 5, colour = "steelblue4") +
  annotate("text", label = "Left IFG", x = 28, y = -.00025, size = 5, colour = "steelblue4") +
  geom_vline(xintercept=8.5, color = "steelblue4")+
  geom_vline(xintercept=16.5, color = "steelblue4")+
  geom_vline(xintercept=24.5, color = "steelblue4")+
  theme_bw()+
  theme(legend.position = "top")+
  theme(axis.text.x = element_text(size=12, angle = 55, hjust=1))
tpso
ggsave(tpso, file="plot_prod_sem_task_baseline_hbo.jpg", width=12, height=8, dpi=300)



#########Create a dataset of just the comprehension experiment, phonological condition

tcp <- subset(t, Experiment %in% c("Comprehension") & Condition %in% c("Phonological"))
summary(tcp)

#pariwise t-tests
lapply(unique(tcp$channel), 
       function(x) pairwise.t.test(tcp[tcp$channel == x,"value"],
                                   tcp[tcp$channel == x,"Base_task"],
                                   p.adjust.method = "bon",
                                   paired = TRUE))

#rename levels of channel to include p-values:
levels(tcp$channel) <- c("Channel 1 p=.018", "Channel 2 p=.16", "Channel 3 p=.39", 
                         "Channel 4 p=.59", "Channel 6 p=.33", "Channel 7 p=.6", 
                         "Channel 8 p=.68", "Channel 9 p=.99", "Channel 10 p=.8", 
                         "Channel 11 p=.96", "Channel 12 p=.8", "Channel 13 p=.7", 
                         "Channel 15 p=.15", "Channel 16 p=.19",  "Channel 17 p=.16", 
                         "Channel 18 p=.12", "Channel 19 p=.53", "Channel 20 p=.032", 
                         "Channel 21 p=.65", "Channel 22 p=.53", "Channel 24 p=.07", 
                         "Channel 25 p=.79", "Channel 26 p=.7", "Channel 27 p=.89", 
                         "Channel 28 p=.71", "Channel 29 p=.75", "Channel 30 p=.35", 
                         "Channel 31 p=.26", "Channel 33 p=.88", "Channel 34 p=.49", 
                         "Channel 35 p=.47", "Channel 36 p=.34")


#re-order levels of channel : right IFG > right PFC > left PFC > left IFG

tcp$channel <- factor(tcp$channel, ordered = TRUE, levels = c("Channel 36 p=.34", "Channel 35 p=.47", "Channel 34 p=.49", 
                                                              "Channel 33 p=.88", "Channel 31 p=.26", "Channel 30 p=.35", 
                                                              "Channel 29 p=.75", "Channel 27 p=.89", "Channel 28 p=.71", 
                                                              "Channel 26 p=.7", "Channel 25 p=.79", "Channel 24 p=.07", 
                                                              "Channel 22 p=.53", "Channel 21 p=.65",  "Channel 20 p=.032", 
                                                              "Channel 19 p=.53", 
                                                              "Channel 1 p=.018", "Channel 2 p=.16", "Channel 3 p=.39", 
                                                              "Channel 4 p=.59", "Channel 6 p=.33", "Channel 7 p=.6", 
                                                              "Channel 8 p=.68", "Channel 10 p=.8", "Channel 9 p=.99",
                                                              "Channel 11 p=.96", "Channel 12 p=.8", "Channel 13 p=.7", 
                                                              "Channel 15 p=.15", "Channel 16 p=.19",  "Channel 17 p=.16", 
                                                              "Channel 18 p=.12"))
levels(tcp$channel)


#create a boxplot of task over baseline HbO
tcpo <- ggplot(tcp, aes(y=value, x=channel, color=Base_task)) +
  geom_boxplot(outlier.shape=NA)+
  geom_jitter(size=1, alpha=.25)+
  scale_x_discrete("Channel-wise mean HbO during the production of phonologically related words", 
                   breaks=c("Channel 36 p=.34", "Channel 35 p=.47", "Channel 34 p=.49", 
                            "Channel 33 p=.88", "Channel 31 p=.26", "Channel 30 p=.35", 
                            "Channel 29 p=.75", "Channel 27 p=.89", "Channel 28 p=.71", 
                            "Channel 26 p=.7", "Channel 25 p=.79", "Channel 24 p=.07", 
                            "Channel 22 p=.53", "Channel 21 p=.65",  "Channel 20 p=.032", 
                            "Channel 19 p=.53", "Channel 1 p=.018", "Channel 2 p=.16", "Channel 3 p=.39", 
                            "Channel 4 p=.59", "Channel 6 p=.33", "Channel 7 p=.6", 
                            "Channel 8 p=.68", "Channel 10 p=.8", "Channel 9 p=.99",
                            "Channel 11 p=.96", "Channel 12 p=.8", "Channel 13 p=.7", 
                            "Channel 15 p=.15", "Channel 16 p=.19",  "Channel 17 p=.16", "Channel 18 p=.12"),
                   labels=c("Channel 36 p=.34", "Channel 35 p=.47", "Channel 34 p=.49", 
                            "Channel 33 p=.88", "Channel 31 p=.26", "Channel 30 p=.35", 
                            "Channel 29 p=.75", "Channel 27 p=.89", "Channel 28 p=.71", 
                            "Channel 26 p=.7", "Channel 25 p=.79", "Channel 24 p=.07", 
                            "Channel 22 p=.53", "Channel 21 p=.65",  "Channel 20 p=.032", 
                            "Channel 19 p=.53", "Channel 1 p=.018", "Channel 2 p=.16", "Channel 3 p=.39", 
                            "Channel 4 p=.59", "Channel 6 p=.33", "Channel 7 p=.6", 
                            "Channel 8 p=.68", "Channel 10 p=.8", "Channel 9 p=.99",
                            "Channel 11 p=.96", "Channel 12 p=.8", "Channel 13 p=.7", 
                            "Channel 15 p=.15", "Channel 16 p=.19",  "Channel 17 p=.16", "Channel 18 p=.12"))+
  scale_y_continuous("Mean HbO (micormolars)", limits=c(-.00025,.00025))+
  scale_color_manual("Baseline vs. Task",
                     values=c("darksalmon", "darkred"))+
  annotate("text", label = "Right IFG", x = 4, y = -.00025, size = 5, colour = "steelblue4") +
  annotate("text", label = "Right MFG", x = 12, y = -.00025, size = 5, colour = "steelblue4") +
  annotate("text", label = "Left MFG", x = 20, y = -.00025, size = 5, colour = "steelblue4") +
  annotate("text", label = "Left IFG", x = 28, y = -.00025, size = 5, colour = "steelblue4") +
  geom_vline(xintercept=8.5, color = "steelblue4")+
  geom_vline(xintercept=16.5, color = "steelblue4")+
  geom_vline(xintercept=24.5, color = "steelblue4")+
  theme_bw()+
  theme(legend.position = "top")+
  theme(axis.text.x = element_text(size=12, angle = 55, hjust=1))
tcpo
ggsave(tcpo, file="plot_comp_phon_task_baseline_hbo.jpg", width=12, height=8, dpi=300)


#########Create a dataset of just the comprehension experiment, semantic condition

tcs <- subset(t, Experiment %in% c("Comprehension") & Condition %in% c("Semantic"))
summary(tcs)


#pairwise t-tests

lapply(unique(tcs$channel), 
       function(x) pairwise.t.test(tcs[tcs$channel == x,"value"],
                                   tcs[tcs$channel == x,"Base_task"],
                                   p.adjust.method = "bon",
                                   paired = TRUE))

#rename levels of channel to include p-values resulting from t-tests
levels(tcs$channel) <- c("Channel 1 p=.042", "Channel 2 p=.064", "Channel 3 p=.52", 
                         "Channel 4 p=.62", "Channel 6 p=.61", "Channel 7 p=.46", 
                         "Channel 8 p=.31", "Channel 9 p=.067", "Channel 10 p=.38", 
                         "Channel 11 p=.84", "Channel 12 p=.64", "Channel 13 p=.1", 
                         "Channel 15 p=.36", "Channel 16 p=.19", "Channel 17 p=.64", 
                         "Channel 18 p=.9", "Channel 19 p=.77", "Channel 20 p=.27", 
                         "Channel 21 p=.13", "Channel 22 p=.45", "Channel 24 p=.22", 
                         "Channel 25 p=.85", "Channel 26 p=.88", "Channel 27 p=.1", 
                         "Channel 28 p=.85", "Channel 29 p=.77", "Channel 30 p=.07", 
                         "Channel 31 p=.1", "Channel 33 p=.19", "Channel 34 p=.038", "Channel 35 p=.071", "Channel 36 p=.044")


#re-order levels of channel : right IFG > right PFC > left PFC > left IFG

tcs$channel <- factor(tcs$channel, ordered = TRUE, levels = c("Channel 36 p=.044", "Channel 35 p=.071", "Channel 34 p=.038", 
                                                              "Channel 33 p=.19", "Channel 31 p=.1", "Channel 30 p=.07", 
                                                              "Channel 29 p=.77", "Channel 27 p=.1", "Channel 28 p=.85", 
                                                              "Channel 26 p=.88", "Channel 25 p=.85", "Channel 24 p=.22", 
                                                              "Channel 22 p=.45", "Channel 21 p=.13",  "Channel 20 p=.027", 
                                                              "Channel 19 p=.77", 
                                                              "Channel 1 p=.042", "Channel 2 p=.064", "Channel 3 p=.52", 
                                                              "Channel 4 p=.62", "Channel 6 p=.61", "Channel 7 p=.46", 
                                                              "Channel 8 p=.31", "Channel 10 p=.38", "Channel 9 p=.067",
                                                              "Channel 11 p=.84", "Channel 12 p=.64", "Channel 13 p=.1", 
                                                              "Channel 15 p=.36", "Channel 16 p=.19", "Channel 17 p=.64", 
                                                              "Channel 18 p=.9"))
levels(tcs$channel)

#Create a boxplot of task and baseline mean HbO concentrations

tcso <- ggplot(tcs, aes(y=value, x=channel, color=Base_task))+
  geom_boxplot(outlier.shape=NA)+
  geom_jitter(size=1, alpha=.25)+
  scale_x_discrete("Channel-wise mean HbO during the comprehension of semantically related words", 
                   breaks=c("Channel 36 p=.044", "Channel 35 p=.071", "Channel 34 p=.038", 
                            "Channel 33 p=.19", "Channel 31 p=.1", "Channel 30 p=.07", 
                            "Channel 29 p=.77", "Channel 27 p=.1", "Channel 28 p=.85", 
                            "Channel 26 p=.88", "Channel 25 p=.85", "Channel 24 p=.22", 
                            "Channel 22 p=.45", "Channel 21 p=.13",  "Channel 20 p=.027", 
                            "Channel 19 p=.77", "Channel 1 p=.042", "Channel 2 p=.064", "Channel 3 p=.52", 
                            "Channel 4 p=.62", "Channel 6 p=.61", "Channel 7 p=.46", 
                            "Channel 8 p=.31", "Channel 10 p=.38", "Channel 9 p=.067",
                            "Channel 11 p=.84", "Channel 12 p=.64", "Channel 13 p=.1", 
                            "Channel 15 p=.36", "Channel 16 p=.19", "Channel 17 p=.64", "Channel 18 p=.9"),
                   labels=c("Channel 36 p=.044", "Channel 35 p=.071", "Channel 34 p=.038", 
                            "Channel 33 p=.19", "Channel 31 p=.1", "Channel 30 p=.07", 
                            "Channel 29 p=.77", "Channel 27 p=.1", "Channel 28 p=.85", 
                            "Channel 26 p=.88", "Channel 25 p=.85", "Channel 24 p=.22", 
                            "Channel 22 p=.45", "Channel 21 p=.13",  "Channel 20 p=.027", 
                            "Channel 19 p=.77","Channel 1 p=.042", "Channel 2 p=.064", "Channel 3 p=.52", 
                            "Channel 4 p=.62", "Channel 6 p=.61", "Channel 7 p=.46", 
                            "Channel 8 p=.31", "Channel 10 p=.38", "Channel 9 p=.067",
                            "Channel 11 p=.84", "Channel 12 p=.64", "Channel 13 p=.1", 
                            "Channel 15 p=.36", "Channel 16 p=.19", "Channel 17 p=.64", "Channel 18 p=.9"))+
  scale_y_continuous("Mean HbO (micormolars)", limits=c(-.00025,.00025))+
  scale_color_manual("Task vs. baseline",
                     values=c("darksalmon", "darkred"))+
  annotate("text", label = "Right IFG", x = 4, y = -.00025, size = 5, colour = "steelblue4") +
  annotate("text", label = "Right MFG", x = 12, y = -.00025, size = 5, colour = "steelblue4") +
  annotate("text", label = "Left MFG", x = 20, y = -.00025, size = 5, colour = "steelblue4") +
  annotate("text", label = "Left IFG", x = 28, y = -.00025, size = 5, colour = "steelblue4") +
  geom_vline(xintercept=8.5, color = "steelblue4")+
  geom_vline(xintercept=16.5, color = "steelblue4")+
  geom_vline(xintercept=24.5, color = "steelblue4")+
  theme_bw()+
  theme(legend.position = "top")+
  theme(axis.text.x = element_text(size=12, angle = 55, hjust=1))
tcso
ggsave(tcso, file="plot_comp_sem_task_baseline_hbo.jpg", width=12, height=8, dpi=300)
