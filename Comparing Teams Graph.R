rm(list=ls())

setwd("D://xxx//xxx//xxx//xxx")

library(tidyverse)
library(xlsx)
library(worldfootballR)
library(ggsoccer)
library("devtools")
library("StatsBombR")
library(soccermatics)
library("dplyr")

Dados <- read.csv("xxx.csv") 

if(!require('elliptic')) {
  install.packages('elliptic')
  library('elliptic')
}

library(SciViews)

Dados[is.na(Dados)] <- 0

Minutes <- aggregate(x= Dados$Min,
                     by= list(Dados$Team),
                     FUN=sum)
colnames(Minutes) <- c("Team","Minutes")

#PRG_C

PRG_C <- aggregate(x= Dados$PrgC_Carries,
                   by= list(Dados$Team),
                   FUN=sum)
colnames(PRG_C) <- c("Team","PRG_C")

PRG_C_H <- aggregate(x= DadosH$PrgC_Carries,
                     by= list(DadosH$Team),
                     FUN=sum)
colnames(PRG_C_H) <- c("Team","PRG_C_H")

PRG_C_A <- aggregate(x= DadosA$PrgC_Carries,
                     by= list(DadosA$Team),
                     FUN=sum)
colnames(PRG_C_A) <- c("Team","PRG_C_A")

#PRG_P

PRG_P <- aggregate(x= Dados$PrgP_Passes,
                   by= list(Dados$Team),
                   FUN=sum)
colnames(PRG_P) <- c("Team","PRG_P")

PRG_P_H <- aggregate(x= DadosH$PrgP_Passes,
                     by= list(DadosH$Team),
                     FUN=sum)
colnames(PRG_P_H) <- c("Team","PRG_P_H")

PRG_P_A <- aggregate(x= DadosA$PrgP_Passes,
                     by= list(DadosA$Team),
                     FUN=sum)
colnames(PRG_P_A) <- c("Team","PRG_P_A")

#GLS 

GLS <- aggregate(x= Dados$Gls,
                 by= list(Dados$Team),
                 FUN=sum)
colnames(GLS) <- c("Team","GLS")

GLS_H <- aggregate(x= DadosH$Gls,
                   by= list(DadosH$Team),
                   FUN=sum)
colnames(GLS_H) <- c("Team","GLS_H")

GLS_A <- aggregate(x= DadosA$Gls,
                   by= list(DadosA$Team),
                   FUN=sum)
colnames(GLS_A) <- c("Team","GLS_A")

#xG

xG <- aggregate(x= Dados$xG_Expected,
                by= list(Dados$Team),
                FUN=sum)
colnames(xG) <- c("Team","xG")

xG_H <- aggregate(x= DadosH$xG_Expected,
                  by= list(DadosH$Team),
                  FUN=sum)
colnames(xG_H) <- c("Team","xG_H")

xG_A <- aggregate(x= DadosA$xG_Expected,
                  by= list(DadosA$Team),
                  FUN=sum)
colnames(xG_A) <- c("Team","xG_A")

#GCA

GCA <- aggregate(x= Dados$GCA_SCA,
                 by= list(Dados$Team),
                 FUN=sum)
colnames(GCA) <- c("Team","GCA")

GCA_H <- aggregate(x= DadosH$GCA_SCA,
                   by= list(DadosH$Team),
                   FUN=sum)
colnames(GCA_H) <- c("Team","GCA_H")

GCA_A <- aggregate(x= DadosA$GCA_SCA,
                   by= list(DadosA$Team),
                   FUN=sum)
colnames(GCA_A) <- c("Team","GCA_A")

#SCA

SCA <- aggregate(x= Dados$SCA_SCA,
                 by= list(Dados$Team),
                 FUN=sum)
colnames(SCA) <- c("Team","SCA")

SCA_H <- aggregate(x= DadosH$SCA_SCA,
                   by= list(DadosH$Team),
                   FUN=sum)
colnames(SCA_H) <- c("Team","SCA_H")

SCA_A <- aggregate(x= DadosA$SCA_SCA,
                   by= list(DadosA$Team),
                   FUN=sum)
colnames(SCA_A) <- c("Team","SCA_A")

DATA <- GLS %>% 
  inner_join(GLS_H,
             by = c("Team")) %>%
  inner_join(GLS_A,
             by = c("Team")) %>%
  inner_join(xG,
             by = c("Team")) %>%
  inner_join(xG_H,
             by = c("Team")) %>%
  inner_join(xG_A,
             by = c("Team")) %>%
  inner_join(PRG_C,
             by = c("Team")) %>%  
  inner_join(PRG_C_H,
             by = c("Team")) %>%
  inner_join(PRG_C_A,
             by = c("Team")) %>%
  inner_join(PRG_P,
             by = c("Team")) %>%
  inner_join(PRG_P_H,
             by = c("Team")) %>%
  inner_join(PRG_P_A,
             by = c("Team")) %>%
  inner_join(GCA,
             by = c("Team")) %>%
  inner_join(GCA_H,
             by = c("Team")) %>%
  inner_join(GCA_A,
             by = c("Team")) %>%
  inner_join(SCA,
             by = c("Team")) %>%
  inner_join(SCA_H,
             by = c("Team")) %>%
  inner_join(SCA_A,
             by = c("Team"))

library(ggplot2)
library(BasketballAnalyzeR)
library(curl)
library(htmltab)
library(stringr)
library(dplyr)
library(gridExtra)
library(grid)
library(cowplot)

#Data per 90 min.

DATA$GLS <-   DATA$GLS/(DATA$Minutes/990)
MeanGls <- mean(as.numeric(DATA$GLS))
MaxGls <- max(as.numeric(DATA$GLS))

DATA$xG <-   DATA$xG/(DATA$Minutes/990)
MeanxG <- mean(as.numeric(DATA$xG))
MaxxG <- max(as.numeric(DATA$xG))

DATA$PRG_C <-   DATA$PRG_C/(DATA$Minutes/990)
MeanPrgC <- mean(as.numeric(DATA$PRG_C))
MaxPrgC <- max(as.numeric(DATA$PRG_C))

DATA$PRG_P <-   DATA$PRG_P/(DATA$Minutes/990)
MeanPrgP <- mean(as.numeric(DATA$PRG_P))
MaxPrgP <- max(as.numeric(DATA$PRG_P))

DATA$GCA <-   DATA$GCA/(DATA$Minutes/990)
MeanGCA <- mean(as.numeric(DATA$GCA))
MaxGCA <- max(as.numeric(DATA$GCA))

DATA$SCA <-   DATA$SCA/(DATA$Minutes/990)
MeanSCA <- mean(as.numeric(DATA$SCA))
MaxSCA <- max(as.numeric(DATA$SCA))

DATA <- select(DATA,Team,GLS,xG,GCA,SCA,PRG_C,PRG_P)

DATA$GLS <- (DATA$GLS/MaxGls)*100
DATA$xG <- (DATA$xG/MaxxG)*100
DATA$GCA <- (DATA$GCA/MaxGCA)*100
DATA$SCA <- (DATA$SCA/MaxSCA)*100
DATA$PRG_C <- (DATA$PRG_C/MaxPrgC)*100
DATA$PRG_P <- (DATA$PRG_P/MaxPrgP)*100

windowsFonts(A = windowsFont("NewsGotT")) #Alterar o tipo de letra

DATA_N <- subset(DATA, select = -Team)
Graf <- mutate_all(DATA_N, function(x) as.numeric(as.character(x)))
Graf1 <- radialprofile(data=Graf,
                       title=DATA$Team,
                       perc=TRUE,
                       std=FALSE,
                       min.mid.max=NULL)

Graf2 <- grid.arrange(grobs=Graf1,
                      ncol=6,
                      top = textGrob("Team Analysis - Primeira Liga 2023/24
(Comparison with the max. values per 90)
",
                      gp=gpar(fontsize=20,font=3)),
                      bottom = textGrob("By: TheStatsWay
Data from FBref",gp=gpar(fontsize=15,font=3)))

ggsave("xxx.png",
       plot = Graf2,
       width = 5500, 
       height = 3500, 
       units = "px")