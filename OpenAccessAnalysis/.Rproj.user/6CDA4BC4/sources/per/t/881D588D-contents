# OpenAccessAnalysis.R
#
# Reports
# 1. Number of open-access articles in JWRPM frojm 2018 to 2023 from Scopus querry.
# 2. Whether number of citations is statistically different for open-access vs closed-access articles.
#
#
# This is a beginning R-programming effort! There could be lurking bugs or basic coding errors that I am not even aware of.
# Please report bugs/feedback to me (contact info below)
#
# David E. Rosenberg
# May 30, 2023
# Utah State University
# david.rosenberg@usu.edu

rm(list = ls())  #Clear history

# Load required libraies

if (!require(tidyverse)) { 
  install.packages("tidyverse", repos="http://cran.r-project.org") 
  library(tidyverse) 
}

if (!require(readxl)) { 
  install.packages("readxl", repos="http://cran.r-project.org") 
  library(readxl) 
}

  
if (!require(RColorBrewer)) { 
  install.packages("RColorBrewer",repos="http://cran.r-project.org") 
  library(RColorBrewer) # 
}

if (!require(dplyr)) { 
  install.packages("dplyr",repos="http://cran.r-project.org") 
  library(dplyr) # 
}

if (!require(expss)) { 
  install.packages("expss",repos="http://cran.r-project.org") 
  library(expss) # 
}

if (!require(reshape2)) { 
  install.packages("reshape2", repos="http://cran.r-project.org") 
  library(reshape2) 
}

if (!require(pracma)) { 
  install.packages("pracma", repos="http://cran.r-project.org") 
  library(pracma) 
}

if (!require(lubridate)) { 
  install.packages("lubridate", repos="http://cran.r-project.org") 
  library(lubridate) 
}

if (!require(directlabels)) { 
  install.packages("directlabels", repo="http://cran.r-project.org")
  library(directlabels) 
}


if (!require(plyr)) { 
  install.packages("plyr", repo="http://cran.r-project.org")
  library(plyr) 
}

if (!require(ggrepel)) { 
  devtools::install_github("slowkow/ggrepel")
  library(ggrepel) 
}

# Read in Scopus query data. Two files one 2017 to 2019, second 2020 to present
# sCSVFile <- 'ScopusQueryMay2023.csv'
#sCSVFile <- 'ScopusQueryJuly2023.csv'
sCSVFile <- 'ScopusQueryAllASCEgt2020.csv'
#sCSVFile <- 'ScopusQueryAllASCEgt2018.csv'
dfArticles <- read.csv(file = sCSVFile, header=TRUE, stringsAsFactors=FALSE, sep=",")

sCSVFile2 <- 'ScopusQueryAllASCEgt2018.csv'
dfArticles2 <- read.csv(file = sCSVFile2, header=TRUE, stringsAsFactors=FALSE, sep=",")
#bind the two files together
dfArticles <- rbind(dfArticles, dfArticles2)

#Screen out articles where text read was not correct
#Convert to numeric or NA
dfArticles$Year <- as.numeric(dfArticles$Year)
dfArticles <- dfArticles[!is.na(dfArticles[,"Year"]),]

#Retreave the column headers
cHeaders <- colnames(dfArticles)

#Substitute zeros for NA in Cited.by
dfArticles$Cited.by[is.na(dfArticles$Cited.by)] <- 0

#Count number of articles and number of open access
nArticles <- as.numeric(nrow(dfArticles))
#Count number of open-access articles
dfArticles$Open.Access.asBinary <- ifelse(dfArticles$Open.Access == '',0,1)
nOpenAccess <- sum(dfArticles$Open.Access.asBinary)

dfArticlesOpenAccess <- dfArticles %>% filter(Open.Access.asBinary == 1) %>% select(Cited.by)
dfArticlesNotOpen <- dfArticles %>% filter(Open.Access.asBinary == 0) %>% select(Cited.by)

#Plot Open access vs closed access per year
cYears <- seq(min(dfArticles$Year)-1,max(dfArticles$Year)+1,1)

#Count number of articles per year open access and closed access
dfArticleByYear <- dfArticles %>% group_by(Year, Open.Access.asBinary) %>% dplyr::summarise(count = n())

#Pivot Open.Access.asBinary into columns
dfTest <-  dfArticleByYear %>% pivot_wider(names_from = Open.Access.asBinary, values_from = count)
#CalculatePercentOpenAccess
dfTest$PercentOpenAccess <- dfTest$`1` / dfTest$`0`

#Calculate number of articles from 2020 to 2023
dfArticles2020To2023 <- dfArticleByYear %>% filter(Year >= 2020) %>% dplyr::summarise(Count = sum(count))
nArticles2020To2023 <- sum(dfArticles2020To2023$Count)

ggplot(data = dfArticles) +
      geom_histogram(aes(x=Year, color = factor(Open.Access.asBinary), fill = factor(Open.Access.asBinary)), binwidth = 1, color = 'black', width = 5) +
      #geom_label(data = dfTest, aes(x = Year, y=`1`+5, label = sprintf("%1.1f%%", 100*PercentOpenAccess)), size = 5) +
    geom_text(data = as.data.frame(dfTest), aes(x = Year, y=`1`+ 7, label = sprintf("%1.0f%%", 100*PercentOpenAccess)), size = 5)    +
    scale_color_discrete(name = "Type", labels= c("Closed", "Open Access")) +
      scale_fill_discrete(name = "Type", labels= c("Closed", "Open Access")) + 
      labs(y = "Number of Articles", x = '') +
      scale_x_continuous(limits=c(min(cYears)+0.5,max(cYears)-0.5), breaks = cYears, labels = cYears, minor_breaks = cYears) +
      theme_light() +
      theme(text = element_text(size = 16) ) 


#Use Kolgomorv-Smirnov nonparametric test to determine if distribution of number of citations is different for open-access papers as non-open access papers
ks_result <- ks.test(as.numeric(dfArticlesOpenAccess$Cited.by), as.numeric(dfArticlesNotOpen$Cited.by))

print(ks_result)

if(ks_result$p.value > 0.05) {
  print("Not different at 0.05 level")
} else {print("Different with 95% confidence")
}

#ggplot(dfArticles, aes(Cited.by ,color = Open.Access.asBinary, linetype = Open.Access.asBinary)) + 

ggplot(dfArticles, aes(x=as.numeric(Cited.by), color = factor(Open.Access.asBinary)), width = 2) + 
  
  stat_ecdf() +
  scale_color_discrete(labels= c("Closed", "Open Access")) + 

  xlim(c(0, 50)) +
  labs(y = "Cummulative Distribution", x = 'Number of Citations', color = "") +
  theme_light() +
  theme(legend.position = c(0.8,0.8), text = element_text(size = 16) ) 


