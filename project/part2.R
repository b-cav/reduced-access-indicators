###################################################################################
# Code Part 2 - Microdata Processing Script
###################################################################################

library(foreign)
library(dplyr)
library(ggplot2)
library(stringr)
library(data.table)

####################      HCBPS Final   | Microdata Processor Script   ###########

customDataSet <- data.frame(Age = character(), Education = character(), Percent = numeric(), Week = numeric())
customDataSet

processor <- function(week_num) {
  setwd(sprintf("../HPS-data/HPS_Week%s_PUF_CSV/", week_num)) 
  
  ####### Change pulse2020 <-> pulse2021 ###########
  govCovid = read.csv(sprintf("pulse2020_puf_%s.csv", week_num))
  head(govCovid) 
  
  #   Age	2021-Answer	          Education
  #   18-29	1992	2003          1) Less than high school
  #   30-39	1982	1991          2) High school or GED
  #   40-59	1962	1981          3) Some college/associate’s degree
  #   60+	1933	1961            4) Bachelor’s degree or higher
  
  #-99 Ignored
  #-88 Missing/Was not given question
  
  govCovid = govCovid %>% filter(DELAY != -88 & DELAY != -99)
  
  tabler <- function(x){
    return(
      x %>% 
        ####### Change years 2020 -> 10002, 2021 -> 21113 ###########
      group_by(Age = cut(TBIRTH_YEAR, breaks = c(1931, 1960, 1980, 1990, 2002)), Education =cut(EEDUC, breaks = c(0.9, 2, 3.9, 5, 5.9,7))) %>%
        summarise(Percent = (sum(EEDUC[DELAY == 1])/(sum(EEDUC[DELAY <= 2]))), Week = week_num)
    )
  }
  tab = tabler(govCovid)
  tab$Age <- as.character(tab$Age)
  tab$Education <- as.character(tab$Education)
  tab[tab == "(1.93e+03,1.96e+03]"] <- as.character("60+")  
  tab[tab == "(1.96e+03,1.98e+03]"] <- as.character("40-59")
  tab[tab == "(1.98e+03,1.99e+03]"] <- as.character("30-39")
  tab[tab == "(1.99e+03,2e+03]"] <- as.character("18-29")
  tab[tab == "(0.9,2]"] <- as.character("Less Than/Incomplete HS")  
  tab[tab == "(2,3.9]"] <- as.character("High School or GED")
  tab[tab == "(3.9,5]"] <- as.character("Some College/Assoc Degree")
  tab[tab == "(5.9,7]"] <- as.character("Bachelor’s Degree or Higher")
  tab
}

#####1-21 is 2020, 22-33 is 2021########
for (x in 1:21) {
  i <- sprintf("%02d", x)
  setwd(sprintf("../HPS-data/HPS_Week%s_PUF_CSV/", i))
  newdata <- processor(i)
  customDataSet <- rbind(customDataSet, newdata) 
}


write.csv(customDataSet,"../HPS-data/ageEduDataset", row.names = FALSE)


