###################################################################################
#Code Part 1
###################################################################################

## setwd("C:/Users/Thirty-Eight/Dropbox/PC/Documents/R/HCBPS-Final-Project/Datasets/PewScience") 
## PewScience = read.spss('PewScience.sav')
## head(PewScience)


## setwd("C:/Users/Thirty-Eight/Dropbox/PC/Documents/R/HCBPS-Final-Project/Datasets/Pew2") 
## pewMedia = read.spss('ATP W85.sav')
## head(pewMedia)
library(dplyr)
library(ggplot2)
library(stringr)
#library(ggcorrplot)
library(corrplot)
library(ggpubr)
library(tidyverse)
library(broom)
library(AICcmodavg)

############################ Load Data, Exploratory Plots ############################################
#setwd("C:/Users/Benjamin/Documents/Lawrenceville/shiny-reconstruct") 
govCovid = read.csv('Indicators_of_Reduced_Access_to_Care_Due_to_the_Coronavirus_Pandemic_During_Last_4_Weeks.csv')
head(govCovid)
govCovid$Time.Period.Start.Date


hist(govCovid$Time.Period, col = 'SKy Blue', main = 'Missed Covid Treatment Last 4 Weeks')
scatter.smooth(govCovid$Time.Period, y = govCovid$Value, xlab = "Days Since 5/5/2020", ylab = "Value", main = "Did Not Get or Delayed Covid Treatment")

table(govCovid$Subgroup)
barplot(eduCovid$Value, col = 'Blue')
plot(eduCovid$Value)


eduData = govCovid %>% filter(Group == "By Education")
head(eduData)
bachData = govCovid %>% filter(Subgroup == "Bachelor's degree or higher", Indicator == "Delayed Medical Care, Last 4 Weeks")
collegeData = govCovid %>% filter(Subgroup == "Some college/Associate's degree")
hsgedData = govCovid %>% filter(Subgroup == "High school diploma or GED")
lhsData = govCovid %>% filter(Subgroup == "Less than a high school diploma")
#arrange(bachData, by_group = TRUE)

scatter.smooth(bachData$Time.Period, y = bachData$Value, xlab = "Days Since 5/5/2020", ylab = "Value", main = "Did Not Get or Delayed Covid Treatment")
scatter.smooth(collegeData$Time.Period, y = collegeData$Value, xlab = "Days Since 5/5/2020", ylab = "Value", main = "Did Not Get or Delayed Covid Treatment")
scatter.smooth(hsgedData$Time.Period, y = hsgedData$Value, xlab = "Days Since 5/5/2020", ylab = "Value", main = "Did Not Get or Delayed Covid Treatment")
scatter.smooth(lhsData$Time.Period, y = lhsData$Value, xlab = "Days Since 5/5/2020", ylab = "Value", main = "Did Not Get or Delayed Covid Treatment")

############################ Separate By Subgroups ############################################

filterer <- function() {
  inType <- readline(prompt="Delayed Medical Care (1) OR Did Not Get Needed Care (2) OR Delayed or Did Not Get Care (3)? ")
  if (inType == "1"){
    indicator = "Delayed Medical Care, Last 4 Weeks"
  }
  else if (inType == "2"){
    indicator = "Did Not Get Needed Care, Last 4 Weeks"
  }
  else if (inType == "3"){
    indicator = "Delayed or Did Not Get Care, Last 4 Weeks"
  }
  else{
    break
  }
  subgroup <- readline(prompt="What is the group? ")
  return(govCovid %>% filter(Group == subgroup, Indicator == indicator))
}

scatterer <- function(){
  customData = filterer()
  customData$numYear = as.Date(as.character(customData$Time.Period.Start.Date))
  sp<-ggplot(customData, aes(x=numYear, y=Value, color=Subgroup)) + geom_point() + scale_x_continuous(breaks = c(1,5), limits = customData$Time.Period.Start.Date)
  sp
}

scatterer()

nationalEstimate = govCovid %>% filter(Group == "National Estimate", Indicator == "Delayed Medical Care, Last 4 Weeks")

ggplot(data = govCovid, aes(fill = Group)) + 
  geom_boxplot(mapping = aes(x = Group, y = Value))+
  stat_summary(mapping = aes(x = Group, y = Value), fun = mean, geom = "point", shape = 1, size = 3)+
  scale_y_continuous("Percentage with delayed access to care (%)")+
  theme_minimal()
fivenum(govCovid$Value)

################################## By State ##############################
library(usmap)

stat = govCovid %>% filter(Group == "By State")
stat
names(stat)[names(stat) == 'State'] <- 'state'

plot_usmap(data = stat, values = "Value", color = "black") + 
  scale_fill_continuous(low = "white", high = "red", name = "Percent Delayed Access To Medical Care.", label = scales::comma) + 
  
  theme(legend.position = "right", panel.background = element_rect(color = "black", fill = "lightblue"))


################################## By State ##############################
prop.test(x, n, p = NULL, alternative = "two.sided",
          correct = TRUE)


#########################Correlation Matrix ############################################
head(cormat)
setwd("../HPS-data")
cormat <- read.csv("CorrelatableData.csv", header = TRUE)

vars <- cormat[,c('18 - 29 years', '30 - 39 years', '40 - 49 years', '50 - 59 years', '60 - 69 years', '70 - 79 years', '80 years and above', 'Male',  
                  'Female',	'Hispanic or Latino', 'Non-Hispanic White, single race',	'Non-Hispanic Black, single race',	'Non-Hispanic Asian, single race',	
                  'Non-Hispanic, other races and multiple races',	'Less than a high school diploma',	'High school diploma or GED',	"Some college/Associate's degree",	
                  "Bachelor's degree or higher")]
corrplot.mixed(cor(mtcars))  #, order = "hclust", tl.col = "black"

corrplot.mixed(cor(mtcars), lower = 'square', upper = 'number', order = 'hclust')
corrplot(cor(cormat), method = 'color',tl.col = "black")

############################# Homogeneity Chi Squared ####################

setwd("C:/Users/Benjamin/Documents/Lawrenceville/shiny-reconstruct/")
xsq <- read.csv("chisquarabledata.csv", header = TRUE)

#Male/Female
malefemale <- rbind(xsq[9,], cormat[10,])
chisq.test(malefemale)

#Old/Young
oldyoung <- rbind(cormat[1,], cormat[7,])
chisq.test(oldyoung)

############################ Loading Custom Data By Age and Education ############################################

setwd("C:/Users/Benjamin/Documents/Lawrenceville/shiny-reconstruct/HPS-data") 
newgovCovid = read.csv('ageEduDataset')
head(newgovCovid, n = 20)
eduData1 = newgovCovid %>% filter(Education == "Less than high school")
eduData2 = newgovCovid %>% filter(Education == "Some high school")
eduData3 = newgovCovid %>% filter(Education == "High school graduate or equivalent (for example GED)")
eduData4 = newgovCovid %>% filter(Education == "Some college, but degree not received or is in progress")
eduData5 = newgovCovid %>% filter(Education == "Associate’s degree (for example AA, AS)")
eduData6 = newgovCovid %>% filter(Education == "Bachelor's degree (for example BA, BS, AB)")
eduData7 = newgovCovid %>% filter(Education == "Graduate degree (for example master's, professional, doctorate)")

ggplot() + 
  geom_point(data=eduData1, aes(x=Week, y=Percent), color='green') + 
  geom_point(data=eduData2, aes(x=Week, y=Percent), color='red') +
  geom_point(data=eduData3, aes(x=Week, y=Percent), color='blue') + 
  geom_point(data=eduData4, aes(x=Week, y=Percent), color='orange')  
ggplot() +
  geom_point(data=eduData2, aes(x=Week, y=Percent), color='purple') +
  geom_point(data=eduData3, aes(x=Week, y=Percent), color='yellow') +
  geom_point(data=eduData4, aes(x=Week, y=Percent), color='pink')


############################# Two-Way ANOVA ####################

hist(newgovCovid$Percent, col = 'Sky Blue', main = 'Data by Education and Age', xlab = 'Percent Access') 
#Normal

ages  <- subset (newgovCovid, select = -Education)
educations <- subset (newgovCovid, select = -Age)

plot(lm(Percent~Education,data= educations))
plot(lm(Percent~Age,data= ages))
plot(lm(Percent, data = newgovCovid))
#NPP Linear

var.test(ages, educations, alternative = "two.sided")

eduAgeInteraction <- aov(Percent ~ Age*Education, data = newgovCovid)
summary(eduAgeInteraction)

