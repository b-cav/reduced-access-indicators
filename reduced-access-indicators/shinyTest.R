library(shiny)
library(dplyr)
library(ggplot2)
library(stringr)
library(ggpubr)
library(tidyverse)
library(broom)
library(usmap)
library(AICcmodavg)
library(corrplot)
library(shinythemes)


govCovid = read.csv('../Indicators_of_Reduced_Access_to_Care_Due_to_the_Coronavirus_Pandemic_During_Last_4_Weeks.csv')
names(govCovid)[names(govCovid) == 'State'] <- 'state'
cormat <- read.csv("../Indicators_of_Reduced_Access_to_Care_Due_to_the_Coronavirus_Pandemic_During_Last_4_Weeks.csv", header = TRUE)
vars <- c('18 - 29 years', '30 - 39 years', '40 - 49 years', '50 - 59 years', '60 - 69 years', '70 - 79 years', '80 years and above', 'Male',  
          'Female',	'Hispanic or Latino', 'Non-Hispanic White, single race',	'Non-Hispanic Black, single race',	'Non-Hispanic Asian, single race',	
          'Non-Hispanic, other races and multiple races',	'Less than a high school diploma',	'High school diploma or GED',	"Some college/Associate's degree",	
          "Bachelor's degree or higher")

ui <- fluidPage(theme = shinytheme("superhero"),
                titlePanel("Delayed Access to Medical Treatment during the Covid-19 Pandemic by Identifiers"),
                plotOutput("accessHist"),
                hr(),
                fluidRow(
                  column(3,
                         selectInput("subgroup", strong("Grouping"), 
                                     choices= unique(govCovid$Group),selected = 'By State'),
                         checkboxInput("CIBars", "Show 95% Confidence Intervals", value = FALSE),
                         h5("Time Frame"),
                         sliderInput("dates", label = h5("(Collection Phases Since 04/23/20)"), min = 1, 
                                     max = 30, value = c(1, 30)),
                         tableOutput("fivenumsum"),
                         uiOutput("source")
                         
                  ),
                  column(8, offset = 1,
                         h4("Subgroup Correlation Matrix"),
                         plotOutput("corrplot")
                  )
                )
)

server <- function(input, output) {
  graphdata <- reactive({govCovid %>% filter(Group == input$subgroup & Indicator == "Delayed Medical Care, Last 4 Weeks" & Time.Period >= input$dates[1] & Time.Period <= input$dates[2])})
  state <- reactive({input$subgroup})
  output$accessHist<- renderPlot({
    if(paste(state()) %in% "By State"){
      plot_usmap(data = graphdata(), values = "Value", color = "black") + 
        scale_fill_continuous(low = "white", high = "red", name = "Percent Delayed Access To Medical Care.", label = scales::comma) + 
        theme(legend.position = "right", panel.background = element_rect(color = "black", fill = "lightblue"))
    }
    else{
      output$accessHist<-renderPlot({
        ggplot(graphdata(), aes(x=Time.Period, y=Value, color=Subgroup)) + geom_point() + xlab("Weeks Since 04/23/20") +
          ylab("Percentage with Delayed Access to Medical Care")+ theme_minimal()+
          if(!isFALSE(input$CIBars)){
            geom_errorbar(aes(ymin = Low.CI, ymax= High.CI), width=.3)
          }
      })
    }
  })
  output$corrplot <- renderPlot({corrplot(cor(cormat), method = 'color',tl.col = "black")  })
  dataLink <- a("Household Pulse Survey", href="https://www.census.gov/programs-surveys/household-pulse-survey/datasets.html/")
  output$source <- renderUI({
    tagList("Data Sourced From the Census Bureau's", dataLink)
  })
  output$fivenumsum <- renderTable({
    dt <- cbind(c("Min", "Q1","Med", "Q3", "Max", "Mean", "StdDev"), c(fivenum(graphdata()$Value), format(round(mean(graphdata()$Value), 1), nsmall = 1), format(round(sd(graphdata()$Value), 1), nsmall = 1)))
    colnames(dt)<- c("Summary Statistics", " ")
    dt
  },include.rownames=FALSE)
}

shinyApp(ui = ui, server = server)


