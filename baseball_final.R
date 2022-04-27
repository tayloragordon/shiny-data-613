# *************** DATA 613 R-Shiny Project: Taylor Gordon & Olivia Mahony ****************

#In this shiny application, we presented MLB team data overall for the 2021 season. 
#It contains 8 datasets, all created from one found via webscraping the MLB official website. 
#Our reactive elements are: checked box buttons, variable selection, and the use of tabs. 
#We made use of histograms, boxplots, and dataframes to visually examine the overall stats of the 2021 MLB season, 
#eventually breaking down the data into the final four teams. 

# **************** Libraries ****************
library(shinythemes)
library(shinyWidgets)
library(tidyverse)
library(rvest)
library(dplyr)
library(repurrrsive)
library(shiny)

# **************** Load in data ****************

#Webscraping the data from MLB website 
wikiurl <- read_html("https://www.mlb.com/stats/team/2021")
teams2021 <- wikiurl%>%
  html_table(., fill = T)
teams2021[[1]] -> BBTEAM2021

#All teams dataframe
#Change column names and team names so they're not double 
BBTEAM2021%>%
  rename(TEAM = TEAMTEAM)%>%
  rename(LEAGUE = LEAGUELEAGUE)%>%
  rename(G = GG)%>%
  rename(AB = ABAB)%>%
  rename(R = RR)%>%
  rename(H = HH)%>%
  rename(AVG = AVGAVG)%>%
  select(TEAM, LEAGUE, G, AB, R, H, AVG)-> BASE2021
BASE2021 <- data.frame(BASE2021)

#National League V American League 
NL <- BASE2021%>%
  filter(LEAGUE == "NL")
NL <- data.frame(NL)
AL <- BASE2021 %>%
  filter(LEAGUE == "AL")
AL <- data.frame(AL)

#Final 4 Teams 
atl <- BASE2021[8,]
bos <- BASE2021[3,]
la <- BASE2021[6,]
hou <- BASE2021[2,]

#For histogram1 - NO TEAM or LEAGUE or G VARIABLE 
graph_data <- BASE2021 %>%
                  select(AB, R, H, AVG)

# **************** Create user interface *****************

ui <- navbarPage("2021 MLB Data", #use navbarpage for cleaner tabs 
                 theme = shinytheme("spacelab"),
                 setBackgroundColor("skyblue", gradient = "linear", direction = "top"),
                 #MLB LOGO
                 tags$div(tags$img(src="mlb_logo.png", width = 108, height = 108, style="float:left; margin-left: 5px; margin-right: 5px; margin-top: -15px")),
                 #Set up tabs for the data to be in 
                 tabPanel("Intro",
                          sidebarLayout(
                            sidebarPanel("About this app"),
                            mainPanel(
                              # p("p creates a paragraph of text."),
                              p("We thought to bring Baseball data to our Shiny App.  Baseball is such a data driven sport in our world right now, 
                       and sometimes it is hard to fully understand its effectiveness."),
                              br(),
                              p("We hope that when looking at our Shiny App, people are able to better see what matters through data driven imaging."),
                              br(),
                              p("When looking at the variables, AB = at bats, which is the amount of times a team gets up to bat. 
                       G means games played. R means runs, which is points scored/ people running through home plate. 
                       H means hits, hits are considered when the person hitting reaches a base. AVG means batting average for the team."),
                              br(),
                              p("This data is web-scraped from MLB website for the 2021 season.  
                       Overall we hope to help the average baseball fan or anyone to be able to better interpret the data from MLB 2021 season."),
                              br(),
                              p("Developed by: Taylor Gordon and Olivia Mahony.")))),
                 tabPanel("All Teams",
                          mainPanel(
                            tabsetPanel(
                              tabPanel("All Teams Stats 2021",
                                sidebarLayout(
                                  sidebarPanel(
                                    checkboxGroupInput("show_vars","Select stats to show:",
                                                     names(BASE2021), selected = names(BASE2021))),
                                mainPanel(h3("All MLB Teams (2021)"), DT::dataTableOutput(("allmlbdata")))
                              )),
                              tabPanel("Boxplot",
                                  sidebarPanel(
                                    selectInput("mod_vars", "Select Variable:",
                                                  choices=colnames(graph_data))),
                                  mainPanel(
                                    h3("MLB Data Boxplot"), plotOutput(outputId = "box1")
                                  )),
                              tabPanel("Histogram", 
                                       sidebarPanel(
                                         selectInput("mod_vars2", "Select Variable:",
                                                     choices= colnames(graph_data))),
                                       mainPanel(
                                         h3("MLB Data Histogram"), plotOutput(outputId = "hist1")
                                       ))))),
                 tabPanel("Final 4", 
                          mainPanel(
                            tabsetPanel(
                              tabPanel("Atlanta Braves",
                                       sidebarPanel(
                                         selectInput("atl", "Select Variable:",
                                                     choices=colnames(atl))),
                                       mainPanel(
                                         h3("Braves Season Data"), DT::dataTableOutput("atl"))
                                       ),
                              tabPanel("Houston Astros",
                                       sidebarPanel(
                                         selectInput("hou", "Select Variable:",
                                                     choices=colnames(hou))),
                                       mainPanel(
                                         h3("Astros Season Data"), DT::dataTableOutput("hou"))
                                       ),
                              tabPanel("Boston Red Sox", 
                                       sidebarPanel(
                                         selectInput("bos", "Select Variable:",
                                                     choices= colnames(bos))),
                                       mainPanel(
                                         h3("Red Sox Season Data"), DT::dataTableOutput("bos"))
                                       ),
                              tabPanel("Los Angeles Dodgers",
                                       sidebarPanel(
                                         selectInput("la", "Select Variable:",
                                                     choices= colnames(la))),
                                       mainPanel(
                                         h3("Dodgers Season Data"), DT::dataTableOutput("la")
                                         )
                )
            )
        )
    )
)
                 
                              

# Define server logic required to draw a histogram
server <- function(input, output) {
  #Show the BASE 2021 data 
  output$allmlbdata <- DT::renderDataTable({
    DT::datatable(BASE2021[, input$show_vars, drop = FALSE])
  })
  
  #Boxplot all teams 
  output$box1 <- renderPlot({
    ggplot(graph_data, aes(y = .data[[input$mod_vars]])) +
      geom_boxplot(fill = "purple") +
      ggtitle("All Teams")
    
  })
  #Histogram all teams 
  output$hist1 <- renderPlot({
    ggplot(graph_data, aes(x =.data[[input$mod_vars2]])) +
      geom_histogram(fill="red", color="black") +
      ggtitle("All Teams")
  })
  
  #Final 4 
  output$atl <- DT::renderDataTable({
    DT::datatable(atl[, input$show_vars, drop = FALSE])
  })
  output$bos <- DT::renderDataTable({
    DT::datatable(bos[, input$show_vars, drop = FALSE])
  })
  output$hou <- DT::renderDataTable({
    DT::datatable(hou[, input$show_vars, drop = FALSE])
  })
  output$la <- DT::renderDataTable({
    DT::datatable(la[, input$show_vars, drop = FALSE])
  })
  
   
}

# Run the application 
shinyApp(ui = ui, server = server)
