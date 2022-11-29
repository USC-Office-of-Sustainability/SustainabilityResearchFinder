#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
library(tidyverse)
library(shiny)
library(shinydashboard)
# install.packages("plotly")
library(plotly)
# install.packages("wordcloud")
library(wordcloud)
# install.packages("DT")
library(DT)
# install.packages("ggplot2")
library(ggplot2)
# install.packages("ggrepel")
library(ggrepel)

library(shiny)

publications = read.csv("USC_SDG1to16.csv")
authors = read.csv("USCauthorsSDG1to16.csv")
authorChoices = setNames(authors$ID, authors$Name)
pub_auth = read.csv("USCpubauthfullinfoSDG1to16.csv")

# Define UI for application that draws a histogram
ui <- dashboardPage( skin="black",
                     
                     # Application title
                     dashboardHeader(title = "USC SDG Mapping"),
                     
                     
                     dashboardSidebar(
                       sidebarMenu( #will eventually add Schools to sdgs and sdgs to schools
                         menuItem("Home (About)", tabName = "6"), #Ric
                         menuItem("Learn About The SDGs", tabName = "5"),#Bhavya
                         menuItem("USC Research: SDGs by Year", tabName = "4"),#Ric
                         menuItem("USC Research: SDGs by Department", tabName = "3"), #Xinyi
                         menuItem("View USC Scholars and Departments by SDGs", tabName = "2"), #Aurora
                         menuItem("Find SDGs and Publications by USC Author", tabName = "1") #Alison
                       )
                     ),
                     dashboardBody( tags$head(tags$link(rel="stylesheet", type="text/css", href="custom.css")), #link up css stylesheet
                                    tabItems(tabItem(tabName = "2",
                                                     fluidPage(
                                                       h1("USC Research: SDGs By Year"),
                                                       #h3("this is a description"),
                                                       div(style="font-size:24px;",selectInput(inputId = "Year",
                                                                                               label = "Choose Year",
                                                                                               # selected = "AY19",
                                                                                               choices = sort(unique(publications$Year)))),
                                                       
                                                       # h3("Not including multiple sections"),
                                                       # plotOutput("pie1"), 
                                                       # textOutput("pie1_numbers")
                                                       
                                                       # fluidRow(
                                                       #   h3("Including multiple sections"),
                                                       #   plotOutput("pie2"),
                                                       #   textOutput("pie2_numbers")
                                                       # ),
                                                       # fluidRow(
                                                       #   h3("By department"),
                                                       #   plotOutput("pie3"),
                                                       #   textOutput("pie3_numbers")
                                                       # ),
                                                       
                                                       h3("Yearly Total Count of Publications By SDG"),
                                                       fluidRow(column(6, plotOutput("year_sdg_barplot"))),
                                                       
                                                     ) # end fluid page
                                    ), # end tab item
                                    tabItem(tabName = "1",
                                            fluidPage(
                                              h1("Find SDGs and Publications by USC Author"),
                                              div(style="font-size:24px;", selectInput(inputId = "usc_author",
                                                                                       label = "Choose USC Author",
                                                                                       choices = authorChoices
                                              )),
                                              fluidRow(column(12, DT::dataTableOutput("auth_about"))),
                                              # graph
                                              h3("Graph of Author's Publications by SDG"),
                                              fluidRow(column(12, plotOutput("author_sdg_barplot"))),
                                              # table
                                              h3("List of Author's Publications"),
                                              fluidRow(bootstrapPage(
                                                column(12, DT::dataTableOutput("author_pub_table"))
                                              ))
                                              
                                            )) # end tab item
                                    )
                     ))

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$year_sdg_barplot <- renderPlot(
    width = 800,
    height = 550,{
      year_sdg_barplot <- publications %>%
        filter(Year == input$Year) %>%
        count(Year,Primary.SDG ) %>%
        mutate(Freq = n) %>%
        ggplot(aes(x = Primary.SDG,y=Freq, fill = factor(as.numeric(Primary.SDG)))) +
        geom_col() +
        geom_text(aes(label = Freq), vjust = -0.2) +
        # geom_hline(yintercept = c(10, 15), color = c("#ffc33c", "#00bc9e")) +
        labs(title = paste0(" (", input$Year, ") ", "Count of Publications Per SDG"),
             fill = "SDG",
             x = "SDG",
             y = "Count of Publications") +
        guides(alpha = FALSE) +
        theme(text = element_text(size = 18)) #+
      #scale_fill_manual(values = sdg_class_keyword_colors)
      
      return(year_sdg_barplot)
    })

  # for find SDGs and pub by auth
  output$auth_about <- DT::renderDataTable({
    authors %>%
      filter(authors$InUSCDirectory & authors$ID == input$usc_author) %>%
      select(FName, LName, Department, Division, Email, PositionTitle)
  }, options =
    list(searching = FALSE, paging = FALSE,
         language = list(
           zeroRecords = "Not a current USC faculty/staff"
         )))
  
  output$author_sdg_barplot <- renderPlot(
    {
      author_sdg_barplot <- pub_auth %>%
        filter(pub_auth$AuthorId == input$usc_author) %>%
        count(Primary.SDG) %>%
        ggplot(aes(x = as.factor(Primary.SDG), y = n)) + 
        geom_col() +
        coord_flip() +
        scale_y_continuous(breaks = scales::pretty_breaks()) +
        labs(title = names(input$usc_author),
             x = "SDG",
             y = "Number of Publications")
      return(author_sdg_barplot)
    }
  )
  
  output$author_pub_table <- DT::renderDataTable({
    pub_auth %>%
      filter(pub_auth$AuthorId == input$usc_author) %>%
      select(Primary.SDG, Titles, Link)
  }, rownames=FALSE)
}

# Run the application 
shinyApp(ui = ui, server = server)
