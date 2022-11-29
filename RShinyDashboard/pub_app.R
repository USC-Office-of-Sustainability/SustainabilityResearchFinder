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
pubwithzero = read.csv("USC_SDG0to16.csv")

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
                                    tabItems(
                                      tabItem(tabName = "6",
                                              fluidPage(
                                                h1("Home (Project Overview)"),
                                                # fluidRow(
                                                h3(strong("Are you interested in sustainability and the ",
                                                          a("UN Sustainability Development Goals (SDGs)?", href="https://sdgs.un.org")), 
                                                   "If so, you have come to the right place! 
Right now, the results are from Scopus SDG Search Query. We are working on updating the dashboard with more accurate SDG classification using Machine Learning.
",br(), br(),strong("This 
                           dashboard is a tool that enables you to see which research publications at USC 
                           relate to the 16 UN SDGs (SDG 17 is not included for now). You can use this dashboard as a tool to find your authors and publications that match your academic interest!"),
                                                   br(),br(),"Sustainability incorporates protection for the environment, 
                           balancing a growing economy, and social responsibility to lead to an 
                           improved quality of life for current and future generations. Here, 
                           we have created a program in collaboration with Carnegie Mellon University 
                           to elevate awareness of sustainability in higher education." ),
                                                
                                                
                                                
                                                fluidRow(img(src="Asgmt_Earth_Research.png", height="550", style="display: block; margin-left: auto; margin-right: auto;"))
                                              )
                                      ), # end tab item 6
                                      tabItem(tabName = "4",
                                              fluidPage(
                                                h1("USC Research: SDGs By Year"),
                                                #h3("this is a description"),
                                                div(style="font-size:24px;",selectInput(inputId = "Year",
                                                                                        label = "Choose Year", choices = sort(unique(publications$Year)))), h3("Yearly Total Count of Publications By SDG"), 
                                                fluidRow(column(6, plotOutput("year_sdg_barplot"))),
                                                #h3("SDG Related Research vs. Non-related Research"),
                                                #fluidRow(column(12, plotOutput("pie1")))
                                                
                                                fluidRow(h3("SDG Related Research vs. Non-related Research"),
                                                         plotOutput("pie1"),
                                                         
                                                ))), # end tab item
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
    width = 600,
    height = 400,{
      year_sdg_barplot <- publications %>%
        filter(Year == input$Year) %>%
        count(Year,Primary.SDG ) %>%
        mutate(Freq = n) %>%
        ggplot(aes(x = Primary.SDG,y=Freq, fill = factor(as.numeric(Primary.SDG)))) +
        geom_col() +
        geom_text(aes(label = Freq), vjust = -0.2) +
        labs(title = paste0(" (", input$Year, ") ", "Count of Publications Per SDG"),
             fill = "SDG",
             x = "SDG",
             y = "Count of Publications") +
        guides(alpha = FALSE) +
        theme(text = element_text(size = 18)) 
      return(year_sdg_barplot)
    })
  
  output$pie1 <- renderPlot({
    pie_data <- pubwithzero %>% filter(Year %in% input$Year) 
    sum_notrelated = sum(is.na(pie_data$Primary.SDG))
    sum_focused = sum(!is.na(pie_data$Primary.SDG))
    vals=c(sum_notrelated, sum_focused)
    labels=c("Not Related", "Related")
    pie = data.frame(labels, vals)
    pie = data.frame(labels, vals)
    pie1 <- pie %>% 
      mutate(csum = rev(cumsum(rev(vals))), 
             pos = vals/2 + lead(csum, 1),
             pos = if_else(is.na(pos), vals/2, pos))
    # ggplot(pie, aes(x = "", y = vals, fill = labels)) +
    #   geom_col() +
    #   coord_polar(theta = "y")
    
    # pie(pie$vals, labels=paste(round(prop.table(vals)*100), "%", sep=""), col= c("#767676", "#990000", "#FFC72C"), radius=1)
    
    ggplot(pie, aes(x = "", y = vals, fill = labels)) +
      geom_col(color = "black") +
      # ggtitle("Title") +
      #geom_label_repel(data = pie1,
      #                 aes(y = pos, label = paste0(vals)),
      #                 size = 4.5, nudge_x = 1, show.legend = FALSE) +
      geom_text(aes(label = vals),
                position = position_stack(vjust = 0.5)) +
      coord_polar(theta = "y") +
      scale_fill_manual(values = c("#990000",
                                   "#FFC72C", "#767676")) +
      theme_void()
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
