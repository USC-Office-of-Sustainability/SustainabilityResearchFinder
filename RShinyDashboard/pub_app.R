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

# remove authors not in USC Directory
authors = read.csv("USCauthorsSDG1to16.csv")
#authors = authors[authors$InUSCDirectory,]
authorChoices = setNames(authors$ID, authors$Name)
pub_auth = read.csv("USCpubauthfullinfoSDG1to16.csv")
#pub_auth = pub_auth_all[pub_auth_all$InUSCDirectory,]
# only 2020-2022
pub_auth = pub_auth[pub_auth$Year %in% c(2020,2021,2022),]
publications = read.csv("USC_SDG0to16.csv")
publications = publications[publications$Year %in% c(2020,2021,2022),]
# named vector for SDG colors
sdg_colors <- c('1' = '#E5243B', '2' = '#DDA63A', '3' = '#4C9F38', '4' = '#C5192D', '5' = '#FF3A21', '6' = '#26BDE2',
                '7' = '#FCC30B', '8' = '#A21942', '9' = '#FD6925', '10' = '#DD1367', '11' = '#FD9D24', '12' = '#BF8B2E',
                '13' = '#3F7E44', '14' = '#0A97D9', '15' = '#56C02B', '16' = '#00689D', '17' = '#19486A')

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
                           relate to the 16 UN SDGs (SDG 17 is not included for now). You can use this dashboard as a tool to find USC scholars and publications that match your academic interest!"),
                                                   br(),br(),"Sustainability incorporates protection for the environment, 
                           balancing a growing economy, and social responsibility to lead to an 
                           improved quality of life for current and future generations." ),
                                                
                                                
                                                
                                                fluidRow(img(src="Asgmt_Earth_Research.png", height="550", style="display: block; margin-left: auto; margin-right: auto;"))
                                              )
                                      ), # end tab item 6
                                      tabItem(tabName = "5",
                                              fluidPage(
                                                h1("Learn About The SDGs"),
                                                h3("SDG stands for", a("UN Sustainability Development Goals", href="https://sdgs.un.org"),", which adopted by all United Nations Member States in 2015, provides a shared blueprint for peace and prosperity for people and the planet, now and into the future. The SDGs are an urgent call for action by all countries - developed and developing - in a global partnership. They recognize that ending poverty and other deprivations must go hand-in-hand with strategies that improve health and education, reduce inequality, and spur economic growth – all while tackling climate change and working to preserve our oceans and forests."),
                                                
                                                div(style="font-size:24px;", selectizeInput(inputId = "sdg_goal", 
                                                                                            label = "Choose SDG", 
                                                                                            choices = sort(unique(publications$Primary.SDG))
                                                )),
                                                fluidRow(bootstrapPage(
                                                  column(4, plotOutput(outputId ="plot3"), br()),
                                                  column(4, plotOutput(outputId = "sdg_total_by_year"), br()),
                                                  column(4, img(src = "un_17sdgs.jpg", width = "100%"))
                                                )),
                                                
                                                # h1(textOutput("sdg_name")),
                                                #fluidRow(bootstrapPage(
                                                # column(12, DT::dataTableOutput("top_classes_sdg_table"))
                                                #))
                                              )
                                      ),# end tab item 5
                                      tabItem(tabName = "4",
                                              fluidPage(
                                                h1("USC Research: SDGs By Year"),
                                                #h3("this is a description"),
                                                div(style="font-size:24px;",selectInput(inputId = "Year",
                                                                                        label = "Choose Year", choices = sort(unique(publications$Year)))), h3("Yearly Total Count of Publications By SDG"), 
                                                fluidRow(column(6, plotOutput("year_sdg_barplot"))),
                                                #h3("SDG Related Research vs. Non-related Research"),
                                                #fluidRow(column(12, plotOutput("pie1")))
                                                
                                                fluidRow(column(6, h3("SDG Related Research vs. Non-related Research"),
                                                         plotOutput("pie1"),)
                                                         
                                                ))), # end tab item
                                      tabItem(tabName = "3",
                                              fluidPage(
                                                h1("USC Research: SDGs by Department"),
                                                h3("Select a USC School below to view the number of SDG-related publications by departments."),
                                                div(style="font-size:24px;", selectInput(inputId = "usc_division",
                                                                                         label = "Choose USC School",
                                                                                         selected = "Dornsife College of Letters, Arts and Sciences",
                                                                                         choices = unique(pub_auth$Division))),
                                                h3("(Top 30) Departments of SDG Publications"),
                                                fluidRow(column(12, plotOutput(outputId = "pubs_to_bar"))),
                                                h3("SDG-Related Research"),
                                                fluidRow(column(12, plotOutput("pubs_to_pie")))
                                                
                                              ))
                                    ,#end tabitem 
                                    tabItem(tabName = "2",
                                            fluidPage(
                                              h1("Find Top Scholars and Departments by SDGs"),
                                              #h3("description"),
                                              div(style="font-size:24px;", selectInput(inputId = "Primary.SDG", 
                                                                                       label = "Choose SDG", 
                                                                                       choices = sort(unique(pub_auth$Primary.SDG)))),
                                              div(style="font-size:24px;", selectizeInput(inputId = "Division", 
                                                                                          label = "Select USC School", 
                                                                                          choices = NULL)), br(),
                                              #h1(textOutput(paste0("Top Researchers in", input$Division))),
                                              fluidRow(bootstrapPage(
                                                column(6, plotOutput(outputId = "top_authors_sdg_table"), br()),
                                                column(6, img(src = "un_17sdgs.jpg", width = "100%")))), br(),
                                              #h1(textOutput(paste0("Top Departments in ", input$Division))),
                                              fluidRow(bootstrapPage(
                                                column(6, plotOutput(outputId = "top_departments_sdg_table"))))
                                            )), #end tabitem
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
                                              fluidRow(column(6, plotOutput("author_sdg_barplot"))),
                                              # table
                                              h3("List of Author's Publications"),
                                              fluidRow(bootstrapPage(
                                                column(12, DT::dataTableOutput("author_pub_table"))
                                              ))
                                              
                                            )) # end tab item
                                    )
                     ))

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  # ric
  output$sdg_total_by_year  <- renderPlot(
    #width = 380,
    #height = 500,
    {
      sdg_total_by_year <- publications %>%
        filter(!is.na(Primary.SDG)) %>%
        filter(Primary.SDG == input$sdg_goal) %>%
        count(Year,Primary.SDG) %>%
        mutate(Freq = n) %>%
        ggplot(aes(x = Year,y=Freq)) +
        geom_col(fill = sdg_colors[as.numeric(input$sdg_goal)], alpha = 1) +
        scale_color_manual(values = sdg_colors,
                           aesthetics = c("fill")) +
        geom_text(aes(label = Freq), vjust = -0.2, size = 4) +
        labs(title = paste0("Count of Publications By Year"),
             fill = "SDG",
             x = "Year",
             y = "Count of Publications") +
        guides(alpha = FALSE) +
        theme_minimal() +
        theme(text = element_text(size = 16))
      return(sdg_total_by_year)
    })
  
  output$plot3 <- renderImage(
    {
      # When input$n is 1, filename is ./images/image1.jpeg
      filename <- normalizePath(file.path('./www',
                                          paste('sdg', input$sdg_goal, '.png', sep='')))
      # Return a list containing the filename
      list(src = filename, height = "100%")
    }, deleteFile = FALSE)

  output$year_sdg_barplot <- renderPlot(
    #width = 600,
    #height = 400,
    {
      year_sdg_barplot <- publications %>%
        filter(!is.na(Primary.SDG)) %>%
        filter(Year == input$Year) %>%
        count(Year,Primary.SDG ) %>%
        mutate(Freq = n) %>%
        ggplot(aes(x = Primary.SDG,y=Freq, fill = factor(as.numeric(Primary.SDG)))) +
        geom_col() +
        scale_color_manual(values = sdg_colors,
                           aesthetics = c("fill")) +
        geom_text(aes(label = Freq), vjust = -0.2) +
        labs(title = paste0(" (", input$Year, ") ", "Count of Publications Per SDG"),
             fill = "SDG",
             x = "SDG",
             y = "Count of Publications") +
        guides(alpha = FALSE) +
        theme_minimal() +
        theme(text = element_text(size = 18))
        return(year_sdg_barplot)
    })
  
  output$pie1 <- renderPlot({
    pie_data <- publications %>% filter(Year %in% input$Year) 
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
                position = position_stack(vjust = 0.5),
                size = 10) +
      coord_polar(theta = "y") +
      scale_fill_manual(values = c("#990000",
                                   "#FFC72C", "#767676")) +
      theme_void() +
      theme(text = element_text(size = 18))
  })
  
  # xinyi
  output$pubs_to_bar <- renderPlot(
    #width = 800,
    #height = 600,
    {
      pubs_to_bar <- pub_auth %>%
        filter(Division == input$usc_division) %>%
        count(Department,Primary.SDG) %>%
        #group_by(Department) %>%
        mutate(Freq = n) %>%
        #arrange(Department,desc(n)) %>%
        #arrange(desc(sum(Freq))) %>%
        #ungroup() %>%
        #distinct(Department, .keep_all = TRUE) %>%
        #head(30) %>% #  num_top_classes <- 10
        ggplot(aes(x = Department, y = Freq, fill = factor(as.numeric(Primary.SDG)))) +
        #geom_col(position = "stack") +
        geom_col() +
        coord_flip()+
        scale_color_manual(values = sdg_colors,
                           aesthetics = c("fill"))+
        #geom_text(aes(label = Freq), vjust = -0.2) #+
        labs(#title = paste0("Count of Publications Per SDG"),
          fill = "SDG",
          x = "Departments",
          y = "Count of Publications") +
        #guides(alpha = FALSE) +
        theme(text = element_text(size = 8)) 
      return (pubs_to_bar)
    }
  )
  output$pubs_to_pie <- renderPlot(
    #width = 600,
    #height = 400,
    {
      pie_data <- pub_auth %>% filter(Division %in% input$usc_division) 
      vals = c()
      for (i in 1:16) {
        vals = c(vals, sum(pie_data$Primary.SDG == i))
      }
      SDG_labels= c('1', '2', '3', '4', '5', '6', '7', '8', '9', '10', '11', '12', '13', '14', '15', '16')
      pie = data.frame(SDG_labels, vals)
      pubs_to_pie <- pie %>% 
        mutate(csum = rev(cumsum(rev(vals))), 
               pos = vals/2 + lead(csum, 1),
               pos = if_else(is.na(pos), vals/2, pos))
      
      
      ggplot(pie, aes(x = "", y = vals, fill = factor(as.numeric(SDG_labels)))) +
        geom_col(color = "black") +
        #ggtitle("SDG-Related Publications") +
        #geom_label_repel(data = pubs_to_pie,
        #aes(y = pos, label = paste0(vals)),
        #size = 4.5, nudge_x = 1, show.legend = FALSE) +
        #geom_text(aes(label = vals),
        #position = position_stack(vjust = 0.5),) +
        labs(fill = "SDG",
             x = "",
             y = "") +
        coord_polar(theta = "y") +
        scale_color_manual(values = sdg_colors,
                           aesthetics = c("fill")) +
        theme_void() + 
        theme(text = element_text(size = 18))
      
    })
  
  # aurora
  observeEvent(input$Primary.SDG,
               {
                 updateSelectizeInput(session, "Division",
                                      server = TRUE,
                                      choices = sort(pub_auth %>% filter(Primary.SDG == input$Primary.SDG) %>% select(Division) %>% distinct() %>% pull()))#,
                 #selected = unique(pub_auth %>% filter(Primary.SDG == input$Primary.SDG) %>% select(Division) %>% pull())[1])
               })
  
  output$top_authors_sdg_table <- renderPlot({ # from goals_to_classes
    top_authors_sdg_table <-  pub_auth %>%
      filter(Primary.SDG == input$Primary.SDG) %>%
      filter(Division == input$Division) %>%
      count(AuthorId, Name, Division) %>%
      mutate(Freq = n) %>%
      arrange(desc(n)) %>%
      distinct(Name, .keep_all = TRUE) %>%
      head(10) %>% #  num_top_classes <- 10
      ggplot(aes(x = reorder(as.factor(Name),n), y = n)) + 
      geom_col(fill = sdg_colors[as.numeric(input$Primary.SDG)], alpha = 1) +
      coord_flip() +
      labs(title = paste0("Top Authors that Map to SDG #", input$Primary.SDG),
           x = "Scholar",
           y = "Number of Publications ") +
      theme(text = element_text(size = 20))
    return(top_authors_sdg_table)
  })
  
  output$top_departments_sdg_table <- renderPlot({
    top_departments_sdg_table <-  pub_auth %>%
      filter(Primary.SDG == input$Primary.SDG) %>%
      filter(Division == input$Division) %>%
      count(Department) %>%
      mutate(Freq = n) %>%
      arrange(desc(n)) %>%
      distinct(Department, .keep_all = TRUE) %>%
      head(10) %>%
      ggplot(aes(x = reorder(as.factor(Department),n), y = n)) + 
      geom_col(fill = sdg_colors[as.numeric(input$Primary.SDG)], alpha = 1) +
      coord_flip() +
      labs(title = paste0("Top Departments that Map to SDG #", input$Primary.SDG),
           x = "Departments",
           y = "Number of Publications ") +
      theme(text = element_text(size = 20))
    return(top_departments_sdg_table)
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
        ggplot(aes(x = factor(Primary.SDG), y = n, fill = factor(Primary.SDG))) + 
        geom_col() +
        scale_color_manual(values = sdg_colors,
                           aesthetics = c("fill")) +
        coord_flip() +
        scale_y_continuous(breaks = scales::pretty_breaks()) +
        labs(title = names(input$usc_author),
             x = "SDG",
             y = "Number of Publications",
             fill = "SDG") +
        theme_minimal() +
        theme(text = element_text(size = 18))
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
