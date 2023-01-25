#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/

# Load the required packages --------------------------------------------------
# install.packages("name") to install any missing packages
list_of_packages <- c("shiny", "shinydashboard", "tidyverse", "plotly",
                      "wordcloud", "DT", "ggplot2", "ggrepel", "here")
lapply(list_of_packages, library, character.only = TRUE)

# Source functions
# source(file = here("00_source_all.R"))

# Set hard-coded variables ----------------------------------------------------
# named vector for SDG colors
sdg_colors <- c("1" = "#E5243B", "2" = "#DDA63A", "3" = "#4C9F38", 
                "4" = "#C5192D", "5" = "#FF3A21", "6" = "#26BDE2",
                "7" = "#FCC30B", "8" = "#A21942", "9" = "#FD6925",
                "10" = "#DD1367", "11" = "#FD9D24", "12" = "#BF8B2E",
                "13" = "#3F7E44", "14" = "#0A97D9", "15" = "#56C02B",
                "16" = "#00689D", "17" = "#19486A")
sdg_names <- c("No Poverty", "Zero Hunger", 
               "Good Health and Well-Being", 
               "Quality Education", "Gender Equality", 
               "Clean Water and Sanitation", 
               "Affordable and Clean Energy", 
               "Decent Work and Economic Growth",
               "Industry, Innovation, and Infrastructure", 
               "Reduced Inequalities",
               "Sustainable Cities and Communities", 
               "Responsible Consumption and Production",
               "Climate Action", "Life Below Water", 
               "Life on Land", 
               "Peace, Justice and Strong Institutions", 
               "Partnerships for the Goals")
sdg_names <- paste(1:17, sdg_names)
sdg_choices <- 1:17
names(sdg_choices) <- sdg_names

# data
usc_pubs <- read.csv(here::here("data_processed/usc_pubs.csv"))
usc_sdgs <- read.csv(here::here("data_processed/usc_sdgs.csv"))
# usc_authors <- read.csv(here::here("data_processed/usc_authors.csv"))
# usc_bridge <- read.csv(here::here("data_processed/usc_bridge.csv"))

# 2020-2022
usc_pubs <- usc_pubs %>% filter(Year %in% c(2020, 2021, 2022))

# merge
usc_pubs_sdgs <- merge(usc_pubs, usc_sdgs, 
                       by.x = c("X", "Link"), by.y = c("document", "Link"), 
                       all.x = TRUE)

ui <- dashboardPage(
  # theme
  skin = "black",
  
  # Application title
  dashboardHeader(title = "USC SDG Mapping"),
  
  dashboardSidebar(
    sidebarMenu(
      #will eventually add Schools to sdgs and sdgs to schools
      menuItem("Home (About)", tabName = "1"),
      menuItem("Learn About The SDGs", tabName = "2"),
      menuItem("USC Research: SDGs by Year", tabName = "3"),
      menuItem("USC Research: SDGs by Department", tabName = "4"),
      menuItem("View USC Scholars and Departments by SDGs", tabName = "5"),
      menuItem("Find SDGs and Publications by USC Author", tabName = "6"),
      menuItem("FAQ", tabName = "7")
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$link( # link up css stylesheet
        rel="stylesheet", 
        type="text/css", 
        href="custom.css"
      )
    ),
    tabItems(
      tabItem(
        tabName = "1",
        fluidPage(
          h1("Home (Project Overview)"),
          h3(
            strong(
              "Are you interested in sustainability and the ",
              a(
                "UN Sustainability Development Goals (SDGs)",
                href="https://sdgs.un.org", 
                .noWS = "after"
              ),
              "?"),
            "If so, you have come to the right place! Right now, the results
            are from Scopus SDG Search Query. We are working on updating the
            dashboard with more accurate SDG classification using Machine 
            Learning.",
            br(), 
            br(),
            strong(
              "This dashboard is a tool that enables you to see which research
              publications at USC relate to the 16 UN SDGs (SDG 17 is not 
              included for now). You can use this dashboard as a tool to find 
              USC scholars and publications that match your academic interest!"
            ),
            br(),
            br(),
            "Sustainability incorporates protection for the environment,
            balancing a growing economy, and social responsibility to lead to 
            an improved quality of life for current and future generations."
          ), # end h3
          fluidRow(
            img(
              src="Asgmt_Earth_Research.png", 
              height="550", 
              style="display: block; margin-left: auto; margin-right: auto;"
            )
          )
        ) # end fluidPage
      ), # end tabItem 1
      tabItem(
        tabName = "2",
        fluidPage(
          h1("Learn About The SDGs"),
          h3(
            "SDG stands for", 
            a(
              "UN Sustainability Development Goals", 
              href="https://sdgs.un.org",
              .noWS = "after"
            ),
            ", which adopted by all United Nations Member States in 2015, 
            provides a shared blueprint for peace and prosperity for people and
            the planet, now and into the future. The SDGs are an urgent call
            for action by all countries - developed and developing - in a 
            global partnership. They recognize that ending poverty and other 
            deprivations must go hand-in-hand with strategies that improve 
            health and education, reduce inequality, and spur economic growth –
            all while tackling climate change and working to preserve our 
            oceans and forests."
          ),
          h4("*This app is a work in progress, and we are continually improving
             accuracy. If you have feedback, please email: oosdata@usc.edu"),
          div(
            style="font-size:24px;", 
            selectizeInput(
              inputId = "sdg_goal", 
              label = "Choose SDG",
              choices = sdg_choices
            )
          ),
          fluidRow(
            bootstrapPage(
              column(4, plotOutput(outputId ="plot3"), br()),
              column(4, plotOutput(outputId = "sdg_total_by_year"), br()),
              column(4, img(src = "un_17sdgs.jpg", width = "100%"))
            )
          )
          
          # h1(textOutput("sdg_name")),
          #fluidRow(bootstrapPage(
          # column(12, DT::dataTableOutput("top_classes_sdg_table"))
          #))
        ) # end fluidPage
      ), # end tabItem 2
      tabItem(
        tabName = "7",
        fluidPage(
          h1("FAQ"),
          h3(
            strong("Q: How was this dashboard created?"), br(),
            "A: This dashboard was created to engage the university community 
            and public with USC scholarly work that relates to sustainability, 
            specifically the 17 UN Sustainability Goals. The project was 
            initiated during the 2022 Fall ",
            a(
              "CKIDS Datafest",
              href = "https://sites.usc.edu/ckids/about/",
            ),
            "at USC by Dr. Julie Hopper in the Office of Sustainability and
            five USC students: Alison Chen, Aurora Massari, Bhavya Ramani, Ric
            Xian and Xinyi Zhang. USC publication data in this dashboard were 
            pulled from",
            a(
              "Scopus",
              href = "https://www.scopus.com/home.uri"
            ),
            "(Elsevier's citation database). All of the datasets, R-packages (",
            a(
              "text2sdg",
              href = "https://CRAN.R-project.org/package=text2sdg",
              .noWS = "outside"
            ),
            ") and code used in this dashboard are in ",
            a(
              "our Github page",
              href = "https://github.com/USC-Office-of-Sustainability/SDGMappingResearch",
              .noWS = "after"
            ),
            ".",
            br(), br(),
            strong("Q: What are the UN's 17 Sustainability Development Goals (SDGs)?"),
            br(),
            "A: The 2030 Agenda for Sustainable Development was adopted in 2015
            by all UN member states and provides a ‘blueprint for peace and 
            prosperity for people and the planet, now and into the future’. At 
            the center of this are the 17 Sustainable Development Goals (SDGs).
            These goals acknowledge that ending poverty and other deprivations
            must accompany strategies that reduce inequality, improve education
            and health, and spur economic growth – all while working to preserve
            our natural ecosystems and tackling climate change. To explore the 
            17 SDGs, please visit ",
            a(
              "their website",
              href = "https://sdgs.un.org/goals#icons",
              .noWS = "after"
            ),
            ".",
            br(), br(),
            strong("Q: How do I use this dashboard?"),
            br(),
            "A: You can choose your search function in the main menu in the 
            upper-left corner of this dashboard. Here you can explore what SDGs 
            (sustainability development goals) are being addressed by specific
            scholars, departments and years at USC. We hope that this dashboard
            helps to engage people in the research at USC and to find research
            groups that match their interests, as well as creating new research
            collaborations to further sustainability initiatives.",
            br(), br(),
            strong("Q: How do I get involved in research at USC?"),
            br(),
            "A: Here is our brief guide:",
            tags$ol(
              tags$li("Figure out what you are passionate about."),
              tags$ol(
                type = "a",
                tags$li("Think about which classes inspire you or what classes
                        you enjoyed. Browse the newspapers everyday to see what
                        articles pull your attention. What are the underlying 
                        themes of things in your everyday life that make you 
                        smile or grab your attention?")
              ),
              tags$li("Find what scholars at USC are doing research related to 
                      your passion (perhaps your passion is related to one of 
                      the 17 UN SDGs?!)"),
              tags$li("Browse the research profiles of these scholars by 
                      searching their names in the ",
                 a(
                   "USC Website Directory",
                   href = "https://uscdirectory.usc.edu/web/directory/web/",
                   .noWS = "after"
                 )),
              tags$ol( 
                type = "a",
                tags$li("Review their websites and their CVs"),
                tags$li("Read several of their recent publications and news 
                        press releases"),
                tags$ol( 
                  type = "i",
                  tags$li("You can also see their publications from 2020-22 in
                          this dashboard")
                )
              ),
              tags$li("Email the scholar (professor, graduate student or 
                      postdoc) with your interest"),
              tags$ol(
                type = "a",
                tags$li("Make sure that you express why you are interested in
                        their research and to inquire about how you can get 
                        involved. Be sure to include details about their 
                        research that you found exciting, but keep your email 
                        short. Be sure to provide your resume and your 
                        availability for the semester (hours/week that you 
                        could dedicate to working on their research project).")
              )
            ),
            br(),
            strong("Q: What if I have more questions or feedback?"),
            br(),
            "A: Please contact: oosdata@usc.edu"
          )
        ) # end fluidPage
      ) # end tabItem 7
    )
  )
)

server <- function(input, output, session) {
  # tab 2
  output$sdg_total_by_year  <- renderPlot(
    {
      # set correct column name
      sdg_col = sym(paste0("SDG.", input$sdg_goal))
      if (length(input$sdg_goal) < 2) {
        sdg_col = sym(paste0("SDG.0", input$sdg_goal))
      }
      # bar chart
      sdg_total_by_year <- usc_pubs_sdgs %>%
        filter(!!sdg_col == 1) %>%
        count(Year, !!sdg_col) %>%
        ggplot(aes(x = Year,y = n)) +
        geom_col(fill = sdg_colors[as.numeric(input$sdg_goal)], alpha = 1) +
        scale_color_manual(values = sdg_colors,
                           aesthetics = c("fill")) +
        #geom_text(aes(label = Freq), vjust = -0.2, size = 4) +
        labs(title = paste0("Count of Publications By Year"),
             fill = "SDG",
             x = "Year",
             y = "Count of Publications") +
        #guides(alpha = FALSE) +
        theme_minimal() +
        theme(text = element_text(size = 16))
      return(sdg_total_by_year)
    })
  
  output$plot3 <- renderImage(
    {
      # When input$n is 1, filename is ./images/image1.jpeg
      filename <- normalizePath(file.path("./www",
                                          paste("sdg", input$sdg_goal, ".png", sep="")))
      # Return a list containing the filename
      list(src = filename, height = "100%")
    }, deleteFile = FALSE)
  
  # 
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
    
    # pie(pie$vals, labels=paste(round(prop.table(vals)*100), "%", sep=""), 
    # col= c("#767676", "#990000", "#FFC72C"), radius=1)
    
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
      SDG_labels = as.character(1:16)
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
  observeEvent(
    input$Division,
    {
      updateSelectizeInput(session, 
                           "Primary.SDG",
                           server = TRUE,
                           choices = sort(pub_auth %>% 
                                            filter(Division == input$Division) %>% 
                                            select(Primary.SDG) %>% 
                                            distinct() %>% 
                                            pull()))
      #selected = unique(pub_auth %>% 
      #                    filter(Primary.SDG == input$Primary.SDG) %>% 
      #                    select(Division) %>% 
      #                    pull())[1])
    }
  )
  
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