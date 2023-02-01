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
                      "wordcloud", "DT", "ggplot2", "ggrepel", "here", 
                      "reshape2", "scales")
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
sdg_col_names <- syms(c("SDG.01", "SDG.02", "SDG.03", "SDG.04", "SDG.05", "SDG.06", 
                   "SDG.07", "SDG.08", "SDG.09", "SDG.10", "SDG.11", "SDG.12", 
                   "SDG.13", "SDG.14", "SDG.15", "SDG.16", "SDG.17"))

disclaimer = paste("Data is from 2020-2022. This app is a work in progress, and,",
                 "we are continually improving accuracy. If you have feedback,",
                 "please email: oosdata@usc.edu")

# data
usc_pubs <- read.csv(here::here("data_processed/usc_pubs.csv"))
usc_sdgs <- read.csv(here::here("data_processed/usc_sdgs.csv"))
usc_authors <- read.csv(here::here("data_processed/authors_only_revalued.csv"))
usc_bridge <- read.csv(here::here("data_processed/bridge.csv"))

# 2020-2022
usc_pubs <- usc_pubs %>% filter(Year %in% c(2020, 2021, 2022))

# merge
usc_pubs_sdgs <- merge(usc_pubs, usc_sdgs, 
                       by.x = c("X", "Link"), by.y = c("document", "Link"), 
                       all.x = TRUE)
tmp <- merge(usc_pubs, usc_bridge,
             by.x = c("X", "Link"), by.y = c("pubID", "link"))
tmp2 <- merge(tmp, usc_authors,
              by.x = "authorID", by.y = "authorID")
usc_joined <- merge(tmp2, usc_sdgs,
                    by.x = c("X", "Link"), by.y = c("document", "Link"),
                    all.x = TRUE)

authorChoices = setNames(usc_authors$authorID, 
                         paste(usc_authors$LName, usc_authors$FName, sep = ", "))

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
          h4(disclaimer),
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
        tabName = "3",
        fluidPage(
          h1("USC Research: SDGs By Year"),
          #h3("this is a description"),
          h4(disclaimer),
          div(
            style="font-size:24px;",
            selectInput(
              inputId = "Year",
              label = "Choose Year", 
              choices = sort(unique(usc_pubs_sdgs$Year))
            )
          ), 
          h3("Yearly Total Count of Publications By SDG"), 
          fluidRow(
            column(6, plotOutput("year_sdg_barplot"))
          ),
          #h3("SDG Related Research vs. Non-related Research"),
          #fluidRow(column(12, plotOutput("pie1")))
          fluidRow(
            column(6, 
                   h3("SDG Related Research vs. Non-related Research"),
                   plotOutput("pie1")
            )
          ) # end fluidRow
        ) # end fluidPage
      ), # end tabItem 3
      tabItem(
        tabName = "4",
        fluidPage(
          h1("USC Research: SDGs by Department"),
          h3("Select a USC School below to view the number of SDG-related
             publications by departments."),
          h4(disclaimer),
          div(
            style="font-size:24px;", 
            selectInput(
              inputId = "usc_division",
              label = "Choose USC School",
              # selected = "Dornsife College of Letters, Arts and Sciences",
              choices = sort(unique(usc_authors$Division))
            )
          ),
          h3("SDG Publications by Departments"),
          fluidRow(column(12, plotlyOutput(outputId = "pubs_to_bar"))),
          h3("SDG-Related Research"),
          fluidRow(column(12, plotOutput("pubs_to_pie")))
        ) # end fluidPage
      ), # end tabItem 4
      tabItem(
        tabName = "5",
        fluidPage(
          h1("View Top Scholars and Departments by SDGs"),
          #h3("description"),
          h4(disclaimer),
          div(
            style="font-size:24px;", 
            selectInput(
              inputId = "Primary.SDG", 
              label = "Choose SDG", 
              choices = sdg_choices,
            )
          ),
          div(
            style="font-size:24px;", 
            selectInput(
              inputId = "Division", 
              label = "Select USC School", 
              choices = "",
              selected = ""
            )
          ),
          br(),
          #h1(textOutput(paste0("Top Researchers in", input$Division))),
          fluidRow(
            bootstrapPage(
              column(12, 
                     plotOutput(outputId = "top_authors_sdg_table"), 
                     br()
              )
            )
          ),
          #h1(textOutput(paste0("Top Departments in ", input$Division))),
          fluidRow(
            bootstrapPage(
              column(8, 
                     plotOutput(outputId = "top_departments_sdg_table")
              ), 
              br(),
              column(4, img(src = "un_17sdgs.jpg", width = "100%"))
            )
          )
        ) # end fluidPage
      ), # end tabItem 5
      tabItem(
        tabName = "6",
        fluidPage(
          h1("Find SDGs and Publications by USC Author"),
          h4(disclaimer),
          div(
            style="font-size:24px;", 
            selectInput(
              inputId = "school",
              label = "Choose USC School",
              choices = sort(unique(usc_authors$Division)),
              selected = ""
            )
          ),
          div(
            style="font-size:24px;", 
            selectInput(
              inputId = "author",
              label = "Choose USC Author",
              choices = authorChoices[sort(names(authorChoices))],
              selected = NULL
            )
          ),
          fluidRow(column(12, DT::dataTableOutput("auth_about"))),
          # graph
          h3("Graph of Author's Publications by SDG"),
          fluidRow(column(6, plotOutput("author_sdg_barplot"))),
          # table
          h3("List of Author's Publications"),
          fluidRow(
            bootstrapPage(
              column(12, DT::dataTableOutput("author_pub_table"))
            )
          )
        ) # end fluidPage
      ), # end tabItem 6
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

get_selected_sdg_col <- function(sdg) {
  if (as.numeric(sdg) < 10) {
    return (sym(paste0("SDG.0", sdg)))
  }
  sym(paste0("SDG.", sdg))
}

server <- function(input, output, session) {
  # tab 2
  output$sdg_total_by_year  <- renderPlot(
    {
      # set correct column name
      sdg_col = get_selected_sdg_col(input$sdg_goal)
      # bar chart
      usc_pubs_sdgs %>%
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
    })
  
  output$plot3 <- renderImage(
    {
      # When input$n is 1, filename is ./images/image1.jpeg
      filename <- normalizePath(file.path("./www",
                                          paste("sdg", input$sdg_goal, ".png", sep="")))
      # Return a list containing the filename
      list(src = filename, height = "100%")
    }, deleteFile = FALSE)
  
  # tab 3
  output$year_sdg_barplot <- renderPlot(
    {
      usc_pubs_sdgs %>%
        filter(Year == input$Year) %>%
        summarise(across(starts_with("SDG"), sum, na.rm = TRUE)) %>%
        t %>%
        as.data.frame() %>%
        ggplot(aes(x = as.factor(1:17), y = V1, fill = factor(1:17))) +
        geom_col() +
        scale_color_manual(values = sdg_colors,
                           aesthetics = c("fill")) +
        geom_text(aes(label = V1), vjust = -0.2) +
        labs(title = paste0(" (", input$Year, ") ", "Count of Publications Per SDG"),
             fill = "SDG",
             x = "SDG",
             y = "Count of Publications") +
        #guides(alpha = FALSE) +
        theme_minimal() +
        theme(text = element_text(size = 18))
    })
  
  output$pie1 <- renderPlot(
    {
      # data
      sdg_sum <- usc_pubs_sdgs %>%
        filter(Year == input$Year) %>%
        select(starts_with("SDG")) %>%
        mutate(Total = rowSums(across(), na.rm = TRUE)) %>%
        select(Total)
      pie_data <- data.frame(group = c("Not Related", "Related"),
                             value = c(sum(sdg_sum == 0),
                                       sum(sdg_sum != 0)))
      # compute positions of labels
      pie_data <- pie_data %>% 
        arrange(desc(group)) %>%
        mutate(prop = value / sum(pie_data$value) * 100) %>%
        mutate(ypos = cumsum(prop) - 0.5 * prop )
      
      # plot
      ggplot(pie_data, aes(x = "", y = prop, fill = group)) +
        geom_bar(stat = "identity", width = 1, color = "black") +
        coord_polar("y", start = 0) +
        geom_text(aes(y = ypos, label = value), color = "black", size = 16) +
        scale_fill_manual(values = c("#990000", "#FFC72C"), name = "Legend") +
        theme_void() +
        theme(text = element_text(size = 18))
  })
  
  # tab 4
  output$pubs_to_bar <- renderPlotly(
    {
      validate(
        need(input$usc_division != "", label = "USC School")
      )
      
      d <- usc_joined %>%
        filter(Division == input$usc_division) %>%
        select(Department, starts_with("SDG")) %>%
        group_by(Department) %>%
        summarise(across(starts_with("SDG"), sum, na.rm = TRUE))
      df <- d %>% column_to_rownames("Department")
      colnames(df) <- 1:17
      df$category <- row.names(df)
      m <- melt(df, id.vars = "category")
      p <- ggplot(m, aes(category, value, 
                         fill = variable, 
                         text = paste(category, "<br>has", value, "SDG", 
                                      variable))) +
        geom_bar(position = "stack", stat = "identity") +
        #coord_flip() +
        scale_x_discrete(labels = NULL) +
        scale_color_manual(values = sdg_colors,
                           aesthetics = c("fill")) +
        labs(
          fill = "SDG",
          x = "Departments",
          y = "Count of Publications"
        ) +
        theme_minimal() +
        theme(text = element_text(size = 18))
      ggplotly(p, tooltip = "text") %>% layout(hoverlabel = list(font=list(size=18)))
    }
  )
  output$pubs_to_pie <- renderPlot(
    {
      validate(
        need(input$usc_division != "", label = "USC School")
      )
      
      # data
      sdg_sum <- usc_joined %>% 
        filter(Division == input$usc_division) %>%
        summarise(across(starts_with("SDG"), sum, na.rm = TRUE))
      pie_data <- data.frame(group = as.factor(1:17),
                             value = t(sdg_sum))
      
      # compute positions of labels
      pie_pos <- pie_data %>% 
        mutate(csum = rev(cumsum(rev(value))), 
               pos = value/2 + lead(csum, 1),
               pos = if_else(is.na(pos), value/2, pos))
      
      # plot
      ggplot(pie_data, aes(x = "", y = value, fill = group)) +
        geom_bar(stat = "identity", width = 1, color = "black") +
        coord_polar(theta = "y") +
        # geom_label_repel(data = pie_pos,
        #                  aes(y = pos, label = value),
        #                  size = 4.5, nudge_x = 1,
        #                  max.overlaps = 16, show.legend = FALSE) +
        scale_fill_manual(values = sdg_colors,
                          aesthetics = "fill") +
        labs(fill = "SDG") +
        theme_void() +
        theme(text = element_text(size = 18))
    })

  # tab 5
  # select division then sdg
  # observeEvent(
  #   input$Division,
  #   {
  #     validate(
  #       need(input$Division != "", label = "USC School")
  #     )
  #     updateSelectizeInput(session,
  #                          "Primary.SDG",
  #                          server = TRUE,
  #                          choices = usc_joined %>%
  #                            filter(Division == input$Division) %>% 
  #                            summarise(across(starts_with("SDG"), sum, na.rm = TRUE)) %>% 
  #                            select_if(colSums(.) != 0) %>% 
  #                            colnames %>% 
  #                            substr(start = 5, stop = 6) %>% 
  #                            as.numeric,
  #                          selected = "")
  #   }
  # )
  # select sdg then division
  observeEvent(
    input$Primary.SDG,
    {
      validate(
        need(input$Primary.SDG != "", label = "SDG")
      )
      sdg_col = get_selected_sdg_col(input$Primary.SDG)
      divisions = usc_joined %>%
        filter(!!sdg_col == 1) %>%
        select(Division) %>%
        as.list()
      updateSelectizeInput(session,
                           "Division",
                           server = TRUE,
                           choices = sort(divisions[[1]]),
                           selected = ""
                           )
    }
  )

  output$top_authors_sdg_table <- renderPlot({
    validate(
      need(input$Primary.SDG != "", label = "SDG"),
      need(input$Division != "", label = "USC School")
    )
    sdg_col = get_selected_sdg_col(input$Primary.SDG)
    usc_joined %>%
      filter(Division == input$Division) %>%
      filter(!!sdg_col != 0) %>%
      count(authorID, Name, Division) %>%
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
  })

  output$top_departments_sdg_table <- renderPlot({
    validate(
      need(input$Primary.SDG != "", label = "SDG"),
      need(input$Division != "", label = "USC School")
    )
    sdg_col = get_selected_sdg_col(input$Primary.SDG)
    usc_joined %>%
      filter(Division == input$Division) %>%
      filter(!!sdg_col != 0) %>%
      count(Department) %>%
      arrange(desc(n)) %>%
      distinct(Department, .keep_all = TRUE) %>%
      head(10) %>%
      ggplot(aes(x = reorder(as.factor(Department),n), y = n)) +
      geom_col(fill = sdg_colors[as.numeric(input$Primary.SDG)], alpha = 1) +
      coord_flip() +
      scale_x_discrete(labels = label_wrap(15)) +
      labs(title = paste0("Top Departments that Map to SDG #", input$Primary.SDG),
           x = "Departments",
           y = "Number of Publications ") +
      theme(text = element_text(size = 20))
  })

  # tab 6
  observeEvent(
    input$school, ignoreNULL = FALSE,
    {
      if (input$school == "") {
        selected_authors = usc_authors
      } else {
        selected_authors = usc_authors %>% filter(usc_authors$Division == input$school)
      }
      authorChoices = setNames(selected_authors$authorID,
                               paste(selected_authors$LName, selected_authors$FName, sep = ", "))
      updateSelectizeInput(session,
                           "author",
                           server = TRUE,
                           choices = authorChoices[sort(names(authorChoices))],
                           selected = NULL
      )
    }
  )
  output$auth_about <- DT::renderDataTable(
    {
      validate(
        need(input$author != "", label = "USC Author")
      )
      usc_authors %>%
        filter(usc_authors$InUSCDirectory & usc_authors$authorID == input$author) %>%
        select(FName, LName, Department, Division, Email, PositionTitle)
    }, options =
      list(searching = FALSE, paging = FALSE,
           language = list(
             zeroRecords = "Not a current USC faculty/staff"
            )
          )
    )

  output$author_sdg_barplot <- renderPlot(
    {
      validate(
        need(input$author != "", label = "USC Author")
      )
      usc_joined %>% 
        filter(usc_joined$authorID == input$author) %>%
        summarise(across(starts_with("SDG"), sum, na.rm = TRUE)) %>% 
        t %>% 
        as.data.frame() %>% 
        ggplot(aes(x = as.factor(1:17), y = V1, fill = factor(1:17))) + 
        geom_col() + 
        scale_color_manual(values = sdg_colors, aesthetics = "fill") +
        scale_y_continuous(breaks = scales::pretty_breaks()) +
        labs(title = names(input$author),
             x = "SDG",
             y = "Number of Publications",
             fill = "SDG") +
        theme_minimal() +
        theme(text = element_text(size = 18))
    }
  )

  output$author_pub_table <- DT::renderDataTable(
    {
      print(input$author)
      validate(
        need(input$author != "", label = "USC Author")
      )
      pubs <- usc_joined %>%
        filter(usc_joined$authorID == input$author) %>%
        select(Titles, Link)
      sdgs_only <- usc_joined %>%
        filter(usc_joined$authorID == input$author) %>%
        select(starts_with("SDG")) %>%
        replace(is.na(.), 0)
      w <- which(sdgs_only != 0, arr.ind = TRUE)
      if (length(w) == 0) {
        pubs$SDGs <- ""
        pubs[, c("SDGs", "Titles", "Link")] 
        return (pubs)
      }
      sdgs_only[w] <- as.numeric(substr(names(sdgs_only)[w[, "col"]], start = 5, stop = 6))
      sdgs_collapsed <- apply(sdgs_only, 1, function(x) {
        res = ""
        for (i in 1:length(x)) {
          if (x[i] != 0) {
            if (res == "") {
              res = x[i]
            } else {
              res = paste(res, x[i], sep = ", ")
            }
          }
        }
        res
      })
      pubs$SDGs <- sdgs_collapsed
      pubs[, c("SDGs", "Titles", "Link")]
  }, rownames = FALSE)
}

# Run the application 
shinyApp(ui = ui, server = server)