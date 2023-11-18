# 11.17.23
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/

# Load the required packages --------------------------------------------------
# install.packages("name") to install any missing packages
# list_of_packages <- c("shiny", "shinydashboard", "tidyverse", "plotly",
#                       "wordcloud", "DT", "ggplot2", "ggrepel", "here", 
#                       "reshape2", "scales", "ggbreak", "treemapify")
# invisible(lapply(list_of_packages, library, character.only = TRUE))
library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(wordcloud)
library(DT)
library(ggplot2)
library(ggrepel)
library(here)
library(reshape2)
library(scales)
library(ggbreak)
library(treemapify)
library(stringr)

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
sdg_names <- paste(1:17, sdg_names, sep = " - ")
sdg_choices <- 1:17
names(sdg_choices) <- sdg_names
sdg_col_names <- syms(c("SDG.01", "SDG.02", "SDG.03", "SDG.04", "SDG.05", "SDG.06", 
                   "SDG.07", "SDG.08", "SDG.09", "SDG.10", "SDG.11", "SDG.12", 
                   "SDG.13", "SDG.14", "SDG.15", "SDG.16", "SDG.17"))

# data
usc_pubs <- read.csv("data_processed/usc_pubs_law.csv")
usc_sdgs <- read.csv("data_processed/usc_sdgs_with_categories.csv")
# usc_authors <- read.csv("data_processed/authors_only_revalued.csv")
usc_authors <- read.csv("data_processed/usc_authors_law_fixed_dept.csv") %>%
  rename(Division = Div, Department = Dept)
usc_bridge <- read.csv("data_processed/bridge_law_fixed2.csv")
# dei_data <- read.csv("data_processed/DEI_pubs.csv")
dei_joined <- read.csv("data_processed/DEI_pubs_ordered.csv")

# 2020-2022
usc_pubs <- usc_pubs %>% filter(Year %in% c(2020, 2021, 2022))

# url
usc_pubs$url <- paste0("<a href='", usc_pubs$Link, "' target='_blank'>", usc_pubs$Link, "</a>")

# merge
usc_pubs_sdgs <- merge(usc_pubs, usc_sdgs, 
                       by = c("pubID", "Link"),
                       all.x = TRUE)
usc_pubs_sdgs$sustainability_category[is.na(usc_pubs_sdgs$sustainability_category)] = "Not-Related"

tmp <- merge(usc_pubs, usc_bridge,
             by = c("pubID", "Link"))
tmp2 <- merge(tmp, usc_authors,
              by = "authorID")
usc_joined <- merge(tmp2, usc_sdgs,
                    by = c("pubID", "Link"),
                    all.x = TRUE)
usc_joined$sustainability_category[is.na(usc_joined$sustainability_category)] = "Not-Related"


# tmp <- merge(dei_data, usc_bridge,
#              by.x = c("pubID", "Link"), by.y = c("pubID", "link"))
# dei_joined <- merge(tmp, usc_authors,
#               by.x = "authorID", by.y = "authorID")


# create chart data outside app.R
usc_by_product_sust_cat <- read.csv("usc_by_product_sust_cat.csv")
# usc_by_author_sust_cat <- read.csv("usc_by_author_sust_cat.csv")
usc_by_dept_sust_cat <- read.csv("usc_by_dept_sust_cat.csv")

authorChoices = setNames(usc_authors$authorID, usc_authors$fullname)

ui <- dashboardPage(
  # theme
  skin = "black",
  
  # Application title
  dashboardHeader(title = "USC Sustainability Research Finder", titleWidth = 400),
  
  dashboardSidebar(width = 400,
    sidebarMenu(
      menuItem("About", tabName = "1"),
      menuItem("FAQ", tabName = "7"),
      # menuItem("Learn About The SDGs", tabName = "2"),
      menuItem("USC Research: SDGs by Year", tabName = "3"),
      menuItem("USC Research: SDGs by School", tabName = "4"),
      menuItem("View USC Scholars and Schools by SDGs", tabName = "5"),
      menuItem("Find SDGs and Research by USC Scholar", tabName = "6"),
      menuItem("Sustainability Research in Los Angeles", tabName = "8")
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$link( # link css stylesheet
        rel="stylesheet", 
        type="text/css", 
        href="custom.css"
      ),
      tags$link( # link icon library
        rel="stylesheet",
        href="https://cdnjs.cloudflare.com/ajax/libs/font-awesome/4.7.0/css/font-awesome.min.css"
      )
    ),
    tabItems(
      tabItem(
        tabName = "1",
        fluidPage(
          h1("About"),
          fluidRow(
            column(6,
              h3(
                strong(
                  "Are you interested in sustainability and the ",
                  a(
                    "UN Sustainable Development Goals (SDGs)",
                    href="https://sdgs.un.org", 
                    .noWS = "after",
                    target = "_blank"
                  ),
                  "?"),
                "If so, you have come to the right place!",
                br(), 
                br(),
                strong(
                  "This dashboard is a tool that enables you to see which research
                publications at USC relate to the 17 UN SDGs. You can also use this 
                tool to find USC scholars and publications that 
                match your academic interest!"
                ),
                br(),
                br(),
                "Sustainability incorporates protection for the environment,
              balancing a growing economy, and social responsibility to lead to 
              an improved quality of life for current and future generations."
              )), # end h3
            column(6, img( # move next to next
              src="Asgmt_Earth_Research.png", 
              height="450", 
              style="display: block; margin-left: auto; margin-right: auto;"
            ))
          ),
          
          h1("Learn About The SDGs"),
          fluidRow(
            column(6, h3(
              "SDG stands for", 
              a(
                "UN sustainable development goals", 
                href="https://sdgs.un.org",
                .noWS = "after",
                target = "_blank"
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
            )),
            column(6, img(src = "un_17sdgs.jpg", width = "100%"))
            
          ),
          
          uiOutput("disclaimer"),
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
              column(6, plotOutput(outputId ="plot3"), br()),
              column(6, plotOutput(outputId = "sdg_total_by_year"), br())
              # column(4, ) # move image next to paragraph text
            )
          )
        ) # end fluidPage
      ), # end tabItem 1
      # tabItem(
      #   tabName = "2",
      #   fluidPage(
      #     
      #     
      #     # h1(textOutput("sdg_name")),
      #     #fluidRow(bootstrapPage(
      #     # column(12, DT::dataTableOutput("top_classes_sdg_table"))
      #     #))
      #   ) # end fluidPage
      # ), # end tabItem 2
      tabItem(
        tabName = "3",
        fluidPage(
          h1("USC Research: SDGs By Year"),
          #h3("this is a description"),
          # uiOutput("disclaimer"),
          h4("Data is from 2020-2022. This app is a work in progress, and,
         we are continually improving accuracy. If you have feedback,
         please fill out our ",
             a("feedback form",
               href="https://forms.gle/P6QJDSJaaRusZLZh6", .noWS = "after",
               target = "_blank"),
             "."),
          div(
            style="font-size:24px;",
            selectInput(
              inputId = "Year",
              label = "Choose Year", 
              choices = sort(unique(usc_pubs_sdgs$Year))
            )
          ), 
          h4("Please wait for data to load (~30 sec)"), 
          fluidRow(
            column(6, h3("Count of Research Products* per SDG"), 
                   h4("Products include publications, books, conference proceedings, and scholarly reports"),
                   plotlyOutput("year_sdg_barplot")),
            column(6, img(src = "un_17sdgs.jpg", width = "100%"))
          ),
          #h3("SDG Related Research vs. Non-related Research"),
          #fluidRow(column(12, plotOutput("pie1")))
          # fluidRow(
          #   column(6, 
          #          # h3("SDG Related Research vs. Non-related Research"),
          #          plotOutput("pie1")
          #   ),
          #   
          # ),
          # h3("For 2020-2022"),
          # h4("*Employees include students, postdocs, staff and faculty that are on publications"),
          fluidRow(
            column(6, plotOutput("pie2")),
            # column(6, plotOutput("stacked_bar2"))
            column(6, plotOutput("stacked_bar_product"))
          ),
          fluidRow(
            column(6, plotOutput("pie3")),
            column(6, plotOutput("stacked_bar3"))
          )# end fluidRow
        ) # end fluidPage
      ), # end tabItem 3
      tabItem(
        tabName = "4",
        fluidPage(
          h1("USC Research: SDGs by School"),
          h3("Select a USC School/Unit below to view the number of SDG-related
             publications by departments."),
          # uiOutput("disclaimer"),
          h4("Data is from 2020-2022. This app is a work in progress, and,
         we are continually improving accuracy. If you have feedback,
         please fill out our ",
             a("feedback form",
               href="https://forms.gle/P6QJDSJaaRusZLZh6", .noWS = "after",
               target = "_blank"),
             "."),
          div(
            style="font-size:24px;", 
            selectInput(
              inputId = "usc_division",
              label = "Choose USC School/Unit",
              choices = sort(unique(usc_authors$Division)),
              selected = "Dornsife College of Letters Arts and Sciences"
            )
          ),
          h3("Research Products* and SDGs by Departments/Centers/Institutes"),
          h4("Products include publications, books, conference proceedings, and scholarly reports"),
          h4("Hover over the columns to see Department/Centers/Institutes name. 
            Drag cursor over a section to zoom in and double click to zoom out. 
            You can also use the tools in the top right corner."),
          fluidRow(column(12, plotlyOutput(outputId = "pubs_to_bar"))),
          h3("SDG-Related Research Across All Departments/Centers/Institutes"),
          fluidRow(column(12, plotOutput("pubs_to_treemap")))
        ) # end fluidPage
      ), # end tabItem 4
      tabItem(
        tabName = "5",
        fluidPage(
          h1("View USC Scholars and Schools by SDGs"),
          #h3("description"),
          # uiOutput("disclaimer"),
          h4("Data is from 2020-2022. This app is a work in progress, and,
         we are continually improving accuracy. If you have feedback,
         please fill out our ",
             a("feedback form",
               href="https://forms.gle/P6QJDSJaaRusZLZh6", .noWS = "after",
               target = "_blank"),
             "."),
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
              label = "Select USC School/Unit", 
              choices = "",
              selected = ""
            )
          ),
          # h4("Please wait for data to load (~30 sec)"),
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
          h1("Find SDGs and Research by USC Scholar"),
          # uiOutput("disclaimer"),
          h4("Data is from 2020-2022. This app is a work in progress, and,
         we are continually improving accuracy. If you have feedback,
         please fill out our ",
             a("feedback form",
               href="https://forms.gle/P6QJDSJaaRusZLZh6", .noWS = "after",
               target = "_blank"),
             "."),
          # div(
          #   style="font-size:24px;", 
          #   selectInput(
          #     inputId = "school",
          #     label = "Choose USC School/Unit",
          #     choices = sort(unique(usc_authors$Division)),
          #     selected = ""
          #   )
          # ),
          div(
            style="font-size:24px;", 
            selectizeInput(
              inputId = "author",
              label = (HTML("<p style='font-size:24px;font-weight:700;margin:0;'>Choose USC Scholar</p>
                            <p style='font-size:20px;font-weight:400;margin:0;'>Start by typing scholar's last name</p>")),
              # choices = authorChoices[sort(names(authorChoices))], 
              choices = NULL,
              selected = NULL
            )
          ),
          fluidRow(column(12, DT::dataTableOutput("auth_about"))),
          # graph
          h3("Scholar's Research Products by SDG"),
          fluidRow(column(6, plotOutput("author_sdg_barplot")),
                   column(6, img(src = "un_17sdgs.jpg", width = "100%"))),
          # table
          h3("List of Scholar's Research Products"),
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
              target = "_blank"
            ),
            "at USC by Dr. Julie Hopper in the Office of Sustainability and 
            five USC students: Alison Chen, Aurora Massari, Bhavya Ramani, Ric 
            Xian and Xinyi Zhang. ", 
            strong("USC research products in the dashboard dataset includes 
                   books, publications, conference proceedings, and scholarly 
                   reports pulled from ",
            a(
              "Scopus",
              href = "https://www.scopus.com/home.uri",
              target = "_blank"
            ),
            "(Elsevier's citation database) and augmented with data provided 
            by USC Librarians for USC schools that are not fully represented 
            in Scopus."),
            "All of the datasets, R-packages (",
            a(
              "text2sdg",
              href = "https://CRAN.R-project.org/package=text2sdg",
              .noWS = "outside",
              target = "_blank"
            ),
            ") and code used in this dashboard are in ",
            a(
              "our Github page",
              href = "https://github.com/USC-Office-of-Sustainability/SDGMappingResearch",
              .noWS = "after",
              target = "_blank"
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
              .noWS = "after",
              target = "_blank"
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
                   .noWS = "after",
                   target = "_blank"
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
            "A: Please fill out our ",
            a("feedback form",
              href="https://forms.gle/P6QJDSJaaRusZLZh6", .noWS = "after",
              target = "_blank"),
            "."
          )
        ) # end fluidPage
      ), # end tabItem 7
      tabItem(
        tabName = "8",
        fluidPage(
          h1("Sustainability-Research in Los Angeles"),
          # uiOutput("disclaimer"),
          h4("Data is from 2020-2022. This app is a work in progress, and,
         we are continually improving accuracy. If you have feedback,
         please fill out our ",
             a("feedback form",
               href="https://forms.gle/P6QJDSJaaRusZLZh6", .noWS = "after",
               target = "_blank"),
             "."),
          downloadButton("download_dei_data", "Download"),
          fluidRow(column(12, DT::dataTableOutput("dei_table"))),
        )
      ) # end tabItem 8
    ), # end tabItems
    tags$footer(
      fluidPage(
        h4("Stay connected by visiting our", 
          a("home page", href="https://sustainability.usc.edu",
            target = "_blank"), 
          "or by following the Office of Sustainability on social media via", 
          a("", href="https://www.instagram.com/green.usc/", class="fa fa-instagram",
            target = "_blank"),
          a("Instagram", href="https://www.instagram.com/green.usc/",
            target = "_blank"), "or", 
          a("", href="https://twitter.com/GreenUSC", class="fa fa-twitter",
            target = "_blank"),
          a("Twitter", href="https://twitter.com/GreenUSC", .noWS = "after",
            target = "_blank"), 
          ". You can also support the Office of Sustainability by donating", 
          a("here", 
            href="https://green.usc.edu/get-involved/give-to-the-office-of-sustainability/",
            .noWS = "after",
            target = "_blank"), 
          ". More questions or suggestions in regard to this tool? Please fill out our",
          a("feedback form",
            href="https://forms.gle/P6QJDSJaaRusZLZh6", .noWS = "after",
            target = "_blank"),
          "."
          ),
      )
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
  output$disclaimer <- renderUI({
    tagList(
      h4("Data is from 2020-2022. This app is a work in progress, and,
         we are continually improving accuracy. If you have feedback,
         please fill out our ",
         a("feedback form",
           href="https://forms.gle/P6QJDSJaaRusZLZh6", .noWS = "after",
           target = "_blank"),
         ".")
    )
  })
  # tab 2
  output$sdg_total_by_year  <- renderPlot(
    {
      # set correct column name
      sdg_col = get_selected_sdg_col(input$sdg_goal)
      # bar chart
      usc_pubs_sdgs %>%
        filter(!!sdg_col > 0) %>%
        count(Year) %>%
        ggplot(aes(x = Year,y = n)) +
        geom_col(fill = sdg_colors[as.numeric(input$sdg_goal)], alpha = 1) +
        scale_color_manual(values = sdg_colors,
                           aesthetics = c("fill")) +
        #geom_text(aes(label = Freq), vjust = -0.2, size = 4) +
        labs(title = str_wrap("Count of Research Products* by Year", 40), 
             subtitle = str_wrap("Products include publications, books, conference proceedings, and scholarly reports", 40),
             fill = "SDG",
             x = "Year",
             y = "Count") +
        #guides(alpha = FALSE) +
        theme_minimal(base_size = 20)
    })
  
  output$plot3 <- renderImage(
    {
      # When input$n is 1, filename is ./images/image1.jpeg
      filename <- normalizePath(file.path("./www",
                                          paste("sdg", input$sdg_goal, ".png", sep="")))
      # Return a list containing the filename
      list(src = filename, height = "100%")
    }, deleteFile = FALSE)
  
  # tab 3 # stacked bar instead of pie charts
  output$year_sdg_barplot <- renderPlotly(
    {
      # y_max = usc_pubs_sdgs %>%
      #   filter(Year == input$Year) %>%
      #   summarise(across(starts_with("SDG"), sum, na.rm = TRUE)) %>%
      #   t %>%
      #   as.data.frame() %>%
      #   max
      # y_max_floor = y_max %/% 1000 * 1000
      p <- usc_pubs_sdgs %>%
        filter(Year == input$Year) %>%
        select(starts_with("SDG")) %>%
        mutate(across(everything(), ~replace(., . != 0, 1))) %>%
        summarise(across(starts_with("SDG"), sum, na.rm = TRUE)) %>%
        t %>%
        as.data.frame() %>%
        ggplot(aes(x = as.factor(1:17), y = V1, fill = factor(1:17), 
               text = paste0("n = ", V1))) +
        geom_col() +
        scale_color_manual(values = sdg_colors,
                           aesthetics = c("fill")) +
        # scale_y_break(c(200, y_max_floor)) +
        # ylim(0, y_max+50) +
        # geom_text(aes(label = V1), vjust = -0.2, size = 16/.pt) +
        labs(#title = str_wrap(paste0("Count of Publications per SDG in ", input$Year), 25), # Research Product* Count per SDG in Year # subtitle: Products include publications, books, conference proceedings, and scholarly reports.
             fill = "SDG",
             x = "SDG",
             y = "Count") +
        #guides(alpha = FALSE) +
        theme_minimal(base_size = 18) +
        theme(legend.position = "none")
      ggplotly(p, tooltip = "text")
    })
  
  output$pie1 <- renderPlot(
    {
      # 3 categories:
      # sust focused = at least 1 %in% 13:15 and at least 1 %in% 1:12, 16, 17
      # sust inclusive = at least 1 sdg
      # not related
      
      # data
      # sdg_sum <- usc_pubs_sdgs %>%
      #   filter(Year == input$Year) %>%
      #   select(starts_with("SDG")) %>%
      #   mutate(Total = rowSums(across(), na.rm = TRUE)) %>%
      #   select(Total)
      # num_not_related <- sum(sdg_sum == 0)
      # num_related <- sum(sdg_sum != 0)
      # num_focused <- usc_pubs_sdgs %>% 
      #   filter(Year == input$Year) %>% 
      #   select(starts_with("SDG")) %>% 
      #   filter(SDG.13 > 0 | SDG.14 > 0 | SDG.15 > 0) %>% 
      #   filter(SDG.01 > 0 | SDG.02 > 0 | SDG.03 > 0 | SDG.04 > 0 | 
      #            SDG.05 > 0 | SDG.06 > 0 | SDG.07 > 0 | SDG.08 > 0 | 
      #            SDG.09 > 0 | SDG.10 > 0 | SDG.11 > 0 | SDG.12 > 0 | 
      #            SDG.16 > 0 | SDG.17 > 0) %>% 
      #   nrow()
      # num_inclusive <- num_related - num_focused
      num_not_related <- usc_pubs_sdgs %>%
        filter(sustainability_category == "Not-Related") %>%
        nrow
      num_inclusive <- usc_pubs_sdgs %>%
        filter(sustainability_category == "Sustainability-Inclusive") %>%
        nrow
      num_focused <- usc_pubs_sdgs %>%
        filter(sustainability_category == "Sustainability-Focused") %>%
        nrow
      pie_data <- data.frame(group = c("Not Related", "Inclusive", "Focused"),
                             value = c(num_not_related,
                                       num_inclusive,
                                       num_focused))
      # compute positions of labels
      pie_data <- pie_data %>% 
        arrange(desc(group)) %>%
        mutate(prop = value / sum(pie_data$value) * 100) %>%
        mutate(ypos = cumsum(prop) - 0.5 * prop )
      
      # plot
      ggplot(pie_data, aes(x = "", y = prop, fill = group)) +
        geom_bar(stat = "identity", width = 1, color = "black") +
        coord_polar("y", start = 0) +
        geom_text(aes(y = ypos, label = value), color = "black", size = 20/.pt) +
        scale_fill_manual(values = c("#990000", "#FFC72C", "#767676"), name = "") +
        labs(title = paste0("Sustainability-Related Research in ", input$Year)) +
        theme_void(base_size = 18)
  })
  
  output$pie2 <- renderPlot( # add lines
    {
      # 3 categories:
      # sust focused = at least 1 %in% 13:15 and at least 1 %in% 1:12, 16, 17
      # sust inclusive = at least 1 sdg
      # not related
      
      # data
      
      # sdg_sum <- usc_by_author_sdg_sum$Total
      # num_not_related <- sum(sdg_sum == 0)
      # num_related <- sum(sdg_sum != 0)
      # num_focused <- usc_by_author_sdg_sum %>%
      #   filter(SDG.13 > 0 | SDG.14 > 0 | SDG.15 > 0) %>% 
      #   filter(SDG.01 > 0 | SDG.02 > 0 | SDG.03 > 0 | SDG.04 > 0 | 
      #            SDG.05 > 0 | SDG.06 > 0 | SDG.07 > 0 | SDG.08 > 0 | 
      #            SDG.09 > 0 | SDG.10 > 0 | SDG.11 > 0 | SDG.12 > 0 | 
      #            SDG.16 > 0 | SDG.17 > 0) %>% 
      #   nrow()
      # num_inclusive <- num_related - num_focused
      usc_by_author_sust_cat <- usc_joined %>%
        group_by(authorID) %>%
        summarize(all_sustainability_categories = paste(sustainability_category[!duplicated(sustainability_category)], collapse = ";")) %>%
        mutate(one_sustainability_category = case_when(grepl("Focused", all_sustainability_categories)~"Sustainability-Focused",
                                                       grepl("Inclusive", all_sustainability_categories)~"Sustainability-Inclusive",
                                                       grepl("Not-Related", all_sustainability_categories)~"Not-Related")) %>%
        select(authorID, one_sustainability_category)
      num_not_related <- usc_by_author_sust_cat %>%
        filter(one_sustainability_category == "Not-Related") %>%
        nrow
      num_inclusive <- usc_by_author_sust_cat %>%
        filter(one_sustainability_category == "Sustainability-Inclusive") %>%
        nrow
      num_focused <- usc_by_author_sust_cat %>%
        filter(one_sustainability_category == "Sustainability-Focused") %>%
        nrow
      pie_data <- data.frame(group = c("Not Related", "Inclusive", "Focused"),
                             value = c(num_not_related,
                                       num_inclusive,
                                       num_focused))
      # compute positions of labels
      pie_data <- pie_data %>% 
        arrange(desc(group)) %>%
        mutate(prop = value / sum(pie_data$value) * 100) %>%
        mutate(ypos = cumsum(prop) - 0.5 * prop )
      
      # plot
      ggplot(pie_data, aes(x = "", y = prop, fill = group)) +
        geom_bar(stat = "identity", width = 1, color = "black") +
        coord_polar("y", start = 0) +
        geom_label_repel(aes(y = ypos, label = paste0(value)),
                         size = 4.5, nudge_x = 1, show.legend = FALSE) +
        # geom_text(aes(y = ypos, label = value), color = "black", size = 20/.pt) +
        scale_fill_manual(values = c("#990000", "#FFC72C", "#767676"), name = "") +
        labs(title = str_wrap("Employees conducting Sustainability-Related Research", 40)) +
        theme_void(base_size = 18)
      total_count <- num_not_related + num_inclusive + num_focused
      pie_data$percent <- round(pie_data$value/total_count*100,1)
      pie(pie_data$value, 
          labels = paste(pie_data$group,paste0("(", pie_data$value, ", ",pie_data$percent,"%)"), sep = "\n"), # put count, % in next line
          col = c("#767676", "#FFC72C", "#990000"), 
          main = str_wrap("Scholars Conducting Sustainability-Related Research 2020-22", 40), 
          cex = 1.5, cex.main = 1.5)
    })
  
  output$pie3 <- renderPlot(
    {
      # 3 categories:
      # sust focused = at least 1 %in% 13:15 and at least 1 %in% 1:12, 16, 17
      # sust inclusive = at least 1 sdg
      # not related
      
      # data
      
      # sdg_sum <- usc_by_dept_sdg_sum$Total
      # num_not_related <- sum(sdg_sum == 0)
      # num_related <- sum(sdg_sum != 0)
      # num_focused <- usc_by_dept_sdg_sum %>%
      #   filter(SDG.13 > 0 | SDG.14 > 0 | SDG.15 > 0) %>% 
      #   filter(SDG.01 > 0 | SDG.02 > 0 | SDG.03 > 0 | SDG.04 > 0 | 
      #            SDG.05 > 0 | SDG.06 > 0 | SDG.07 > 0 | SDG.08 > 0 | 
      #            SDG.09 > 0 | SDG.10 > 0 | SDG.11 > 0 | SDG.12 > 0 | 
      #            SDG.16 > 0 | SDG.17 > 0) %>% 
      #   nrow()
      # num_inclusive <- num_related - num_focused
      
      usc_by_dept_sust_cat <- usc_joined %>%
        group_by(Department) %>%
        summarize(all_sustainability_categories = paste(sustainability_category[!duplicated(sustainability_category)], collapse = ";")) %>%
        mutate(one_sustainability_category = case_when(grepl("Focused", all_sustainability_categories)~"Sustainability-Focused",
                                                       grepl("Inclusive", all_sustainability_categories)~"Sustainability-Inclusive",
                                                       grepl("Not-Related", all_sustainability_categories)~"Not-Related")) %>%
        select(Department, one_sustainability_category)
      num_not_related <- usc_by_dept_sust_cat %>%
        filter(one_sustainability_category == "Not-Related") %>%
        nrow
      num_inclusive <- usc_by_dept_sust_cat %>%
        filter(one_sustainability_category == "Sustainability-Inclusive") %>%
        nrow
      num_focused <- usc_by_dept_sust_cat %>%
        filter(one_sustainability_category == "Sustainability-Focused") %>%
        nrow
      pie_data <- data.frame(group = c("Not Related", "Inclusive", "Focused"),
                             value = c(num_not_related,
                                       num_inclusive,
                                       num_focused))
      # compute positions of labels
      pie_data <- pie_data %>% 
        arrange(desc(group)) %>%
        mutate(prop = value / sum(pie_data$value) * 100) %>%
        mutate(ypos = cumsum(prop) - 0.5 * prop )
      
      # plot
      ggplot(pie_data, aes(x = "", y = prop, fill = group)) +
        geom_bar(stat = "identity", width = 1, color = "black") +
        coord_polar("y", start = 0) +
        geom_label_repel(aes(y = ypos, label = paste0(value)),
                         size = 4.5, nudge_x = 1, show.legend = FALSE) +
        # geom_text(aes(y = ypos, label = value), color = "black", size = 20/.pt) +
        scale_fill_manual(values = c("#990000", "#FFC72C", "#767676"), name = "") +
        labs(title = paste0("Sustainability Related Departments")) +
        theme_void(base_size = 18)
      total_count <- num_not_related + num_inclusive + num_focused
      pie_data$percent <- round(pie_data$value/total_count*100,1)
      pie(pie_data$value, 
          labels = paste(pie_data$group,paste0("(", pie_data$value, ", ",pie_data$percent,"%)"), sep = "\n"), # put count, % in next line
          col = c("#767676", "#FFC72C", "#990000"), 
          main = str_wrap("Department/Centers/Institutes Conducting Sustainability-Related Research 2020-22", 40), 
          cex = 1.5, cex.main = 1.5)
    })
  output$stacked_bar_product <- renderPlot(
    {
      
      usc_by_product_sust_cat$one_sustainability_category <- factor(usc_by_product_sust_cat$one_sustainability_category, levels = c("Sustainability-Focused", "Sustainability-Inclusive", "Not Related"))
      ggplot(usc_by_product_sust_cat, aes(fill = one_sustainability_category, y = n, x = Year)) +
        geom_bar(position="fill", stat="identity") +
        scale_fill_manual(values = c("#990000", "#FFC72C", "#767676"), name = "") +
        scale_y_continuous(labels = scales::percent) +
        labs(title = str_wrap("Sustainability Related Products by Year", 40),
             y = "Percent") +
        theme_minimal(base_size = 20)
    })
  
  # output$stacked_bar2 <- renderPlot(
  #   {
  #     
  #     usc_by_author_sust_cat$one_sustainability_category <- factor(usc_by_author_sust_cat$one_sustainability_category, levels = c("Sustainability-Focused", "Sustainability-Inclusive", "Not Related"))
  #     ggplot(usc_by_author_sust_cat, aes(fill = one_sustainability_category, y = n, x = Year)) +
  #       geom_bar(position="fill", stat="identity") +
  #       scale_fill_manual(values = c("#990000", "#FFC72C", "#767676"), name = "") +
  #       scale_y_continuous(labels = scales::percent) +
  #       labs(title = str_wrap("Sustainability Related Scholars by Year", 40),
  #            y = "Percent") +
  #       theme_minimal(base_size = 20)
  #   })
  
  output$stacked_bar3 <- renderPlot({
    
    usc_by_dept_sust_cat$one_sustainability_category <- factor(usc_by_dept_sust_cat$one_sustainability_category, levels = c("Sustainability-Focused", "Sustainability-Inclusive", "Not Related"))
    ggplot(usc_by_dept_sust_cat, aes(fill = one_sustainability_category, y = n, x = Year)) +
      geom_bar(position="fill", stat="identity") +
      scale_fill_manual(values = c("#990000", "#FFC72C", "#767676"), name = "") +
      scale_y_continuous(labels = scales::percent) +
      labs(title = str_wrap("Sustainability Related Departments/Centers/Institutes by Year", 40),
           y = "Percent") +
      theme_minimal(base_size = 20)
  })
  
  # tab 4
  output$pubs_to_bar <- renderPlotly(
    {
      validate(
        need(input$usc_division != "", label = "USC School/Unit")
      )
      
      d <- usc_joined %>%
        filter(Division == input$usc_division) %>%
        select(Department, starts_with("SDG")) %>%
        group_by(Department) %>%
        mutate(across(starts_with("SDG"), ~replace(., . != 0, 1))) %>%
        summarise(across(starts_with("SDG"), sum, na.rm = TRUE))
      df <- d %>% column_to_rownames("Department")
      colnames(df) <- 1:17
      df$category <- row.names(df)
      m <- melt(df, id.vars = "category")
      m$category <- trimws(gsub("(Dornsife|Viterbi|Marshall|KSOM)", "", m$category))
      m <- m %>% filter(category != "")
      p <- ggplot(m, aes(category, value, 
                         fill = variable, 
                         text = paste(category, "<br>has", value, "SDG", 
                                      variable))) +
        geom_bar(position = "stack", stat = "identity", width = 1) +
        # coord_flip() +
        scale_x_discrete(labels = NULL) +
        # scale_x_discrete(labels = label_wrap(20)) +
        scale_color_manual(values = sdg_colors,
                           aesthetics = c("fill")) +
        labs(
          fill = "SDG",
          x = "Departments/Centers/Institutes",
          y = "Count of Publications"
        ) +
        theme_minimal(base_size = 20)
      ggplotly(p, tooltip = "text") %>% layout(hoverlabel = list(font=list(size=18)))
    }
  )
  output$pubs_to_treemap <- renderPlot(
    {
      validate(
        need(input$usc_division != "", label = "USC School/Unit")
      )

      sdg_sum <- usc_joined %>% 
        filter(Division == input$usc_division) %>%
        mutate(across(starts_with("SDG"), ~replace(., . != 0, 1))) %>%
        summarise(across(starts_with("SDG"), sum, na.rm = TRUE)) %>%
        t %>%
        as.data.frame
      sdg_sum$Division <- input$usc_division
      sdg_sum$sdg <- 1:17
      
      ggplot(sdg_sum, aes(fill = as.factor(sdg), area = V1, label = paste0("SDG ", sdg, "\n(n = ", V1, ")"))) + 
        geom_treemap() + 
        scale_fill_manual(values = sdg_colors, aesthetics = "fill", name = "SDG") + 
        geom_treemap_text(place = "centre", size = 20, colour = "white") +
        theme(legend.position = "none")
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
        filter(!!sdg_col > 0) %>%
        select(Division) %>%
        as.list()
      updateSelectizeInput(session,
                           "Division",
                           server = TRUE,
                           choices = unique(sort(divisions[[1]])),
                           selected = "Dornsife College of Letters Arts and Sciences"
                           )
    }
  )

  output$top_authors_sdg_table <- renderPlot({
    validate(
      need(input$Primary.SDG != "", label = "SDG"),
      need(input$Division != "", label = "USC School/Unit")
    )
    sdg_col = get_selected_sdg_col(input$Primary.SDG)
    usc_joined %>%
      filter(Division == input$Division) %>%
      filter(!!sdg_col != 0) %>%
      select(pubID, Link, authorID, name) %>% distinct() %>%
      count(authorID, name) %>%
      arrange(desc(n)) %>%
      distinct(name, .keep_all = TRUE) %>%
      head(10) %>% #  num_top_classes <- 10
      ggplot(aes(x = reorder(as.factor(name),n), y = n)) +
      geom_col(fill = sdg_colors[as.numeric(input$Primary.SDG)], alpha = 1) +
      coord_flip() +
      labs(title = paste0("Top USC Scholars by SDG ", input$Primary.SDG),
           x = "Scholar",
           y = "Number of Research Products ") +
      theme_minimal(base_size = 20)
  })

  output$top_departments_sdg_table <- renderPlot({
    validate(
      need(input$Primary.SDG != "", label = "SDG"),
      need(input$Division != "", label = "USC School/Unit")
    )
    sdg_col = get_selected_sdg_col(input$Primary.SDG)
    usc_joined %>%
      filter(Division == input$Division) %>%
      filter(!!sdg_col != 0) %>%
      filter(Department != "") %>%
      count(Department) %>%
      arrange(desc(n)) %>%
      distinct(Department, .keep_all = TRUE) %>%
      head(10) %>%
      mutate(Department = gsub("(Dornsife|Viterbi|Marshall|KSOM)", "", Department)) %>%
      ggplot(aes(x = reorder(as.factor(Department),n), y = n)) +
      geom_col(fill = sdg_colors[as.numeric(input$Primary.SDG)], alpha = 1) +
      coord_flip() +
      scale_x_discrete(labels = label_wrap(40)) + # whole numbers 
      labs(title = paste0("Top Departments/Centers/Institutes by SDG ", input$Primary.SDG),
           x = "Departments/Centers/Institutes",
           y = "Number of Research Products ") +
      theme_minimal(base_size = 20)
  })

  # tab 6
  observeEvent(
    input$school, ignoreNULL = FALSE,
    {
      # if (input$school == "") {
      #   selected_authors = usc_authors
      # } else {
      #   selected_authors = usc_authors %>% filter(usc_authors$Division == input$school)
      # }
      selected_authors = usc_authors
      authorChoices = setNames(selected_authors$authorID,
                               selected_authors$fullname)
      updateSelectizeInput(session,
                           "author",
                           server = TRUE,
                           choices = authorChoices[sort(names(authorChoices))],
                           selected = character(0)
      )
    }
  )
  output$auth_about <- DT::renderDataTable(
    {
      validate(
        need(input$author != "", label = "USC Author")
      )
      usc_authors %>%
        # filter(usc_authors$InUSCDirectory & usc_authors$authorID == input$author) %>%
        filter(usc_authors$authorID == input$author) %>%
        select(firstname, lastname, Department, Division, Email, PositionTitle, Type)
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
        distinct(pubID, .keep_all = TRUE) %>%
        mutate(across(starts_with("SDG"), ~replace(., . != 0, 1))) %>%
        summarise(across(starts_with("SDG"), sum, na.rm = TRUE)) %>% 
        t %>% 
        as.data.frame() %>% 
        ggplot(aes(x = as.factor(1:17), y = V1, fill = factor(1:17))) + 
        geom_col() + 
        scale_color_manual(values = sdg_colors, aesthetics = "fill") +
        scale_y_continuous(breaks = scales::pretty_breaks()) +
        labs(title = names(input$author),
             x = "SDG",
             y = "Count",
             fill = "SDG") +
        theme_minimal(base_size = 20) +
        theme(legend.position = "none")
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
        select(Titles, url)
      sdgs_only <- usc_joined %>%
        filter(usc_joined$authorID == input$author) %>%
        select(starts_with("SDG")) %>%
        replace(is.na(.), 0)
      w <- which(sdgs_only != 0, arr.ind = TRUE)
      if (length(w) == 0) {
        pubs$SDGs <- ""
        pubs[, c("SDGs", "Titles", "url")] 
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
      pubs[, c("SDGs", "Titles", "url")] %>% distinct()
  }, rownames = FALSE, escape = FALSE)
  
  # tab 8
  output$dei_table <- DT::renderDataTable(
    {
      dei_joined$Titles <- paste0("<a href='", dei_joined$Link, "' target='_blank'>", dei_joined$Titles, "</a>")
      # first 50 words
      dei_joined$Abstract <- sapply(dei_joined$Abstract, function(x) {
        if (length(strsplit(x, " ")[[1]]) < 50) {
          x
        } else {
          paste0(paste(strsplit(x, " ")[[1]][1:50], collapse = " "), "...")
        }
      })
      # missing source
      dei_joined[, c("DEI_3.3_keywords", "sustainability_category", "SDGs", "Titles", "name", "Div", "Year", "Source.title", "Cited.by", "Abstract", "Open.Access")]
    }, rownames = FALSE, escape = FALSE, #extensions = 'Buttons', class = 'display',
    # sort by sustainability focused first
    # author before title
    options = list(
      columnDefs = list(list(width = '200px', targets = c(3)),
                        list(width = '100px', targets = c(0,1,10)),
                        list(width = '800px', targets = c(9))), # 0-indexed
      autoWidth = TRUE,
      scrollX = TRUE,
      columns = list(
        list(title = 'Assignment Earth 3.3 Keywords'),
        list(title = 'Sustainability Category'),
        NULL,
        list(title = 'Title'),
        list(title = 'Name'),
        list(title = 'Division'),
        NULL,
        list(title = 'Source'),
        list(title = 'Cited by'),
        NULL,
        list(title = 'Open Access')
      )
      # buttons = list('pageLength',
      #   list(extend = 'collection', 
      #        buttons = c('csv', 'excel', 'pdf'),
      #        text = 'Download')),
      # dom = 'Bfrtip'
    )
  )
  
  output$download_dei_data <- downloadHandler(
    filename = function() {"dei_data.csv"},
    content = function(fname){
      write.csv(dei_joined, fname, row.names = FALSE)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)